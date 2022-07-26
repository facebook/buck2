/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(

use std::fs::File;
use std::io::Write;
#[cfg(unix)]
use std::os::unix::io::AsRawFd;
#[cfg(windows)]
use std::os::windows::io::AsRawHandle;
use std::path::Path;
use std::path::PathBuf;
use std::process;
use std::time::Duration;

use anyhow::Context as _;
use buck2_common::memory;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::anyhow as fs;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::ForwardRelativePath;
use cli_proto::DaemonProcessInfo;
use dice::cycles::DetectCycles;
use futures::channel::mpsc;
use futures::channel::mpsc::UnboundedSender;
use futures::pin_mut;
use futures::select;
use futures::FutureExt;
use futures::StreamExt;
use thiserror::Error;
use tokio::runtime::Builder;

use crate::daemon::daemon_utils::create_listener;
use crate::daemon::server::BuckdServer;
use crate::daemon::server::BuckdServerDelegate;
use crate::paths::Paths;
use crate::version::BuckVersion;
use crate::CommandContext;

#[derive(Debug, Error)]
enum DaemonError {
    #[error("The buckd pid file at `{0}` had a mismatched pid, expected `{1}`, got `{2}`")]
    PidFileMismatch(PathBuf, u32, u32),
}

#[derive(Clone, Debug, clap::Parser)]
#[clap(about = "start buckd")]
pub(crate) struct DaemonCommand {
    #[clap(
        help(
            "Sets the interval for how often the daemon performs consistency checks. These are used to ensure that the daemon is still the one referenced by files in the daemon dir."
        ),
        long,
        default_value("60")
    )]
    checker_interval_seconds: u64,
    #[clap(help("Run buck daemon but do not daemonize the process"), long)]
    dont_daemonize: bool,
}

pub(crate) async fn run_buckd(
    fb: fbinit::FacebookInit,
    paths: Paths,
    detect_cycles: Option<DetectCycles>,
    delegate: Box<dyn BuckdServerDelegate>,
) -> anyhow::Result<()> {
    let daemon_dir = paths.daemon_dir()?;
    let (endpoint, listener) = create_listener(&daemon_dir).await?;

    crate::eprintln!("starting daemon on {}", &endpoint)?;
    let pid = process::id();
    let process_info = DaemonProcessInfo {
        pid: pid as i64,
        endpoint,
        version: BuckVersion::get().unique_id().to_owned(),
    };

    // TODO(cjhopman): We shouldn't write this until the server is ready to accept clients, but tonic doesn't provide those hooks.
    write_process_info(&daemon_dir, &process_info)?;
    BuckdServer::run(fb, paths, delegate, detect_cycles, process_info, listener).await
}

pub(crate) fn write_process_info(
    daemon_dir: &Path,
    process_info: &DaemonProcessInfo,
) -> anyhow::Result<()> {
    let file = File::create(daemon_dir.join("buckd.info"))?;
    serde_json::to_writer(&file, &process_info)?;
    Ok(())
}

fn verify_current_daemon(daemon_dir: &Path) -> anyhow::Result<()> {
    let file = daemon_dir.join("buckd.pid");
    let my_pid = process::id();

    let recorded_pid: u32 = fs::read_to_string(&file)?.trim().parse()?;
    if recorded_pid != my_pid {
        return Err(DaemonError::PidFileMismatch(file, my_pid, recorded_pid).into());
    }

    Ok(())
}

/// Our tests sometimes don't exit Buck 2 cleanly, and they might not get an oppportunity to do so
/// if they are terminated. This allows the daemon to self-destruct.
fn maybe_schedule_termination() -> anyhow::Result<()> {
    static TERMINATE_AFTER: EnvHelper<u64> = EnvHelper::new("BUCK2_TERMINATE_AFTER");

    if let Some(duration) = *TERMINATE_AFTER.get()? {
        std::thread::spawn(move || {
            std::thread::sleep(Duration::from_secs(duration));
            panic!("Buck is exiting after {} elapsed", duration);
        });
    }

    Ok(())
}

impl DaemonCommand {
    fn run(
        &self,
        fb: fbinit::FacebookInit,
        paths: Paths,
        detect_cycles: Option<DetectCycles>,
    ) -> anyhow::Result<()> {
        // Higher performance for jemalloc, recommended (but may not have any effect on Mac)
        // https://github.com/jemalloc/jemalloc/blob/dev/TUNING.md#notable-runtime-options-for-performance-tuning
        memory::enable_background_threads()?;

        #[cfg(all(fbcode_build, target_os = "linux"))]
        {
            gflags::set_gflag_value(
                fb,
                "cgroup2_reader_update_interval_ms",
                gflags::GflagValue::U32(2000),
            )
            .expect("failed to set gflag --cgroup2_reader_update_interval_ms");
        }

        let mut builder = Builder::new_multi_thread();
        builder.enable_all();
        builder.thread_name("buck2-rt");

        static RUNTIME_THREADS: EnvHelper<usize> = EnvHelper::new("BUCK2_RUNTIME_THREADS");
        if let Some(threads) = RUNTIME_THREADS.get()? {
            builder.worker_threads(*threads);
        }

        static MAX_BLOCKING_THREADS: EnvHelper<usize> =
            EnvHelper::new("BUCK2_MAX_BLOCKING_THREADS");
        if let Some(threads) = MAX_BLOCKING_THREADS.get()? {
            builder.max_blocking_threads(*threads);
        }

        let rt = builder.build().context("Error creating Tokio runtime")?;

        rt.block_on(async move {
            // Once any item is received on the hard_shutdown_receiver, the daemon process will exit immediately.
            let (hard_shutdown_sender, mut hard_shutdown_receiver): (UnboundedSender<()>, _) =
                mpsc::unbounded();

            struct Delegate {
                hard_shutdown_sender: UnboundedSender<()>,
            }

            impl BuckdServerDelegate for Delegate {
                fn force_shutdown(&self) -> anyhow::Result<()> {
                    self.hard_shutdown_sender.unbounded_send(())?;
                    Ok(())
                }

                fn force_shutdown_with_timeout(&self, timeout: Duration) {
                    let sender = self.hard_shutdown_sender.clone();
                    tokio::spawn(async move {
                        tokio::time::sleep(timeout).await;
                        sender.unbounded_send(()).expect("Shouldn't happen.");
                    });
                }
            }

            let delegate = box Delegate {
                hard_shutdown_sender: hard_shutdown_sender.clone(),
            };
            let daemon_dir = paths.daemon_dir()?;

            let buckd_server = run_buckd(fb, paths, detect_cycles, delegate).fuse();
            let shutdown_future = async move { hard_shutdown_receiver.next().await }.fuse();
            pin_mut!(buckd_server);
            pin_mut!(shutdown_future);

            let checker_interval_seconds = self.checker_interval_seconds;

            // We start a dedicated thread to periodically check that the files in the daemon
            // dir still reflect that we are the current buckd and verify that when you connect
            // to the server it is our server.
            // It gets a dedicated thread so that if somehow the main runtime gets all jammed up,
            // this will still run (and presumably connecting to the server or our request would
            // then fail and we'd do a hard shutdown).
            std::thread::spawn(move || {
                let this_rt = Builder::new_current_thread().enable_all().build().unwrap();

                this_rt.block_on(async move {
                    loop {
                        tokio::time::sleep(Duration::from_secs(checker_interval_seconds)).await;
                        match verify_current_daemon(&daemon_dir) {
                            Ok(()) => {}
                            Err(e) => {
                                // This bit of code cannot relay errors, ignoring that we can't log
                                // a warning is reasonable.
                                let _ignored = crate::eprintln!(
                                    "daemon verification failed, forcing shutdown: {:#}",
                                    e
                                );
                                hard_shutdown_sender.unbounded_send(()).unwrap();
                            }
                        };
                    }
                })
            });

            // clippy doesn't get along well with the select!
            #[allow(clippy::mut_mut)]
            {
                select! {
                    _ = buckd_server => {
                        crate::eprintln!("server shutdown gracefully")?;
                    }
                    _ = shutdown_future => {
                        crate::eprintln!("server forced shutdown")?;
                    },
                };
            }

            anyhow::Ok(())
        })?;
        Ok(())
    }

    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> anyhow::Result<()> {
        let project_root = ctx.paths()?.project_root();
        let daemon_dir = ctx.paths()?.daemon_dir()?;
        let stdout_path = daemon_dir.join_unnormalized(ForwardRelativePath::new("buckd.stdout")?);
        let stderr_path = daemon_dir.join_unnormalized(ForwardRelativePath::new("buckd.stderr")?);
        let pid_path = daemon_dir.join_unnormalized(ForwardRelativePath::new("buckd.pid")?);

        if !daemon_dir.is_dir() {
            fs::create_dir_all(daemon_dir)?;
        }
        let stdout = File::create(stdout_path)?;
        let stderr = File::create(stderr_path)?;

        if self.dont_daemonize {
            let mut pid_file = File::create(pid_path)?;
            write!(pid_file, "{}", std::process::id())?;
            std::env::set_current_dir(project_root)?;
            self.redirect_output(stdout, stderr)?;
        } else {
            self.daemonize(project_root, &pid_path, stdout, stderr)?;
            // On Windows process will be restarted.
            if cfg!(windows) {
                return Ok(());
            }
        }
        gazebo::terminate_on_panic();

        maybe_schedule_termination()?;

        self.run(ctx.fbinit(), ctx.paths?, ctx.detect_cycles)?;
        Ok(())
    }

    #[cfg(unix)]
    fn redirect_output(&self, stdout: File, stderr: File) -> anyhow::Result<()> {
        nix::unistd::dup2(stdout.as_raw_fd(), nix::libc::STDOUT_FILENO)?;
        nix::unistd::dup2(stderr.as_raw_fd(), nix::libc::STDERR_FILENO)?;
        Ok(())
    }

    #[cfg(windows)]
    fn redirect_output(&self, stdout: File, stderr: File) -> anyhow::Result<()> {
        unsafe {
            let stdout_fd = libc::open_osfhandle(stdout.as_raw_handle() as isize, libc::O_RDWR);
            let stderr_fd = libc::open_osfhandle(stderr.as_raw_handle() as isize, libc::O_RDWR);
            if stdout_fd == -1 || stderr_fd == -1 {
                return Err(anyhow::Error::msg(
                    "Can't get file descriptors for output files",
                ));
            }
            // MSVC libc doesn't export STDOUT_FILENO and STDERR_FILENO.
            let stdout_exit_code = libc::dup2(stdout_fd, 1);
            let stderr_exit_code = libc::dup2(stderr_fd, 2);
            if stdout_exit_code == -1 || stderr_exit_code == -1 {
                return Err(anyhow::Error::msg("Failed to redirect daemon output"));
            }
        }
        Ok(())
    }

    #[cfg(unix)]
    fn daemonize(
        &self,
        project_root: &AbsPath,
        pid_path: &AbsPath,
        stdout: File,
        stderr: File,
    ) -> anyhow::Result<()> {
        // TODO(cjhopman): Daemonize is pretty un-maintained. We may need to move
        // to something else or just do it ourselves.
        let daemonize = daemonize::Daemonize::new()
            .pid_file(pid_path)
            .chown_pid_file(true)
            .working_directory(project_root)
            // This umask corresponds to a default of `rwxr-xr-x` (which is the default on Linux).
            // We should probably just leave this alone, but the Daemonize crate doesn't let us do
            // that.
            .umask(0o022)
            .stdout(stdout)
            .stderr(stderr);
        daemonize.start()?;
        Ok(())
    }

    #[cfg(windows)]
    /// Restart current process in detached mode with '--dont-daemonize' flag.
    fn daemonize(
        &self,
        project_root: &AbsPath,
        _pid_path: &AbsPath,
        _stdout: File,
        _stderr: File,
    ) -> anyhow::Result<()> {
        // We have to call CreateProcessW manually because std::process::Command
        // doesn't allow to set 'bInheritHandles' to false. Without this waiting on
        // parent process will also wait on inherited handles of daemon process.
        use std::env;
        use std::ffi::OsStr;
        use std::ffi::OsString;
        use std::iter;
        use std::mem;
        use std::os::windows::ffi::OsStrExt;
        use std::ptr;

        use winapi::shared::minwindef::DWORD;
        use winapi::shared::minwindef::FALSE;
        use winapi::um::handleapi::CloseHandle;
        use winapi::um::processthreadsapi::CreateProcessW;
        use winapi::um::processthreadsapi::PROCESS_INFORMATION;
        use winapi::um::processthreadsapi::STARTUPINFOW;
        use winapi::um::winbase::CREATE_NEW_PROCESS_GROUP;
        use winapi::um::winbase::CREATE_UNICODE_ENVIRONMENT;
        use winapi::um::winbase::DETACHED_PROCESS;

        fn to_nullterm(s: &OsStr) -> Vec<u16> {
            s.encode_wide().chain(iter::once(0)).collect()
        }

        // Translated from ArgvQuote at http://tinyurl.com/zmgtnls
        fn append_quoted(arg: &OsStr, cmdline: &mut Vec<u16>) {
            if !arg.is_empty()
                && !arg.encode_wide().any(|c| {
                    c == ' ' as u16
                        || c == '\t' as u16
                        || c == '\n' as u16
                        || c == '\x0b' as u16
                        || c == '\"' as u16
                })
            {
                cmdline.extend(arg.encode_wide());
                return;
            }
            cmdline.push('"' as u16);

            let arg: Vec<_> = arg.encode_wide().collect();
            let mut i = 0;
            while i < arg.len() {
                let mut num_backslashes = 0;
                while i < arg.len() && arg[i] == '\\' as u16 {
                    i += 1;
                    num_backslashes += 1;
                }

                if i == arg.len() {
                    for _ in 0..num_backslashes * 2 {
                        cmdline.push('\\' as u16);
                    }
                    break;
                } else if arg[i] == b'"' as u16 {
                    for _ in 0..num_backslashes * 2 + 1 {
                        cmdline.push('\\' as u16);
                    }
                    cmdline.push(arg[i]);
                } else {
                    for _ in 0..num_backslashes {
                        cmdline.push('\\' as u16);
                    }
                    cmdline.push(arg[i]);
                }
                i += 1;
            }
            cmdline.push('"' as u16);
        }

        #[allow(clippy::vec_init_then_push)]
        fn make_command_line(program: &OsStr, args: impl Iterator<Item = String>) -> Vec<u16> {
            let mut cmd: Vec<u16> = Vec::new();
            cmd.push(b'"' as u16);
            cmd.extend(program.encode_wide());
            cmd.push(b'"' as u16);
            for arg in args.map(OsString::from) {
                cmd.push(b' ' as u16);
                append_quoted(&arg, &mut cmd);
            }
            cmd.push(0); // null terminator
            cmd
        }

        let exe_path = env::current_exe().expect("somehow couldn't get current exe");
        let program = exe_path.as_os_str();
        let cwd = project_root.as_os_str();

        let mut cmd = make_command_line(
            program,
            env::args()
                .skip(1)
                .chain(iter::once("--dont-daemonize".to_owned())),
        );

        let mut sinfo: STARTUPINFOW = unsafe { mem::zeroed() };
        sinfo.cb = mem::size_of::<STARTUPINFOW>() as DWORD;
        let mut pinfo: PROCESS_INFORMATION = unsafe { mem::zeroed() };
        let creation_flags =
            CREATE_NEW_PROCESS_GROUP | DETACHED_PROCESS | CREATE_UNICODE_ENVIRONMENT;

        let status = unsafe {
            CreateProcessW(
                to_nullterm(program).as_ptr(), // lpApplicationName
                cmd.as_mut_ptr(),              // lpCommandLine
                ptr::null_mut(),               // lpProcessAttributes
                ptr::null_mut(),               // lpThreadAttributes
                FALSE,                         // bInheritHandles
                creation_flags,                // dwCreationFlags
                ptr::null_mut(),               // lpEnvironment
                to_nullterm(cwd).as_ptr(),     // lpCurrentDirectory
                &mut sinfo,
                &mut pinfo,
            )
        };
        if status == 0 {
            return Err(anyhow::anyhow!(std::io::Error::last_os_error()));
        }
        unsafe { CloseHandle(pinfo.hThread) };
        unsafe { CloseHandle(pinfo.hProcess) };
        Ok(())
    }
}
