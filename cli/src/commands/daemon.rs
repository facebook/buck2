/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(unix)]
use std::os::unix::io::AsRawFd;
#[cfg(windows)]
use std::os::windows::io::AsRawHandle;
use std::{fs::File, io::Write, path::Path, process, time::Duration};

use anyhow::Context as _;
use buck2_common::memory;
use buck2_core::env_helper::EnvHelper;
use cli_proto::DaemonProcessInfo;
#[cfg(unix)]
use daemonize::Daemonize;
use dice::cycles::DetectCycles;
use futures::{
    channel::mpsc::{self, UnboundedSender},
    pin_mut, select, FutureExt, StreamExt,
};
use structopt::{clap, StructOpt};
use thiserror::Error;
use tokio::runtime::Builder;

use crate::{
    daemon::{
        daemon_utils::create_listener,
        server::{BuckdServer, BuckdServerDelegate},
    },
    paths::Paths,
    version::BuckVersion,
    CommandContext,
};

#[derive(Debug, Error)]
enum DaemonError {
    #[error("The buckd pid file had a mismatched pid, expected `{0}`, got `{1}`")]
    PidFileMismatch(u32, u32),
}

#[derive(Clone, Debug, StructOpt)]
#[structopt(about = "start buckd")]
pub struct DaemonCommand {
    #[structopt(
        help(
            "Sets the interval for how often the daemon performs consistency checks. These are used to ensure that the daemon is still the one referenced by files in the daemon dir."
        ),
        long,
        default_value("60")
    )]
    checker_interval_seconds: u64,
    #[structopt(help("Run buck daemon but do not daemonize the process"), long)]
    dont_daemonize: bool,
}

pub async fn run_buckd(
    fb: fbinit::FacebookInit,
    paths: Paths,
    detect_cycles: DetectCycles,
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

pub fn write_process_info(
    daemon_dir: &Path,
    process_info: &DaemonProcessInfo,
) -> anyhow::Result<()> {
    let file = File::create(daemon_dir.join("buckd.info"))?;
    serde_json::to_writer(&file, &process_info)?;
    Ok(())
}

fn verify_current_daemon(daemon_dir: &Path) -> anyhow::Result<()> {
    let my_pid = process::id();

    let recorded_pid: u32 = std::fs::read_to_string(daemon_dir.join("buckd.pid"))?
        .trim()
        .parse()?;
    if recorded_pid != my_pid {
        return Err(DaemonError::PidFileMismatch(my_pid, recorded_pid).into());
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
        detect_cycles: DetectCycles,
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

    pub fn exec(self, _matches: &clap::ArgMatches, ctx: CommandContext) -> anyhow::Result<()> {
        let project_root = ctx.paths()?.project_root();
        let daemon_dir = ctx.paths()?.daemon_dir()?;
        let stdout_path = daemon_dir.join("buckd.stdout");
        let stderr_path = daemon_dir.join("buckd.stderr");
        let pid_path = daemon_dir.join("buckd.pid");
        let stdout = File::create(stdout_path)?;
        let stderr = File::create(stderr_path)?;

        if self.dont_daemonize {
            let mut pid_file = File::create(pid_path)?;
            write!(pid_file, "{}", std::process::id())?;
            std::env::set_current_dir(project_root)?;
            self.redirect_output(stdout, stderr)?;
        } else {
            #[cfg(unix)]
            {
                // TODO(cjhopman): Daemonize is pretty un-maintained. We may need to move
                // to something else or just do it ourselves.
                let daemonize = Daemonize::new()
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
            }
            #[cfg(windows)]
            {
                use std::os::windows::process::CommandExt;
                // Restart current process in detached mode with '--dont-daemonize' flag.
                let mut cmd = std::process::Command::new(
                    std::env::current_exe().expect("somehow couldn't get current exe"),
                );
                cmd.creation_flags(winapi::um::winbase::DETACHED_PROCESS);
                cmd.args(std::env::args().skip(1));
                cmd.arg("--dont-daemonize");
                cmd.spawn()?;
                return Ok(());
            }
        }

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
}
