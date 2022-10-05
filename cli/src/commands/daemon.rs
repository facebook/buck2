/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process;
use std::thread;
use std::time::Duration;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_audit::server::server_audit_command;
use buck2_build_api::bxl::calculation::BxlCalculationDyn;
use buck2_bxl::bxl::calculation::BxlCalculationImpl;
use buck2_bxl::bxl::starlark_defs::configure_bxl_file_globals;
use buck2_bxl::command::bxl_command;
use buck2_client::version::BuckVersion;
use buck2_common::daemon_dir::DaemonDir;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::memory;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::FileName;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_server::daemon::daemon_utils::create_listener;
use buck2_server::daemon::server::BuckdServer;
use buck2_server::daemon::server::BuckdServerDelegate;
use buck2_server::daemon::server::BuckdServerDependencies;
use buck2_server::daemon::tcp_or_unix_stream::TcpOrUnixStream;
use buck2_server::docs::docs_command;
use buck2_server::profile::profile_command;
use buck2_server_commands::commands::build::build_command;
use buck2_server_commands::commands::install::install_command;
use buck2_server_commands::commands::query::aquery::aquery_command;
use buck2_server_commands::commands::query::cquery::cquery_command;
use buck2_server_commands::commands::query::uquery::uquery_command;
use buck2_server_commands::commands::targets::targets_command;
use buck2_server_commands::commands::targets_show_outputs::targets_show_outputs_command;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_test::command::test_command;
use cli_proto::DaemonProcessInfo;
use dice::cycles::DetectCycles;
use futures::channel::mpsc;
use futures::channel::mpsc::UnboundedSender;
use futures::pin_mut;
use futures::select;
use futures::stream::BoxStream;
use futures::FutureExt;
use futures::StreamExt;
use starlark::environment::GlobalsBuilder;
use thiserror::Error;
use tokio::runtime::Builder;

#[derive(Debug, Error)]
enum DaemonError {
    #[error("The buckd pid file at `{0}` had a mismatched pid, expected `{1}`, got `{2}`")]
    PidFileMismatch(PathBuf, u32, u32),
}

/// Start or run buck daemon.
///
/// This is an internal command, not intended to be used directly.
/// Buck client invokes it to spawn a server process.
#[derive(Clone, Debug, clap::Parser)]
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
    /// Do not redirect stdout/stderr to a file.
    /// This is used for in-process server, because if we redirect,
    /// server buck2 process (acting as client) won't be able to print anything.
    ///
    /// This shouldn't be an option.
    #[clap(hidden = true, long)]
    dont_redirect_output: bool,
}

impl DaemonCommand {
    /// Command instance for `--no-buckd`.
    pub(crate) fn new_in_process() -> DaemonCommand {
        DaemonCommand {
            checker_interval_seconds: 60,
            dont_daemonize: true,
            dont_redirect_output: true,
        }
    }
}

struct BuckdServerDependenciesImpl;

#[async_trait]
impl BuckdServerDependencies for BuckdServerDependenciesImpl {
    async fn test(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::TestRequest,
    ) -> anyhow::Result<cli_proto::TestResponse> {
        test_command(ctx, req).await
    }
    async fn build(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::BuildRequest,
    ) -> anyhow::Result<cli_proto::BuildResponse> {
        build_command(ctx, req).await
    }
    async fn install(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::InstallRequest,
    ) -> anyhow::Result<cli_proto::InstallResponse> {
        install_command(ctx, req).await
    }
    async fn bxl(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::BxlRequest,
    ) -> anyhow::Result<cli_proto::BxlResponse> {
        bxl_command(ctx, req).await
    }
    async fn audit(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::GenericRequest,
    ) -> anyhow::Result<cli_proto::GenericResponse> {
        server_audit_command(ctx, req).await
    }
    async fn profile(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::ProfileRequest,
    ) -> anyhow::Result<cli_proto::ProfileResponse> {
        profile_command(ctx, req).await
    }
    async fn uquery(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::UqueryRequest,
    ) -> anyhow::Result<cli_proto::UqueryResponse> {
        uquery_command(ctx, req).await
    }
    async fn cquery(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::CqueryRequest,
    ) -> anyhow::Result<cli_proto::CqueryResponse> {
        cquery_command(ctx, req).await
    }
    async fn aquery(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::AqueryRequest,
    ) -> anyhow::Result<cli_proto::AqueryResponse> {
        aquery_command(ctx, req).await
    }
    async fn targets(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::TargetsRequest,
    ) -> anyhow::Result<cli_proto::TargetsResponse> {
        targets_command(ctx, req).await
    }
    async fn targets_show_outputs(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::TargetsRequest,
    ) -> anyhow::Result<cli_proto::TargetsShowOutputsResponse> {
        targets_show_outputs_command(ctx, req).await
    }
    async fn docs(
        &self,
        ctx: Box<dyn ServerCommandContextTrait>,
        req: cli_proto::UnstableDocsRequest,
    ) -> anyhow::Result<cli_proto::UnstableDocsResponse> {
        docs_command(ctx, req).await
    }
    fn bxl_calculation(&self) -> &'static dyn BxlCalculationDyn {
        &BxlCalculationImpl
    }
    fn configure_bxl_file_globals(&self) -> fn(&mut GlobalsBuilder) {
        configure_bxl_file_globals
    }
}

pub(crate) async fn init_listener(
    daemon_dir: &DaemonDir,
) -> anyhow::Result<(
    BoxStream<'static, Result<TcpOrUnixStream, std::io::Error>>,
    DaemonProcessInfo,
)> {
    let (endpoint, listener) = create_listener(daemon_dir.path.to_path_buf()).await?;

    buck2_client::eprintln!("starting daemon on {}", &endpoint)?;
    let pid = process::id();
    let process_info = DaemonProcessInfo {
        pid: pid as i64,
        endpoint,
        version: BuckVersion::get().unique_id().to_owned(),
    };

    // TODO(cjhopman): We shouldn't write this until the server is ready to accept clients, but tonic doesn't provide those hooks.
    write_process_info(daemon_dir, &process_info)?;
    Ok((listener, process_info))
}

pub(crate) fn write_process_info(
    daemon_dir: &DaemonDir,
    process_info: &DaemonProcessInfo,
) -> anyhow::Result<()> {
    let file = File::create(daemon_dir.buckd_info()?)?;
    serde_json::to_writer(&file, &process_info)?;
    Ok(())
}

fn verify_current_daemon(daemon_dir: &DaemonDir) -> anyhow::Result<()> {
    let file = daemon_dir.path.join(FileName::new("buckd.pid")?);
    let my_pid = process::id();

    let recorded_pid: u32 = fs_util::read_to_string(&file)?.trim().parse()?;
    if recorded_pid != my_pid {
        return Err(
            DaemonError::PidFileMismatch(file.into_path_buf(), my_pid, recorded_pid).into(),
        );
    }

    Ok(())
}

/// Our tests sometimes don't exit Buck 2 cleanly, and they might not get an oppportunity to do so
/// if they are terminated. This allows the daemon to self-destruct.
fn maybe_schedule_termination() -> anyhow::Result<()> {
    static TERMINATE_AFTER: EnvHelper<u64> = EnvHelper::new("BUCK2_TERMINATE_AFTER");

    if let Some(duration) = TERMINATE_AFTER.get_copied()? {
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
        paths: InvocationPaths,
        detect_cycles: Option<DetectCycles>,
        listener_created: impl FnOnce() + Send,
    ) -> anyhow::Result<()> {
        // Higher performance for jemalloc, recommended (but may not have any effect on Mac)
        // https://github.com/jemalloc/jemalloc/blob/dev/TUNING.md#notable-runtime-options-for-performance-tuning
        memory::enable_background_threads()?;

        if cfg!(target_os = "linux") {
            #[cfg(fbcode_build)]
            {
                gflags::set_gflag_value(
                    fb,
                    "cgroup2_reader_update_interval_ms",
                    gflags::GflagValue::U32(2000),
                )
                .expect("failed to set gflag --cgroup2_reader_update_interval_ms");
            }
        }

        let mut builder = Builder::new_multi_thread();
        builder.enable_all();
        builder.thread_name("buck2-rt");

        static RUNTIME_THREADS: EnvHelper<usize> = EnvHelper::new("BUCK2_RUNTIME_THREADS");
        if let Some(threads) = RUNTIME_THREADS.get_copied()? {
            builder.worker_threads(threads);
        }

        static MAX_BLOCKING_THREADS: EnvHelper<usize> =
            EnvHelper::new("BUCK2_MAX_BLOCKING_THREADS");
        if let Some(threads) = MAX_BLOCKING_THREADS.get_copied()? {
            builder.max_blocking_threads(threads);
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

            let (listener, process_info) = init_listener(&daemon_dir).await?;

            listener_created();

            let buckd_server = BuckdServer::run(
                fb,
                paths,
                delegate,
                detect_cycles,
                process_info,
                listener,
                &BuckdServerDependenciesImpl,
            )
            .fuse();
            let shutdown_future = async move { hard_shutdown_receiver.next().await }.fuse();
            pin_mut!(buckd_server);
            pin_mut!(shutdown_future);

            let checker_interval_seconds = self.checker_interval_seconds;

            thread::Builder::new()
                .name("check-daemon-dir".to_owned())
                .spawn(move || {
                    Self::check_daemon_dir_thread(
                        checker_interval_seconds,
                        daemon_dir,
                        hard_shutdown_sender,
                    )
                })?;

            // clippy doesn't get along well with the select!
            #[allow(clippy::mut_mut)]
            {
                select! {
                    _ = buckd_server => {
                        buck2_client::eprintln!("server shutdown gracefully")?;
                    }
                    _ = shutdown_future => {
                        buck2_client::eprintln!("server forced shutdown")?;
                    },
                };
            }

            anyhow::Ok(())
        })?;
        Ok(())
    }

    /// We start a dedicated thread to periodically check that the files in the daemon
    /// dir still reflect that we are the current buckd and verify that when you connect
    /// to the server it is our server.
    /// It gets a dedicated thread so that if somehow the main runtime gets all jammed up,
    /// this will still run (and presumably connecting to the server or our request would
    /// then fail and we'd do a hard shutdown).
    fn check_daemon_dir_thread(
        checker_interval_seconds: u64,
        daemon_dir: DaemonDir,
        hard_shutdown_sender: UnboundedSender<()>,
    ) {
        let this_rt = Builder::new_current_thread().enable_all().build().unwrap();

        this_rt.block_on(async move {
            loop {
                tokio::time::sleep(Duration::from_secs(checker_interval_seconds)).await;
                match verify_current_daemon(&daemon_dir) {
                    Ok(()) => {}
                    Err(e) => {
                        // This bit of code cannot relay errors, ignoring that we can't log
                        // a warning is reasonable.
                        let _ignored = buck2_client::eprintln!(
                            "daemon verification failed, forcing shutdown: {:#}",
                            e
                        );
                        hard_shutdown_sender.unbounded_send(()).unwrap();
                    }
                };
            }
        })
    }

    pub(crate) fn exec(
        &self,
        init: fbinit::FacebookInit,
        paths: InvocationPaths,
        detect_cycles: Option<DetectCycles>,
        listener_created: impl FnOnce() + Send,
    ) -> anyhow::Result<()> {
        let project_root = paths.project_root();
        let daemon_dir = paths.daemon_dir()?;
        let stdout_path = daemon_dir.buckd_stdout()?;
        let stderr_path = daemon_dir.buckd_stderr()?;
        let pid_path = daemon_dir.path.join(ForwardRelativePath::new("buckd.pid")?);

        if !daemon_dir.path.is_dir() {
            fs_util::create_dir_all(&daemon_dir.path)?;
        }

        // TODO(nga): this breaks relative paths in `--no-buckd`.
        //   `--no-buckd` should capture correct directories earlier.
        //   Or even better, client should set current directory to project root,
        //   and resolve all paths relative to original cwd.
        env::set_current_dir(project_root.root())?;

        let stdout = File::create(stdout_path)?;
        let stderr = File::create(stderr_path)?;

        if self.dont_daemonize {
            let mut pid_file = File::create(pid_path)?;
            write!(pid_file, "{}", std::process::id())?;
            if !self.dont_redirect_output {
                self.redirect_output(stdout, stderr)?;
            }
        } else {
            self.daemonize(project_root.root(), &pid_path, stdout, stderr)?;
        }
        gazebo::terminate_on_panic();

        maybe_schedule_termination()?;

        self.run(init, paths, detect_cycles, listener_created)?;
        Ok(())
    }

    #[cfg(unix)]
    fn redirect_output(&self, stdout: File, stderr: File) -> anyhow::Result<()> {
        use std::os::unix::io::AsRawFd;

        nix::unistd::dup2(stdout.as_raw_fd(), nix::libc::STDOUT_FILENO)?;
        nix::unistd::dup2(stderr.as_raw_fd(), nix::libc::STDERR_FILENO)?;
        Ok(())
    }

    #[cfg(windows)]
    fn redirect_output(&self, stdout: File, stderr: File) -> anyhow::Result<()> {
        use std::os::windows::io::AsRawHandle;

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
        _project_root: &AbsPath,
        _pid_path: &AbsPath,
        _stdout: File,
        _stderr: File,
    ) -> anyhow::Result<()> {
        Err(anyhow::anyhow!("Cannot daemonize on Windows"))
    }
}
