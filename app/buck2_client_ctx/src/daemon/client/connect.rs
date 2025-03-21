/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::BufReader;
use std::net::Ipv4Addr;
use std::path::PathBuf;
use std::time::Duration;

use buck2_cli_proto::daemon_api_client::DaemonApiClient;
use buck2_cli_proto::DaemonProcessInfo;
use buck2_common::buckd_connection::ConnectionType;
use buck2_common::buckd_connection::BUCK_AUTH_TOKEN_HEADER;
use buck2_common::client_utils::get_channel_tcp;
use buck2_common::client_utils::get_channel_uds;
use buck2_common::client_utils::retrying;
use buck2_common::client_utils::RetryError;
use buck2_common::daemon_dir::DaemonDir;
use buck2_common::init::DaemonStartupConfig;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::systemd::replace_unit_delimiter;
use buck2_common::systemd::ParentSlice;
use buck2_common::systemd::SystemdRunner;
use buck2_common::systemd::SystemdRunnerConfig;
use buck2_core::buck2_env;
use buck2_data::DaemonWasStartedReason;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::source_location::SourceLocation;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_util::process::async_background_command;
use buck2_util::truncate::truncate;
use buck2_wrapper_common::kill::process_exists;
use buck2_wrapper_common::pid::Pid;
use dupe::Dupe;
use futures::future::try_join3;
use futures::FutureExt;
use serde::Deserialize;
use tokio::io::AsyncReadExt;
use tokio::time::timeout;
use tonic::codegen::InterceptedService;
use tonic::metadata::AsciiMetadataValue;
use tonic::service::Interceptor;
use tonic::transport::Channel;
use tonic::Request;
use tonic::Status;

use crate::command_outcome::CommandOutcome;
use crate::daemon::client::kill;
use crate::daemon::client::kill::hard_kill_until;
use crate::daemon::client::BuckdClient;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::BuckdLifecycleLock;
use crate::daemon::daemon_windows::spawn_background_process_on_windows;
use crate::daemon_constraints;
use crate::daemon_constraints::get_possibly_nested_invocation_daemon_uuid;
use crate::events_ctx::EventsCtx;
use crate::immediate_config::ImmediateConfigContext;
use crate::startup_deadline::StartupDeadline;
use crate::subscribers::classify_server_stderr::classify_server_stderr;
use crate::subscribers::stdout_stderr_forwarder::StdoutStderrForwarder;
use crate::subscribers::subscribers::EventSubscribers;

/// The client side matcher for DaemonConstraints.
#[derive(Clone, Debug)]
pub struct DaemonConstraintsRequest {
    /// The version of buck2.
    version: String,
    /// Sandcastle id.
    user_version: Option<String>,
    desired_trace_io_state: DesiredTraceIoState,
    nested_invocation_daemon_uuid: Option<String>,
    pub reject_daemon: Option<String>,
    pub reject_materializer_state: Option<String>,
    pub daemon_startup_config: DaemonStartupConfig,
}

#[derive(Debug, derive_more::Display)]
pub(crate) enum ConstraintUnsatisfiedReason {
    #[display("Version mismatch")]
    Version,
    #[display("User version mismatch")]
    UserVersion,
    #[display("Startup config mismatch")]
    StartupConfig,
    #[display("Reject daemon id")]
    RejectDaemonId,
    #[display("Trace IO mismatch")]
    TraceIo,
    #[display("Materializer state identity mismatch")]
    MaterializerStateIdentity,
}

impl ConstraintUnsatisfiedReason {
    pub(crate) fn to_daemon_was_started_reason(&self) -> buck2_data::DaemonWasStartedReason {
        match self {
            ConstraintUnsatisfiedReason::Version => {
                buck2_data::DaemonWasStartedReason::ConstraintMismatchVersion
            }
            ConstraintUnsatisfiedReason::UserVersion => {
                buck2_data::DaemonWasStartedReason::ConstraintMismatchUserVersion
            }
            ConstraintUnsatisfiedReason::StartupConfig => {
                buck2_data::DaemonWasStartedReason::ConstraintMismatchStartupConfig
            }
            ConstraintUnsatisfiedReason::RejectDaemonId => {
                buck2_data::DaemonWasStartedReason::ConstraintRejectDaemonId
            }
            ConstraintUnsatisfiedReason::TraceIo => {
                buck2_data::DaemonWasStartedReason::ConstraintMismatchTraceIo
            }
            ConstraintUnsatisfiedReason::MaterializerStateIdentity => {
                buck2_data::DaemonWasStartedReason::ConstraintMismatchMaterializerStateIdentity
            }
        }
    }
}

impl DaemonConstraintsRequest {
    pub fn new(
        immediate_config: &ImmediateConfigContext<'_>,
        desired_trace_io_state: DesiredTraceIoState,
    ) -> buck2_error::Result<Self> {
        Ok(Self {
            version: daemon_constraints::version(),
            user_version: daemon_constraints::user_version()?,
            desired_trace_io_state,
            nested_invocation_daemon_uuid: get_possibly_nested_invocation_daemon_uuid(),
            reject_daemon: None,
            reject_materializer_state: None,
            daemon_startup_config: immediate_config.daemon_startup_config()?.clone(),
        })
    }

    fn is_trace_io_requested(&self) -> bool {
        matches!(self.desired_trace_io_state, DesiredTraceIoState::Enabled)
    }

    fn satisfied(
        &self,
        daemon: &buck2_cli_proto::DaemonConstraints,
    ) -> Result<(), ConstraintUnsatisfiedReason> {
        if self.version != daemon.version {
            return Err(ConstraintUnsatisfiedReason::Version);
        }

        if !is_nested_invocation(self.nested_invocation_daemon_uuid.as_ref(), daemon)
            && self.user_version != daemon.user_version
        {
            return Err(ConstraintUnsatisfiedReason::UserVersion);
        }

        let server_daemon_startup_config = daemon.daemon_startup_config.as_ref().and_then(|c| {
            let server = DaemonStartupConfig::deserialize(c);
            if let Err(e) = server.as_ref() {
                tracing::warn!("Daemon returned invalid DaemonStartupConfig: {:#}", e);
            }
            server.ok()
        });

        if Some(&self.daemon_startup_config) != server_daemon_startup_config.as_ref() {
            return Err(ConstraintUnsatisfiedReason::StartupConfig);
        }

        if let Some(r) = &self.reject_daemon {
            if *r == daemon.daemon_id {
                return Err(ConstraintUnsatisfiedReason::RejectDaemonId);
            }
        }

        // At this point, if ExtraDaemonConstraints is missing, we'll reuse the daemon (as that
        // means it failed to start), if not we proceed to check further constraints.

        let extra = match &daemon.extra {
            Some(e) => e,
            None => return Ok(()),
        };

        match (self.desired_trace_io_state, extra.trace_io_enabled) {
            (DesiredTraceIoState::Enabled, false) => {
                return Err(ConstraintUnsatisfiedReason::TraceIo);
            }
            (DesiredTraceIoState::Disabled, true) => {
                return Err(ConstraintUnsatisfiedReason::TraceIo);
            }
            _ => {}
        }

        if let Some(r) = &self.reject_materializer_state {
            if extra
                .materializer_state_identity
                .as_ref()
                .map_or(false, |i| i == r)
            {
                return Err(ConstraintUnsatisfiedReason::MaterializerStateIdentity);
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Copy, Dupe)]
pub enum DesiredTraceIoState {
    Enabled,
    Disabled,
    Existing,
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum BuckdConnectConstraints {
    ExistingOnly,
    Constraints(DaemonConstraintsRequest),
}

async fn get_channel(
    endpoint: ConnectionType,
    change_to_parent_dir: bool,
) -> buck2_error::Result<Channel> {
    match endpoint {
        ConnectionType::Uds { unix_socket } => {
            Ok(get_channel_uds(&unix_socket, change_to_parent_dir).await?)
        }
        ConnectionType::Tcp { port } => Ok(get_channel_tcp(Ipv4Addr::LOCALHOST, port).await?),
    }
}

#[derive(Clone)]
pub struct BuckAddAuthTokenInterceptor {
    auth_token: AsciiMetadataValue,
}

impl Interceptor for BuckAddAuthTokenInterceptor {
    fn call(&mut self, mut request: Request<()>) -> Result<Request<()>, Status> {
        request
            .metadata_mut()
            .append(BUCK_AUTH_TOKEN_HEADER, self.auth_token.clone());
        Ok(request)
    }
}

pub async fn new_daemon_api_client(
    endpoint: ConnectionType,
    auth_token: String,
) -> buck2_error::Result<DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>>
{
    let channel = get_channel(endpoint, true).await?;
    Ok(DaemonApiClient::with_interceptor(
        channel,
        BuckAddAuthTokenInterceptor {
            auth_token: AsciiMetadataValue::try_from(auth_token)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?,
        },
    )
    .max_encoding_message_size(usize::MAX)
    .max_decoding_message_size(usize::MAX))
}

pub fn buckd_startup_timeout() -> buck2_error::Result<Duration> {
    Ok(Duration::from_secs(
        buck2_env!("BUCKD_STARTUP_TIMEOUT", type=u64)?.unwrap_or(10),
    ))
}

pub fn buckd_startup_init_timeout() -> buck2_error::Result<Duration> {
    Ok(Duration::from_secs(
        buck2_env!("BUCKD_STARTUP_INIT_TIMEOUT", type=u64)?.unwrap_or(90),
    ))
}

/// Responsible for starting the daemon when no daemon is running.
/// This struct holds a lock such that only one daemon is ever started per daemon directory.
struct BuckdLifecycle<'a> {
    paths: &'a InvocationPaths,
    lock: BuckdLifecycleLock,
    constraints: &'a DaemonConstraintsRequest,
}

impl<'a> BuckdLifecycle<'a> {
    async fn lock_with_timeout(
        paths: &'a InvocationPaths,
        deadline: StartupDeadline,
        constraints: &'a DaemonConstraintsRequest,
    ) -> buck2_error::Result<BuckdLifecycle<'a>> {
        Ok(BuckdLifecycle::<'a> {
            paths,
            lock: BuckdLifecycleLock::lock_with_timeout(paths.daemon_dir()?, deadline).await?,
            constraints,
        })
    }

    fn clean_daemon_dir(&self) -> buck2_error::Result<()> {
        self.lock.clean_daemon_dir(true)
    }

    async fn start_server(&self) -> buck2_error::Result<()> {
        let mut args = vec!["--isolation-dir", self.paths.isolation.as_str(), "daemon"];

        if self.constraints.is_trace_io_requested() {
            args.push("--enable-trace-io");
        }

        if let Some(r) = &self.constraints.reject_materializer_state {
            args.push("--reject-materializer-state");
            args.push(r);
        }

        let mut daemon_env_vars = Vec::new();

        daemon_env_vars.push((OsStr::new("RUST_BACKTRACE"), OsStr::new("1")));

        // TODO(nga): We create too many backtraces during `attrs.source()` coercion. Can be
        //   reproduced with this command:
        //   ```
        //   buck2 --isolation-dir=xx audit providers fbcode//buck2:buck2 --quiet
        //   ```
        //   Which regresses from 15s to 80s when `RUST_LIB_BACKTRACE` is set. So we disable
        //   backtraces in the daemon unless the user has explicitly asked for them. We
        //   intentionally avoid considering the `RUST_BACKTRACE` variables that buck was invoked
        //   with, because a lot of Rust tooling sets those without meaning to influence this
        //   behavior.
        daemon_env_vars.push((
            OsStr::new("RUST_LIB_BACKTRACE"),
            OsStr::new(buck2_env!("BUCK2_LIB_BACKTRACE")?.unwrap_or("0")),
        ));

        if env::var_os("FORCE_WANT_RESTART").is_some() {
            // Disable restarter for the actual daemon command, even if it was forced, otherwise we
            // restart the daemon when it exits.
            daemon_env_vars.push((OsStr::new("FORCE_WANT_RESTART"), OsStr::new("false")));
        }

        if cfg!(unix) {
            // On Unix we spawn a process which forks and exits,
            // and here we wait for that spawned process to terminate.
            self.start_server_unix(
                args,
                &daemon_env_vars,
                &self.constraints.daemon_startup_config,
            )
            .await
        } else {
            self.start_server_windows(
                args,
                &daemon_env_vars,
                &self.constraints.daemon_startup_config,
            )
        }
    }

    fn start_server_windows(
        &self,
        mut args: Vec<&str>,
        daemon_env_vars: &[(&OsStr, &OsStr)],
        daemon_startup_config: &DaemonStartupConfig,
    ) -> buck2_error::Result<()> {
        let daemon_startup_config = daemon_startup_config.serialize()?;
        args.extend(["--dont-daemonize"]);
        spawn_background_process_on_windows(
            self.paths.project_root().root(),
            &get_daemon_exe()?,
            args.into_iter()
                .chain(std::iter::once(daemon_startup_config.as_str())),
            daemon_env_vars,
        )
    }

    async fn start_server_unix(
        &self,
        args: Vec<&str>,
        daemon_env_vars: &[(&OsStr, &OsStr)],
        daemon_startup_config: &DaemonStartupConfig,
    ) -> buck2_error::Result<()> {
        let project_dir = self.paths.project_root();
        let timeout_secs = buckd_startup_timeout()?;

        let daemon_exe = get_daemon_exe()?;
        let slice_name = format!(
            "buck2-daemon.{}.{}",
            replace_unit_delimiter(project_dir.name().unwrap_or("unknown_project")),
            replace_unit_delimiter(self.paths.isolation.as_str())
        );
        let systemd_runner =
            SystemdRunner::create_if_enabled(&SystemdRunnerConfig::daemon_runner_config(
                &daemon_startup_config.resource_control,
                ParentSlice::Root(slice_name.clone()),
            ))?;
        let mut cmd = if let Some(systemd_runner) = &systemd_runner {
            systemd_runner
                .ensure_scope_stopped(&format!("{}.scope", &slice_name))
                .await?;
            systemd_runner
                .background_command_linux(daemon_exe, &slice_name, &project_dir.root())
                .into()
        } else {
            async_background_command(daemon_exe)
        };

        cmd.current_dir(project_dir.root())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .args(args);

        cmd.arg(daemon_startup_config.serialize()?);

        if buck2_env!("BUCK_DAEMON_LOG_TO_FILE", type=u8)? == Some(1) {
            cmd.env("BUCK_LOG_TO_FILE_PATH", self.paths.log_dir().as_os_str());
        }

        for (key, val) in daemon_env_vars {
            cmd.env(key, val);
        }

        // For Unix, set a daemon process title
        #[cfg(unix)]
        {
            use std::ffi::OsString;

            let mut title = OsString::new();
            title.push("buck2d");
            if let Some(dir) = project_dir.root().file_name() {
                title.push("[");
                title.push(dir);
                title.push("]");
            }
            cmd.arg0(title);
        }

        let mut child = cmd.spawn()?;
        let mut stdout_taken = child
            .stdout
            .take()
            .buck_error_context("Child should have its stdout piped")
            .unwrap();
        let mut stderr_taken = child
            .stderr
            .take()
            .buck_error_context("Child should have its stderr piped")
            .unwrap();

        let status_fut = async {
            let result = timeout(timeout_secs, child.wait()).await;
            match result {
                Err(_elapsed) => {
                    // The command has timed out, kill the process and wait
                    child.kill().await.buck_error_context(
                        "Error killing process after buck2 daemon launch timing out",
                    )?;
                    // This should return immediately as kill() waits for the process to end. We wait here again to fetch the ExitStatus
                    // Signal termination is not considered a success, so wait() results in an appropriate ExitStatus
                    buck2_error::Ok(child.wait().await?)
                }
                Ok(result) => result.map_err(|e| buck2_error::Error::from(e).into()),
            }
        };
        let stdout_fut = async {
            let mut buf = Vec::new();
            stdout_taken
                .read_to_end(&mut buf)
                .await
                .buck_error_context("Error reading stdout of child")?;
            Ok(buf)
        };
        let stderr_fut = async {
            let mut buf = Vec::new();
            stderr_taken
                .read_to_end(&mut buf)
                .await
                .buck_error_context("Error reading stderr of child")?;
            Ok(buf)
        };

        // `buck2 daemon` will either:
        // * fork and kill parent (daemonize) on Unix
        // * or spawn another process and exit on Windows
        // so we wait for termination of the child process.
        let joined = try_join3(status_fut, stdout_fut, stderr_fut).await;
        match joined {
            Err(e) => Err(BuckdConnectError::BuckDaemonStartupFailed {
                code: 1,
                stdout: "".to_owned(),
                stderr: format!("Failed to launch Buck2 daemon: {:#}", e),
            }
            .into()),
            Ok((status, stdout, stderr)) => {
                if !status.success() {
                    Err(BuckdConnectError::BuckDaemonStartupFailed {
                        code: status.code().unwrap_or(1),
                        stdout: String::from_utf8_lossy(&stdout).to_string(),
                        stderr: String::from_utf8_lossy(&stderr).to_string(),
                    }
                    .into())
                } else {
                    if let Some(systemd_runner) = systemd_runner {
                        // set memory limits to buck2 daemon slice to control
                        // the buck memory including daemon, fork-server and all actions
                        systemd_runner
                            .set_slice_memory_limit(&format!("{}.slice", slice_name))
                            .await?;
                    }
                    Ok(())
                }
            }
        }
    }
}

/// Represents an established connection to the daemon. We then upgrade it into a
/// BootstrapBuckdClient by querying constraints. This is a separate step so that we retry
/// establishing the channel but not querying constraints.
pub struct BuckdChannel {
    info: DaemonProcessInfo,
    daemon_dir: DaemonDir,
    client: DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
}

impl BuckdChannel {
    /// Upgrade this BuckdChannel to a BootstrapBuckdClient.
    pub async fn upgrade(self) -> buck2_error::Result<BootstrapBuckdClient> {
        let Self {
            info,
            daemon_dir,
            mut client,
        } = self;

        let constraints = get_constraints(&mut client)
            .await
            .buck_error_context("Error obtaining daemon constraints")?;

        Ok(BootstrapBuckdClient {
            info,
            daemon_dir,
            client,
            constraints,
        })
    }
}

/// Client used for connection setup. Can be used to create BuckdClientConnector instances later.
#[derive(Clone)]
pub struct BootstrapBuckdClient {
    info: DaemonProcessInfo,
    daemon_dir: DaemonDir,
    client: DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
    /// The constraints for the daemon we're connected to.
    constraints: buck2_cli_proto::DaemonConstraints,
}

impl BootstrapBuckdClient {
    pub async fn connect(
        paths: &InvocationPaths,
        constraints: BuckdConnectConstraints,
        event_subscribers: &mut EventSubscribers,
    ) -> buck2_error::Result<Self> {
        let daemon_dir = paths.daemon_dir()?;

        buck2_core::fs::fs_util::create_dir_all(&daemon_dir.path)
            .with_buck_error_context(|| format!("Error creating daemon dir: {}", daemon_dir))?;

        let res = match constraints {
            BuckdConnectConstraints::ExistingOnly => {
                establish_connection_existing(&daemon_dir).await
            }
            BuckdConnectConstraints::Constraints(constraints) => {
                establish_connection(paths, constraints, event_subscribers).await
            }
        };

        if let Err(e) = res {
            Err(daemon_connect_error(e, paths)
                .await
                .tag([ErrorTag::DaemonConnect]))
        } else {
            res
        }
    }

    pub fn to_connector(self) -> BuckdClientConnector {
        BuckdClientConnector {
            client: BuckdClient {
                daemon_dir: self.daemon_dir,
                client: self.client,
                constraints: self.constraints,
                tailers: None,
            },
        }
    }

    pub(crate) async fn kill(&mut self, reason: &str) -> buck2_error::Result<Pid> {
        kill::kill(&mut self.client, &self.info, reason).await?;
        Pid::from_i64(self.info.pid)
    }

    async fn kill_for_constraints_mismatch(&mut self) -> buck2_error::Result<Pid> {
        self.kill("client expected different buckd constraints")
            .await
    }

    pub fn pid(&self) -> i64 {
        self.info.pid
    }
}

/// Attempt to connect to a daemon that can satisfy specified constraints.
/// If the daemon does not match constraints (different version or does not enable I/O tracing),
/// it will kill it and restart it with the correct constraints.
/// This behavior can be overridden by passing `BuckdConnectConstraints::ExistingOnly`.
/// In that case, then any existing buck daemon (regardless of constraint) is accepted.
pub async fn connect_buckd(
    constraints: BuckdConnectConstraints,
    events_ctx: &mut EventsCtx,
    paths: &InvocationPaths,
) -> buck2_error::Result<BuckdClientConnector> {
    match BootstrapBuckdClient::connect(paths, constraints, &mut events_ctx.subscribers)
        .await
        .map_err(buck2_error::Error::from)
    {
        Ok(client) => Ok(client.to_connector()),
        Err(e) => {
            events_ctx.subscribers.handle_daemon_connection_failure(&e);
            Err(e.into())
        }
    }
}

pub async fn establish_connection_existing(
    daemon_dir: &DaemonDir,
) -> buck2_error::Result<BootstrapBuckdClient> {
    let deadline = StartupDeadline::duration_from_now(buckd_startup_timeout()?)?;
    deadline
        .run(
            "establishing connection to existing Buck daemon",
            async move {
                BuckdProcessInfo::load(daemon_dir)?
                    .create_channel()
                    .await?
                    .upgrade()
                    .await
            },
        )
        .await
        .map_err(buck2_error::Error::from)
}

async fn establish_connection(
    paths: &InvocationPaths,
    constraints: DaemonConstraintsRequest,
    event_subscribers: &mut EventSubscribers,
) -> buck2_error::Result<BootstrapBuckdClient> {
    // There are many places where `establish_connection_inner` may hang.
    // If it does, better print something to the user instead of hanging quietly forever.
    let timeout = buckd_startup_init_timeout()?;
    let deadline = StartupDeadline::duration_from_now(timeout)?;
    deadline
        .down(
            "establishing connection to Buck daemon or start a daemon",
            |timeout| establish_connection_inner(paths, constraints, timeout, event_subscribers),
        )
        .await
        .map_err(buck2_error::Error::from)
}

fn explain_failed_to_connect_reason(reason: buck2_data::DaemonWasStartedReason) -> &'static str {
    match reason {
        DaemonWasStartedReason::UnknownReason => "Unknown reason",
        DaemonWasStartedReason::ConstraintMismatchVersion => "Version mismatch",
        DaemonWasStartedReason::ConstraintMismatchUserVersion => "User version mismatch",
        DaemonWasStartedReason::ConstraintMismatchStartupConfig => "Startup config mismatch",
        DaemonWasStartedReason::ConstraintRejectDaemonId => "Reject daemon id",
        DaemonWasStartedReason::ConstraintMismatchTraceIo => "Trace IO mismatch",
        DaemonWasStartedReason::ConstraintMismatchMaterializerStateIdentity => {
            "Materializer state identity mismatch"
        }
        DaemonWasStartedReason::CouldNotConnectToDaemon => {
            // TODO(nga): get rid of this variant.
            "Could not connect to daemon"
        }
        DaemonWasStartedReason::TimedOutConnectingToDaemon => "Timed out connecting to daemon",
        DaemonWasStartedReason::TimeoutCalculationError => "Timeout calculation error",
        DaemonWasStartedReason::NoBuckdInfo => "No buckd.info",
        DaemonWasStartedReason::CouldNotLoadBuckdInfo => "Could not load buckd.info",
        DaemonWasStartedReason::NoDaemonProcess => "buck2 daemon is not running",
    }
}

#[allow(clippy::collapsible_match)]
async fn establish_connection_inner(
    paths: &InvocationPaths,
    constraints: DaemonConstraintsRequest,
    deadline: StartupDeadline,
    event_subscribers: &mut EventSubscribers,
) -> buck2_error::Result<BootstrapBuckdClient> {
    let daemon_dir = paths.daemon_dir()?;

    let res = deadline
        .half()?
        .run("connecting to existing buck daemon", {
            try_connect_existing_before_acquiring_lifecycle_lock(&daemon_dir, &constraints).map(Ok)
        })
        .await;
    if let Ok(connect_before_restart) = res {
        if let ConnectBeforeRestart::Accepted(client) = connect_before_restart {
            return Ok(client);
        };
    }

    // At this point, we've either failed to connect to buckd or buckd had the wrong constraints.
    // Get the lifecycle lock to ensure we don't have races with other processes as we check and change things.
    let lifecycle_lock = deadline
        .down("acquire lifecycle lock", |deadline| {
            BuckdLifecycle::lock_with_timeout(paths, deadline, &constraints)
        })
        .await?;

    // Even if we didn't connect before, it's possible that we just raced with another invocation
    // starting the server, so we try to connect again while holding the lock.
    let daemon_was_started_reason = {
        match BuckdProcessInfo::load_if_exists(&daemon_dir) {
            Ok(Some(buckd_info)) => {
                match try_connect_existing(&buckd_info, &deadline, &lifecycle_lock).await {
                    Ok(channel) => {
                        let mut client = channel.upgrade().await?;

                        let reason = match constraints.satisfied(&client.constraints) {
                            Ok(()) => return Ok(client),
                            Err(reason) => reason,
                        };

                        if is_nested_invocation(
                            get_possibly_nested_invocation_daemon_uuid().as_ref(),
                            &client.constraints,
                        ) {
                            match reason {
                                ConstraintUnsatisfiedReason::TraceIo
                                | ConstraintUnsatisfiedReason::StartupConfig => {
                                    return Err(BuckdConnectError::NestedConstraintMismatch {
                                        reason,
                                    }
                                    .into());
                                }
                                _ => (),
                            }
                        }

                        event_subscribers
                            .eprintln(&format!(
                                "buck2 daemon constraint mismatch: {reason}; killing daemon..."
                            ))
                            .await?;

                        deadline
                            .run(
                                "sending kill command to the Buck daemon",
                                client.kill_for_constraints_mismatch(),
                            )
                            .await?;

                        event_subscribers
                            .eprintln("Starting new buck2 daemon...")
                            .await?;

                        reason.to_daemon_was_started_reason()
                    }
                    Err(reason) => {
                        // TODO(nga): should print some proper message here.
                        hard_kill_until(&buckd_info.info, deadline.down_deadline()?.deadline())
                            .await?;

                        event_subscribers
                            .eprintln(&format!(
                                "Could not connect to buck2 daemon ({}), starting a new one...",
                                explain_failed_to_connect_reason(reason)
                            ))
                            .await?;

                        reason
                    }
                }
            }
            Ok(None) => {
                event_subscribers
                    .eprintln("Starting new buck2 daemon...")
                    .await?;

                buck2_data::DaemonWasStartedReason::NoBuckdInfo
            }
            Err(e) => {
                event_subscribers
                    .eprintln(&format!(
                        "Could not load buckd.info: {}, starting new buck2 daemon...",
                        e
                    ))
                    .await?;

                buck2_data::DaemonWasStartedReason::CouldNotLoadBuckdInfo
            }
        }
    };

    deadline
        .down(
            &format!(
                "starting new buck2 daemon for reason: {}",
                explain_failed_to_connect_reason(daemon_was_started_reason)
            ),
            |deadline| {
                start_new_buckd_and_connect(
                    deadline,
                    &lifecycle_lock,
                    paths,
                    &constraints,
                    event_subscribers,
                    daemon_was_started_reason,
                )
            },
        )
        .await
}

async fn start_new_buckd_and_connect(
    deadline: StartupDeadline,
    lifecycle_lock: &BuckdLifecycle<'_>,
    paths: &InvocationPaths,
    constraints: &DaemonConstraintsRequest,
    event_subscribers: &mut EventSubscribers,
    daemon_was_started_reason: buck2_data::DaemonWasStartedReason,
) -> buck2_error::Result<BootstrapBuckdClient> {
    // Daemon dir may be corrupted. Safer to delete it.
    lifecycle_lock
        .clean_daemon_dir()
        .buck_error_context("Cleaning daemon dir")?;

    // Now there's definitely no server that can be connected to
    lifecycle_lock.start_server().await?;
    // It might take a little bit for the daemon server to start up. We could wait for the buckd.info
    // file to appear, but it's just as easy to just retry the connection itself.

    let channel = deadline
        .retrying(
            "connect to buckd after server start",
            Duration::from_millis(5),
            Duration::from_millis(100),
            || async { BuckdProcessInfo::load_and_create_channel(&paths.daemon_dir()?).await },
        )
        .await?;

    let client = channel.upgrade().await?;

    if let Err(reason) = constraints.satisfied(&client.constraints) {
        return Err(BuckdConnectError::BuckDaemonConstraintWrongAfterStart {
            reason,
            expected: constraints.clone(),
            actual: client.constraints,
        }
        .into());
    }

    event_subscribers.handle_daemon_started(daemon_was_started_reason);

    event_subscribers
        .eprintln("Connected to new buck2 daemon.")
        .await?;

    Ok(client)
}

enum ConnectBeforeRestart {
    Accepted(BootstrapBuckdClient),
    Rejected,
}

/// Connect to buckd before attempt to restart the server.
///
/// # Returns
///
/// * `Ok(Some(client))` if we connected to an existing buckd
/// * `Ok(None)` if we failed to connect and should restart buckd
/// * `Err` if we failed to connect and should abandon startup
async fn try_connect_existing_before_acquiring_lifecycle_lock(
    daemon_dir: &DaemonDir,
    constraints: &DaemonConstraintsRequest,
) -> ConnectBeforeRestart {
    match BuckdProcessInfo::load_and_create_channel(daemon_dir).await {
        Ok(channel) => {
            let Ok(client) = channel.upgrade().await else {
                return ConnectBeforeRestart::Rejected;
            };
            if constraints.satisfied(&client.constraints).is_ok() {
                ConnectBeforeRestart::Accepted(client)
            } else {
                ConnectBeforeRestart::Rejected
            }
        }
        Err(e) => {
            tracing::debug!("Connect failed: {:#}", e);
            ConnectBeforeRestart::Rejected
        }
    }
}

async fn try_connect_existing(
    buckd_info: &BuckdProcessInfo<'_>,
    timeout: &StartupDeadline,
    _lock: &BuckdLifecycle<'_>,
) -> Result<BuckdChannel, buck2_data::DaemonWasStartedReason> {
    let timeout: buck2_error::Result<_> = try { timeout.min(buckd_startup_timeout()?)? };
    let Ok(timeout) = timeout else {
        return Err(buck2_data::DaemonWasStartedReason::TimeoutCalculationError);
    };
    let Ok(rem_duration) = timeout.rem_duration("connect existing buckd") else {
        return Err(buck2_data::DaemonWasStartedReason::TimedOutConnectingToDaemon);
    };
    match tokio::time::timeout(rem_duration, buckd_info.create_channel()).await {
        Ok(Ok(channel)) => Ok(channel),
        Ok(Err(_)) => {
            let Ok(pid) = buckd_info.pid() else {
                return Err(buck2_data::DaemonWasStartedReason::CouldNotLoadBuckdInfo);
            };
            let buckd_process_exists = process_exists(pid).unwrap_or(true);
            if !buckd_process_exists {
                // We don't delete the `buckd.info` file, and if we failed to connect,
                // the most likely reason is that the daemon process doesn't exist.
                Err(buck2_data::DaemonWasStartedReason::NoDaemonProcess)
            } else {
                Err(buck2_data::DaemonWasStartedReason::CouldNotConnectToDaemon)
            }
        }
        Err(e) => {
            let _assert_type: tokio::time::error::Elapsed = e;
            Err(buck2_data::DaemonWasStartedReason::TimedOutConnectingToDaemon)
        }
    }
}

pub struct BuckdProcessInfo<'a> {
    pub(crate) info: DaemonProcessInfo,
    daemon_dir: &'a DaemonDir,
}

impl<'a> BuckdProcessInfo<'a> {
    /// Utility method for places that want to match on the overall result of those two operations.
    async fn load_and_create_channel(
        daemon_dir: &'a DaemonDir,
    ) -> buck2_error::Result<BuckdChannel> {
        Self::load(daemon_dir)?.create_channel().await
    }

    pub fn load(daemon_dir: &'a DaemonDir) -> buck2_error::Result<Self> {
        Self::load_if_exists(daemon_dir)?.with_buck_error_context(|| {
            format!(
                "buckd info {} does not exist",
                daemon_dir.buckd_info().display()
            )
        })
    }

    pub fn load_if_exists(daemon_dir: &'a DaemonDir) -> buck2_error::Result<Option<Self>> {
        let location = daemon_dir.buckd_info();
        let file = match File::open(&location) {
            Ok(file) => file,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(e) => {
                return Err(e).with_buck_error_context(|| {
                    format!("Trying to open buckd info, `{}`", location.display())
                });
            }
        };
        let reader = BufReader::new(file);
        let info = serde_json::from_reader(reader).with_buck_error_context(|| {
            format!(
                "Error parsing daemon info in `{}`. \
                Try deleting that file and running `buck2 killall` before running your command again",
                location.display(),
            )
        })?;

        Ok(Some(BuckdProcessInfo { info, daemon_dir }))
    }

    pub async fn create_channel(&self) -> buck2_error::Result<BuckdChannel> {
        tracing::debug!("Creating channel to: {}", self.info.endpoint);
        let connection_type = ConnectionType::parse(&self.info.endpoint)?;

        let client = new_daemon_api_client(connection_type, self.info.auth_token.clone())
            .await
            .buck_error_context("Error connecting")?;

        Ok(BuckdChannel {
            info: self.info.clone(),
            daemon_dir: self.daemon_dir.clone(),
            client,
        })
    }

    pub async fn hard_kill(&self) -> buck2_error::Result<()> {
        kill::hard_kill(&self.info).await
    }

    pub fn pid(&self) -> buck2_error::Result<Pid> {
        Pid::from_i64(self.info.pid)
    }
}

async fn get_constraints(
    client: &mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
) -> buck2_error::Result<buck2_cli_proto::DaemonConstraints> {
    // NOTE: No tailers in bootstrap client, we capture logs if we fail to connect, but
    // otherwise we leave them alone.
    let status = EventsCtx::new(EventSubscribers::new(vec![Box::new(StdoutStderrForwarder)]))
        .unpack_oneshot(None, {
            client.status(tonic::Request::new(buck2_cli_proto::StatusRequest {
                snapshot: false,
            }))
        })
        .await?;

    let status: buck2_cli_proto::StatusResponse = match status {
        CommandOutcome::Success(r) => Ok(r),
        CommandOutcome::Failure(_) => Err(buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "Unexpected failure message in status()"
        )),
    }?;

    Ok(status.daemon_constraints.unwrap_or_default())
}

pub fn get_daemon_exe() -> buck2_error::Result<PathBuf> {
    let exe = env::current_exe().buck_error_context("Failed to get current exe")?;
    if buck2_core::client_only::is_client_only()? {
        let ext = if cfg!(windows) { ".exe" } else { "" };
        Ok(exe
            .parent()
            .buck_error_context("Expected current exe to be in a directory")?
            .join(format!("buck2-daemon{}", ext)))
    } else {
        Ok(exe)
    }
}

#[derive(Debug, buck2_error::Error)]
#[allow(clippy::large_enum_variant)]
#[buck2(tag = DaemonConnect)]
enum BuckdConnectError {
    #[error(
        "buck daemon startup failed with exit code {code}\nstdout:\n{stdout}\nstderr:\n{stderr}"
    )]
    BuckDaemonStartupFailed {
        code: i32,
        stdout: String,
        stderr: String,
    },
    #[error(
        "during buck daemon startup, the started process did not match constraints ({reason}).\nexpected: {expected:?}\nactual: {actual:?}"
    )]
    BuckDaemonConstraintWrongAfterStart {
        reason: ConstraintUnsatisfiedReason,
        expected: DaemonConstraintsRequest,
        actual: buck2_cli_proto::DaemonConstraints,
    },
    #[error("buck2 daemon constraint mismatch during nested invocation: {reason}")]
    NestedConstraintMismatch { reason: ConstraintUnsatisfiedReason },
}

async fn daemon_connect_error(
    error: buck2_error::Error,
    paths: &InvocationPaths,
) -> buck2_error::Error {
    let error_report: Result<buck2_data::ErrorReport, RetryError<buck2_error::Error>> = retrying(
        Duration::from_millis(50),
        Duration::from_millis(100),
        Duration::from_millis(500),
        || async {
            let daemon_dir = paths.daemon_dir()?;
            let error_log = std::fs::read(daemon_dir.buckd_error_log())?;

            let error_report = buck2_data::ErrorReport::deserialize(
                &mut serde_json::Deserializer::from_slice(&error_log),
            )?;
            Ok(error_report)
        },
    )
    .await;

    let error = if let Ok(error_report) = error_report {
        // Daemon wrote an error and most likely quit.
        // (note: not using tags() as a workaround for likely false positive ASAN failure)
        let tags: Vec<ErrorTag> = error_report
            .tags
            .into_iter()
            .filter_map(|v| buck2_error::ErrorTag::try_from(v).ok())
            .collect();
        let daemon_error = buck2_error::Error::new(
            error_report.message.clone(),
            *tags.first().unwrap_or(&ErrorTag::Tier0),
            SourceLocation::new(file!()),
            None,
        )
        .tag(tags);
        if daemon_error.has_tag(ErrorTag::DaemonStateInitFailed) {
            // If error is in this stage of daemon init, exclude connection error details/workaround message.
            // TODO(ctolliday) always hide connection error details/workaround message if there is a structured error from daemon.
            return daemon_error;
        }
        daemon_error
    } else {
        // Daemon crashed or panicked, or is still running but can't be connected to.
        let stderr = paths
            .daemon_dir()
            .and_then(|dir| {
                let stderr = std::fs::read(dir.buckd_stderr())?;
                Ok(String::from_utf8_lossy(&stderr).into_owned())
            })
            .unwrap_or_else(|_| "<none>".to_owned());

        let stderr = truncate(&stderr, 64000);
        let error = error
            .context(format!(
                "Error connecting to the daemon, daemon stderr follows:\n{}",
                stderr
            ))
            .tag([ErrorTag::DaemonConnect]);

        classify_server_stderr(error, &stderr)
    };
    let delete_commad = if cfg!(windows) {
        "rmdir /s /q %USERPROFILE%\\.buck\\buckd"
    } else {
        "rm -rf ~/.buck/buckd"
    };
    // FIXME(ctolliday) we should check if the pid at daemon_dir.buckd_pid is still running before suggesting buck2 kill.
    let error_message = format!(
        "Failed to connect to buck daemon.
    Try running `buck2 kill` and your command afterwards.
    Alternatively, try running `{}` and your command afterwards",
        delete_commad
    );
    error.context(error_message)
}

fn is_nested_invocation(
    buck2_daemon_uuid: Option<&String>,
    daemon: &buck2_cli_proto::DaemonConstraints,
) -> bool {
    buck2_daemon_uuid.map_or(false, |uuid| uuid == &daemon.daemon_id)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn constraints(trace_io_enabled: bool) -> buck2_cli_proto::DaemonConstraints {
        buck2_cli_proto::DaemonConstraints {
            version: "version".to_owned(),
            user_version: Some("test".to_owned()),
            daemon_id: "foo".to_owned(),
            extra: Some(buck2_cli_proto::ExtraDaemonConstraints {
                trace_io_enabled,
                materializer_state_identity: None,
            }),
            daemon_startup_config: Some(
                serde_json::to_string(&DaemonStartupConfig::testing_empty()).unwrap(),
            ),
        }
    }

    fn request(desired_trace_io_state: DesiredTraceIoState) -> DaemonConstraintsRequest {
        DaemonConstraintsRequest {
            version: "version".to_owned(),
            user_version: Some("test".to_owned()),
            desired_trace_io_state,
            nested_invocation_daemon_uuid: None,
            reject_daemon: None,
            reject_materializer_state: None,
            daemon_startup_config: DaemonStartupConfig::testing_empty(),
        }
    }

    #[test]
    fn test_constraints_equal_for_same_constraints() {
        let req = request(DesiredTraceIoState::Enabled);
        let daemon = constraints(true);
        assert!(req.satisfied(&daemon).is_ok());
    }

    #[test]
    fn test_constraints_equal_for_trace_io_existing() {
        let req = request(DesiredTraceIoState::Existing);
        let daemon = constraints(true);
        assert!(req.satisfied(&daemon).is_ok());
    }

    #[test]
    fn test_constraints_unequal_for_trace_io() {
        let req = request(DesiredTraceIoState::Disabled);
        let daemon = constraints(true);
        assert!(req.satisfied(&daemon).is_err());
    }

    #[test]
    fn test_trace_io_is_enabled() {
        let c = request(DesiredTraceIoState::Enabled);
        assert!(c.is_trace_io_requested());

        let c = request(DesiredTraceIoState::Disabled);
        assert!(!c.is_trace_io_requested());
    }

    #[test]
    fn test_reject_daemon() {
        let mut req = DaemonConstraintsRequest {
            version: "foo".to_owned(),
            user_version: None,
            desired_trace_io_state: DesiredTraceIoState::Existing,
            reject_daemon: None,
            nested_invocation_daemon_uuid: None,
            reject_materializer_state: None,
            daemon_startup_config: DaemonStartupConfig::testing_empty(),
        };

        let daemon = buck2_cli_proto::DaemonConstraints {
            version: "foo".to_owned(),
            user_version: None,
            daemon_id: "ddd".to_owned(),
            extra: None,
            daemon_startup_config: Some(
                serde_json::to_string(&DaemonStartupConfig::testing_empty()).unwrap(),
            ),
        };

        assert!(req.satisfied(&daemon).is_ok());
        req.reject_daemon = Some("zzz".to_owned());
        assert!(req.satisfied(&daemon).is_ok());
        req.reject_daemon = Some("ddd".to_owned());
        assert!(req.satisfied(&daemon).is_err());
    }

    #[test]
    fn test_reject_materializer_state() {
        let mut req = DaemonConstraintsRequest {
            version: "foo".to_owned(),
            user_version: None,
            desired_trace_io_state: DesiredTraceIoState::Existing,
            reject_daemon: None,
            nested_invocation_daemon_uuid: None,
            reject_materializer_state: None,
            daemon_startup_config: DaemonStartupConfig::testing_empty(),
        };

        let daemon = buck2_cli_proto::DaemonConstraints {
            version: "foo".to_owned(),
            user_version: None,
            daemon_id: "ddd".to_owned(),
            extra: Some(buck2_cli_proto::ExtraDaemonConstraints {
                trace_io_enabled: false,
                materializer_state_identity: Some("mmm".to_owned()),
            }),
            daemon_startup_config: Some(
                serde_json::to_string(&DaemonStartupConfig::testing_empty()).unwrap(),
            ),
        };

        assert!(req.satisfied(&daemon).is_ok());
        req.reject_materializer_state = Some("zzz".to_owned());
        assert!(req.satisfied(&daemon).is_ok());
        req.reject_materializer_state = Some("mmm".to_owned());
        assert!(req.satisfied(&daemon).is_err());
    }

    #[test]
    fn test_daemon_buster() {
        let mut req = DaemonConstraintsRequest {
            version: "foo".to_owned(),
            user_version: None,
            desired_trace_io_state: DesiredTraceIoState::Existing,
            reject_daemon: None,
            nested_invocation_daemon_uuid: None,
            reject_materializer_state: None,
            daemon_startup_config: DaemonStartupConfig::testing_empty(),
        };

        let daemon = buck2_cli_proto::DaemonConstraints {
            version: "foo".to_owned(),
            user_version: None,
            daemon_id: "ddd".to_owned(),
            extra: Some(buck2_cli_proto::ExtraDaemonConstraints {
                trace_io_enabled: false,
                materializer_state_identity: Some("mmm".to_owned()),
            }),
            daemon_startup_config: Some(
                serde_json::to_string(&DaemonStartupConfig::testing_empty()).unwrap(),
            ),
        };

        assert!(req.satisfied(&daemon).is_ok());
        req.daemon_startup_config.daemon_buster = Some("1".to_owned());
        assert!(req.satisfied(&daemon).is_err());
    }

    #[test]
    fn test_constraints_nested_invocation_diff_user_version() {
        let mut req = DaemonConstraintsRequest {
            version: "foo".to_owned(),
            user_version: Some("fake_version_1".to_owned()),
            desired_trace_io_state: DesiredTraceIoState::Existing,
            nested_invocation_daemon_uuid: None,
            reject_daemon: None,
            reject_materializer_state: None,
            daemon_startup_config: DaemonStartupConfig::testing_empty(),
        };

        let daemon = buck2_cli_proto::DaemonConstraints {
            version: "foo".to_owned(),
            user_version: Some("fake_version_2".to_owned()),
            daemon_id: "ddd".to_owned(),
            extra: Some(buck2_cli_proto::ExtraDaemonConstraints {
                trace_io_enabled: false,
                materializer_state_identity: Some("mmm".to_owned()),
            }),
            daemon_startup_config: Some(
                serde_json::to_string(&DaemonStartupConfig::testing_empty()).unwrap(),
            ),
        };

        assert!(req.satisfied(&daemon).is_err());

        req.nested_invocation_daemon_uuid = Some("ddd".to_owned());
        assert!(req.satisfied(&daemon).is_ok());
    }
}
