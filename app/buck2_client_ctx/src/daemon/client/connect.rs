/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::env;
use std::fs::File;
use std::io::BufReader;
use std::net::Ipv4Addr;
use std::time::Duration;

use anyhow::Context;
use buck2_cli_proto::daemon_api_client::DaemonApiClient;
use buck2_cli_proto::DaemonProcessInfo;
use buck2_common::buckd_connection::ConnectionType;
use buck2_common::buckd_connection::BUCK_AUTH_TOKEN_HEADER;
use buck2_common::client_utils::get_channel_tcp;
use buck2_common::client_utils::get_channel_uds;
use buck2_common::daemon_dir::DaemonDir;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::env_helper::EnvHelper;
use buck2_util::process::async_background_command;
use dupe::Dupe;
use futures::future::try_join3;
use thiserror::Error;
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
use crate::daemon::client::BuckdClient;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::BuckdLifecycleLock;
use crate::daemon::daemon_windows::spawn_background_process_on_windows;
use crate::daemon_constraints;
use crate::events_ctx::EventsCtx;
use crate::startup_deadline::StartupDeadline;
use crate::subscribers::stdout_stderr_forwarder::StdoutStderrForwarder;
use crate::subscribers::subscriber::EventSubscriber;

#[allow(clippy::large_enum_variant)]
#[derive(Debug)]
pub enum ConstraintCheckResult {
    Match,
    Mismatch {
        expected: DaemonConstraintsRequest,
        actual: buck2_cli_proto::DaemonConstraints,
    },
}

impl ConstraintCheckResult {
    fn is_match(&self) -> bool {
        matches!(self, Self::Match)
    }
}

/// The client side matcher for DaemonConstraints.
#[derive(Clone, Debug)]
pub struct DaemonConstraintsRequest {
    version: String,
    user_version: Option<String>,
    desired_trace_io_state: DesiredTraceIoState,
    pub reject_daemon: Option<String>,
    pub reject_materializer_state: Option<String>,
}

impl DaemonConstraintsRequest {
    pub fn new(desired_trace_io_state: DesiredTraceIoState) -> anyhow::Result<Self> {
        Ok(Self {
            version: daemon_constraints::version(),
            user_version: daemon_constraints::user_version()?,
            desired_trace_io_state,
            reject_daemon: None,
            reject_materializer_state: None,
        })
    }

    fn satisfied(&self, daemon: &buck2_cli_proto::DaemonConstraints) -> bool {
        if self.version != daemon.version {
            return false;
        }

        if self.user_version != daemon.user_version {
            return false;
        }

        if let Some(r) = &self.reject_daemon {
            if *r == daemon.daemon_id {
                return false;
            }
        }

        // At this point, if ExtraDaemonConstraints is missing, we'll reuse the daemon (as that
        // means it failed to start), if not we proceed to check further constraints.

        let extra = match &daemon.extra {
            Some(e) => e,
            None => return true,
        };

        match (self.desired_trace_io_state, extra.trace_io_enabled) {
            (DesiredTraceIoState::Enabled, false) => return false,
            (DesiredTraceIoState::Disabled, true) => return false,
            _ => {}
        }

        if let Some(r) = &self.reject_materializer_state {
            if extra
                .materializer_state_identity
                .as_ref()
                .map_or(false, |i| i == r)
            {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Clone, Copy, Dupe)]
pub enum DesiredTraceIoState {
    Enabled,
    Disabled,
    Existing,
}

#[derive(Debug)]
pub enum BuckdConnectConstraints {
    ExistingOnly,
    Constraints(DaemonConstraintsRequest),
}

impl BuckdConnectConstraints {
    pub fn existing_only(&self) -> bool {
        matches!(self, Self::ExistingOnly)
    }

    pub fn satisfied(&self, daemon: &buck2_cli_proto::DaemonConstraints) -> ConstraintCheckResult {
        match self {
            Self::ExistingOnly => ConstraintCheckResult::Match,
            Self::Constraints(expected) => {
                let satisfied = expected.satisfied(daemon);

                tracing::debug!(
                    "Matching constraints: {:?} {:?} -> satisfied = {}",
                    self,
                    daemon,
                    satisfied
                );

                if satisfied {
                    ConstraintCheckResult::Match
                } else {
                    ConstraintCheckResult::Mismatch {
                        expected: expected.clone(),
                        actual: daemon.clone(),
                    }
                }
            }
        }
    }

    pub fn is_trace_io_requested(&self) -> bool {
        if let Self::Constraints(constraints) = self {
            matches!(
                constraints.desired_trace_io_state,
                DesiredTraceIoState::Enabled
            )
        } else {
            false
        }
    }

    pub fn reject_materializer_state(&self) -> Option<&str> {
        match self {
            Self::Constraints(c) => c.reject_materializer_state.as_deref(),
            Self::ExistingOnly => None,
        }
    }
}

static BUCKD_STARTUP_TIMEOUT: EnvHelper<u64> = EnvHelper::new("BUCKD_STARTUP_TIMEOUT");

async fn get_channel(
    endpoint: ConnectionType,
    change_to_parent_dir: bool,
) -> anyhow::Result<Channel> {
    match endpoint {
        ConnectionType::Uds { unix_socket } => {
            get_channel_uds(&unix_socket, change_to_parent_dir).await
        }
        ConnectionType::Tcp { port } => get_channel_tcp(Ipv4Addr::LOCALHOST, port).await,
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
) -> anyhow::Result<DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>> {
    let channel = get_channel(endpoint, true).await?;
    Ok(DaemonApiClient::with_interceptor(
        channel,
        BuckAddAuthTokenInterceptor {
            auth_token: AsciiMetadataValue::try_from(auth_token)?,
        },
    ))
}

fn buckd_startup_timeout() -> anyhow::Result<Duration> {
    Ok(Duration::from_secs(
        BUCKD_STARTUP_TIMEOUT.get_copied()?.unwrap_or(10),
    ))
}

/// Responsible for starting the daemon when no daemon is running.
/// This struct holds a lock such that only one daemon is ever started per daemon directory.
struct BuckdLifecycle<'a> {
    paths: &'a InvocationPaths,
    lock: BuckdLifecycleLock,
    constraints: &'a BuckdConnectConstraints,
}

impl<'a> BuckdLifecycle<'a> {
    async fn lock_with_timeout(
        paths: &'a InvocationPaths,
        deadline: StartupDeadline,
        constraints: &'a BuckdConnectConstraints,
    ) -> anyhow::Result<BuckdLifecycle<'a>> {
        Ok(BuckdLifecycle::<'a> {
            paths,
            lock: BuckdLifecycleLock::lock_with_timeout(paths.daemon_dir()?, deadline).await?,
            constraints,
        })
    }

    fn clean_daemon_dir(&self) -> anyhow::Result<()> {
        self.lock.clean_daemon_dir()
    }

    async fn start_server(&self) -> anyhow::Result<()> {
        let mut args = vec!["--isolation-dir", self.paths.isolation.as_str()];

        if self.constraints.is_trace_io_requested() {
            args.push("--enable-trace-io");
        }

        if let Some(r) = self.constraints.reject_materializer_state() {
            args.push("--reject-materializer-state");
            args.push(r);
        }

        let daemon_env_vars = if env::var_os("RUST_BACKTRACE").is_some()
            || env::var_os("RUST_LIB_BACKTRACE").is_some()
        {
            // Inherit.
            [].as_slice()
        } else {
            [
                ("RUST_BACKTRACE", "1"),
                // TODO(nga): somewhere we capture too many backtraces, probably
                //   we create too many `anyhow::Error` on non-error paths.
                //   Probably somewhere in Starlark, because of "evaluating build file" spans.
                //   Can be reproduced with this command:
                //   ```
                //   buck2 --isolation-dir=xx audit providers fbcode//buck2:buck2 --quiet
                //   ```
                //   Which regresses from 15s to 80s when `RUST_LIB_BACKTRACE` is set.
                ("RUST_LIB_BACKTRACE", "0"),
            ]
            .as_slice()
        };

        if cfg!(unix) {
            // On Unix we spawn a process which forks and exits,
            // and here we wait for that spawned process to terminate.
            self.start_server_unix(args, daemon_env_vars).await
        } else {
            self.start_server_windows(args, daemon_env_vars)
        }
    }

    fn start_server_windows(
        &self,
        mut args: Vec<&str>,
        daemon_env_vars: &[(&str, &str)],
    ) -> anyhow::Result<()> {
        args.extend(["daemon", "--dont-daemonize"]);
        spawn_background_process_on_windows(
            self.paths.project_root().root(),
            &env::current_exe()?,
            args,
            daemon_env_vars,
        )
    }

    async fn start_server_unix(
        &self,
        args: Vec<&str>,
        daemon_env_vars: &[(&str, &str)],
    ) -> anyhow::Result<()> {
        let project_dir = self.paths.project_root();
        let timeout_secs = Duration::from_secs(env::var("BUCKD_STARTUP_TIMEOUT").map_or(10, |t| {
            t.parse::<u64>()
                .unwrap_or_else(|_| panic!("Cannot convert {} to int", t))
        }));

        let mut cmd =
            async_background_command(std::env::current_exe().context("Failed to get current exe")?);
        cmd.current_dir(project_dir.root())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .args(args);

        cmd.arg("daemon");

        static DAEMON_LOG_TO_FILE: EnvHelper<u8> = EnvHelper::<u8>::new("BUCK_DAEMON_LOG_TO_FILE");
        if DAEMON_LOG_TO_FILE.get_copied()? == Some(1) {
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
            .context("Child should have its stdout piped")
            .unwrap();
        let mut stderr_taken = child
            .stderr
            .take()
            .context("Child should have its stderr piped")
            .unwrap();

        let status_fut = async {
            let result = timeout(timeout_secs, child.wait()).await;
            match result {
                Err(_elapsed) => {
                    // The command has timed out, kill the process and wait
                    child
                        .kill()
                        .await
                        .context("Error killing process after buck2 daemon launch timing out")?;
                    // This should return immediately as kill() waits for the process to end. We wait here again to fetch the ExitStatus
                    // Signal termination is not considered a success, so wait() results in an appropriate ExitStatus
                    Ok(child.wait().await?)
                }
                Ok(result) => result.map_err(|e| anyhow::anyhow!(e)),
            }
        };
        let stdout_fut = async {
            let mut buf = Vec::new();
            stdout_taken
                .read_to_end(&mut buf)
                .await
                .context("Error reading stdout of child")?;
            Ok(buf)
        };
        let stderr_fut = async {
            let mut buf = Vec::new();
            stderr_taken
                .read_to_end(&mut buf)
                .await
                .context("Error reading stderr of child")?;
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
                    Ok(())
                }
            }
        }
    }
}

/// Represents an established connection to the daemon. We then upgrade it into a
/// BootstrapBuckdClient by querying constraints. This is a separate step so that we retry
/// establishing the channel but not querying constraints.
struct BuckdChannel {
    info: DaemonProcessInfo,
    daemon_dir: DaemonDir,
    client: DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
}

impl BuckdChannel {
    /// Upgrade this BuckdChannel to a BootstrapBuckdClient.
    async fn upgrade(self) -> anyhow::Result<BootstrapBuckdClient> {
        let Self {
            info,
            daemon_dir,
            mut client,
        } = self;

        let constraints = get_constraints(&mut client)
            .await
            .context("Error obtaining daemon constraints")?;

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
    ) -> anyhow::Result<Self> {
        let daemon_dir = paths.daemon_dir()?;

        buck2_core::fs::fs_util::create_dir_all(&daemon_dir.path)
            .with_context(|| format!("Error creating daemon dir: {}", daemon_dir))?;

        establish_connection(paths, constraints)
            .await
            .with_context(|| daemon_connect_error(paths))
            .context(
                "Failed to connect to buck daemon. \
                Try running `buck2 clean` and your command afterwards. \
                Alternatively, try running `rm -rf ~/.buck/buckd` and your command afterwards",
            )
    }

    pub fn with_subscribers(
        self,
        subscribers: Vec<Box<dyn EventSubscriber>>,
    ) -> BuckdClientConnector {
        BuckdClientConnector {
            client: BuckdClient {
                info: self.info,
                daemon_dir: self.daemon_dir,
                client: self.client,
                constraints: self.constraints,
                events_ctx: EventsCtx::new(subscribers),
                tailers: None,
            },
        }
    }

    async fn kill_for_constraints_mismatch(&mut self) -> anyhow::Result<()> {
        kill::kill(
            &mut self.client,
            &self.info,
            "client expected different buckd constraints",
        )
        .await
    }
}

/// The settings prior to connecting to the Buck daemon.
/// By default, attempts to connect to a daemon that can satisfy specified constraints.
/// If the daemon does not match constraints (different version or does not enable I/O tracing),
/// it will kill it and restart it with the correct constraints.
/// This behavior can be overridden by calling the `existing_only` method.
/// If the `existing_only` method is called, then any existing buck daemon (regardless of constraint) is accepted.
///
/// The default set of subscribers is *not* empty, but rather forwards stdout and stderr, which captures panics, for example.
pub struct BuckdConnectOptions {
    /// Subscribers manage the way that incoming events from the server are handled.
    /// The client will forward events and stderr/stdout output from the server to each subscriber.
    /// By default, this list is set to a single subscriber that notifies the user of basic output from the server.
    pub(crate) subscribers: Vec<Box<dyn EventSubscriber>>,
    pub constraints: BuckdConnectConstraints,
}

impl BuckdConnectOptions {
    pub fn existing_only_no_console() -> Self {
        Self {
            constraints: BuckdConnectConstraints::ExistingOnly,
            subscribers: vec![Box::new(StdoutStderrForwarder)],
        }
    }

    pub async fn connect(self, paths: &InvocationPaths) -> anyhow::Result<BuckdClientConnector> {
        let client = BootstrapBuckdClient::connect(paths, self.constraints).await?;
        Ok(client.with_subscribers(self.subscribers))
    }
}

async fn establish_connection(
    paths: &InvocationPaths,
    constraints: BuckdConnectConstraints,
) -> anyhow::Result<BootstrapBuckdClient> {
    // There are many places where `establish_connection_inner` may hang.
    // If it does, better print something to the user instead of hanging quietly forever.
    let timeout = buckd_startup_timeout()? * 9;
    let deadline = StartupDeadline::duration_from_now(timeout)?;
    deadline
        .down(
            "establishing connection to Buck daemon or start a daemon",
            |timeout| establish_connection_inner(paths, constraints, timeout),
        )
        .await
}

async fn establish_connection_inner(
    paths: &InvocationPaths,
    constraints: BuckdConnectConstraints,
    deadline: StartupDeadline,
) -> anyhow::Result<BootstrapBuckdClient> {
    if let Some(client) = deadline
        .half()?
        .run("connecting to existing buck daemon", {
            try_connect_existing_before_daemon_restart(paths, &constraints)
        })
        .await?
    {
        return Ok(client);
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
    if let Ok(channel) = try_connect_existing(&paths.daemon_dir()?, &deadline).await {
        let mut client = channel.upgrade().await?;
        if constraints.satisfied(&client.constraints).is_match() {
            return Ok(client);
        }
        deadline
            .run(
                "sending kill command to the Buck daemon",
                client.kill_for_constraints_mismatch(),
            )
            .await?;
    }

    // Daemon dir may be corrupted. Safer to delete it.
    lifecycle_lock
        .clean_daemon_dir()
        .context("Cleaning daemon dir")?;

    // Now there's definitely no server that can be connected to
    // TODO(cjhopman): a non-responsive buckd process may be somehow lingering around and we should probably kill it off here.
    lifecycle_lock.start_server().await?;
    // It might take a little bit for the daemon server to start up. We could wait for the buckd.info
    // file to appear, but it's just as easy to just retry the connection itself.

    let channel = deadline
        .retrying(
            "connect to buckd after server start",
            Duration::from_millis(5),
            Duration::from_millis(100),
            || async { try_connect_existing_impl(&paths.daemon_dir()?).await },
        )
        .await?;

    let client = channel.upgrade().await?;

    match constraints.satisfied(&client.constraints) {
        ConstraintCheckResult::Match => Ok(client),
        ConstraintCheckResult::Mismatch { expected, actual } => {
            Err(BuckdConnectError::BuckDaemonConstraintWrongAfterStart { expected, actual }.into())
        }
    }
}

/// Connect to buckd before attempt to restart the server.
///
/// # Returns
///
/// * `Ok(Some(client))` if we connected to an existing buckd
/// * `Ok(None)` if we failed to connect and should restart buckd
/// * `Err` if we failed to connect and should abandon startup
async fn try_connect_existing_before_daemon_restart(
    paths: &InvocationPaths,
    constraints: &BuckdConnectConstraints,
) -> anyhow::Result<Option<BootstrapBuckdClient>> {
    match try_connect_existing_impl(&paths.daemon_dir()?).await {
        Ok(channel) => {
            let client = channel.upgrade().await?;

            if constraints.satisfied(&client.constraints).is_match() {
                return Ok(Some(client));
            }
            // fallthrough to the more complicated startup case.
        }
        Err(e) if constraints.existing_only() => {
            return Err(e.context("No existing connection and not asked to start one"));
        }
        Err(_) => {
            // fallthrough to the startup case
        }
    }
    Ok(None)
}

async fn try_connect_existing(
    daemon_dir: &DaemonDir,
    timeout: &StartupDeadline,
) -> anyhow::Result<BuckdChannel> {
    timeout
        .min(buckd_startup_timeout()?)?
        .run(
            "connect existing buckd",
            try_connect_existing_impl(daemon_dir),
        )
        .await
}

async fn try_connect_existing_impl(daemon_dir: &DaemonDir) -> anyhow::Result<BuckdChannel> {
    let location = daemon_dir.buckd_info();
    let file = File::open(&location)
        .with_context(|| format!("Trying to open buckd info, `{}`", location.display()))?;
    let reader = BufReader::new(file);
    let info: DaemonProcessInfo = serde_json::from_reader(reader).with_context(|| {
            format!(
                "Parsing daemon info in `{}`. Try deleting that file and running `buck2 killall` before running your command again",
                location.display(),
            )
        })?;

    let connection_type = ConnectionType::parse(&info.endpoint)?;

    let client = new_daemon_api_client(connection_type, info.auth_token.clone())
        .await
        .context("Error connecting")?;

    Ok(BuckdChannel {
        info,
        daemon_dir: daemon_dir.clone(),
        client,
    })
}

async fn get_constraints(
    client: &mut DaemonApiClient<InterceptedService<Channel, BuckAddAuthTokenInterceptor>>,
) -> anyhow::Result<buck2_cli_proto::DaemonConstraints> {
    // NOTE: No tailers in bootstrap client, we capture logs if we fail to connect, but
    // otherwise we leave them alone.
    let status = EventsCtx::new(vec![Box::new(StdoutStderrForwarder)])
        .unpack_oneshot(&mut None, || {
            client.status(tonic::Request::new(buck2_cli_proto::StatusRequest {
                snapshot: false,
            }))
        })
        .await?;

    let status: buck2_cli_proto::StatusResponse = match status {
        CommandOutcome::Success(r) => Ok(r),
        CommandOutcome::Failure(_) => {
            Err(anyhow::anyhow!("Unexpected failure message in status()"))
        }
    }?;

    Ok(status.daemon_constraints.unwrap_or_default())
}

#[derive(Debug, Error)]
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
        "during buck daemon startup, the started process did not match constraints.\nexpected: {expected:?}\nactual: {actual:?}"
    )]
    BuckDaemonConstraintWrongAfterStart {
        expected: DaemonConstraintsRequest,
        actual: buck2_cli_proto::DaemonConstraints,
    },
    #[error("Error connecting to the daemon, daemon stderr follows:\n{stderr}")]
    ConnectError { stderr: String },
}

fn daemon_connect_error(paths: &InvocationPaths) -> BuckdConnectError {
    let stderr = paths
        .daemon_dir()
        .and_then(|dir| {
            let stderr = std::fs::read(dir.buckd_stderr())?;
            Ok(String::from_utf8_lossy(&stderr).into_owned())
        })
        .unwrap_or_else(|_| "<none>".to_owned());

    BuckdConnectError::ConnectError { stderr }
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
        }
    }

    fn request(desired_trace_io_state: DesiredTraceIoState) -> BuckdConnectConstraints {
        BuckdConnectConstraints::Constraints(DaemonConstraintsRequest {
            version: "version".to_owned(),
            user_version: Some("test".to_owned()),
            desired_trace_io_state,
            reject_daemon: None,
            reject_materializer_state: None,
        })
    }

    #[test]
    fn test_constraints_equal_for_existing_only() {
        let req = BuckdConnectConstraints::ExistingOnly;
        let daemon = constraints(true);
        assert!(req.satisfied(&daemon).is_match());
    }

    #[test]
    fn test_constraints_equal_for_same_constraints() {
        let req = request(DesiredTraceIoState::Enabled);
        let daemon = constraints(true);
        assert!(req.satisfied(&daemon).is_match());
    }

    #[test]
    fn test_constraints_equal_for_trace_io_existing() {
        let req = request(DesiredTraceIoState::Existing);
        let daemon = constraints(true);
        assert!(req.satisfied(&daemon).is_match());
    }

    #[test]
    fn test_constraints_unequal_for_trace_io() {
        let req = request(DesiredTraceIoState::Disabled);
        let daemon = constraints(true);
        assert!(!req.satisfied(&daemon).is_match());
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
            reject_materializer_state: None,
        };

        let daemon = buck2_cli_proto::DaemonConstraints {
            version: "foo".to_owned(),
            user_version: None,
            daemon_id: "ddd".to_owned(),
            extra: None,
        };

        assert!(req.satisfied(&daemon));
        req.reject_daemon = Some("zzz".to_owned());
        assert!(req.satisfied(&daemon));
        req.reject_daemon = Some("ddd".to_owned());
        assert!(!req.satisfied(&daemon));
    }

    #[test]
    fn test_reject_materializer_state() {
        let mut req = DaemonConstraintsRequest {
            version: "foo".to_owned(),
            user_version: None,
            desired_trace_io_state: DesiredTraceIoState::Existing,
            reject_daemon: None,
            reject_materializer_state: None,
        };

        let daemon = buck2_cli_proto::DaemonConstraints {
            version: "foo".to_owned(),
            user_version: None,
            daemon_id: "ddd".to_owned(),
            extra: Some(buck2_cli_proto::ExtraDaemonConstraints {
                trace_io_enabled: false,
                materializer_state_identity: Some("mmm".to_owned()),
            }),
        };

        assert!(req.satisfied(&daemon));
        req.reject_materializer_state = Some("zzz".to_owned());
        assert!(req.satisfied(&daemon));
        req.reject_materializer_state = Some("mmm".to_owned());
        assert!(!req.satisfied(&daemon));
    }
}
