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
use std::time::Duration;

use anyhow::Context;
use buck2_common::buckd_connection::ConnectionType;
use buck2_common::client_utils::get_channel_tcp;
use buck2_common::client_utils::get_channel_uds;
use buck2_common::client_utils::retrying;
use buck2_common::client_utils::SOCKET_ADDR;
use buck2_common::daemon_dir::DaemonDir;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_core::env_helper::EnvHelper;
use buck2_core::process::async_background_command;
use cli_proto::daemon_api_client::DaemonApiClient;
use cli_proto::DaemonProcessInfo;
use futures::future::try_join3;
use thiserror::Error;
use tokio::io::AsyncReadExt;
use tokio::time::timeout;
use tonic::transport::Channel;

use crate::daemon::client::BuckdClient;
use crate::daemon::client::BuckdClientConnector;
use crate::daemon::client::BuckdLifecycleLock;
use crate::daemon::client::ClientKind;
use crate::daemon::client::VersionCheckResult;
use crate::daemon::daemon_windows::spawn_background_process_on_windows;
use crate::events_ctx::EventsCtx;
use crate::replayer::Replayer;
use crate::subscribers::stdout_stderr_forwarder::StdoutStderrForwarder;
use crate::subscribers::subscriber::EventSubscriber;

static BUCKD_STARTUP_TIMEOUT: EnvHelper<u64> = EnvHelper::new("BUCKD_STARTUP_TIMEOUT");

pub async fn get_channel(
    endpoint: ConnectionType,
    change_to_parent_dir: bool,
) -> anyhow::Result<Channel> {
    match endpoint {
        ConnectionType::Uds { unix_socket } => {
            get_channel_uds(&unix_socket, change_to_parent_dir).await
        }
        ConnectionType::Tcp { port } => get_channel_tcp(SOCKET_ADDR, port).await,
    }
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
    _lock: BuckdLifecycleLock,
}

impl<'a> BuckdLifecycle<'a> {
    async fn lock_with_timeout(
        paths: &'a InvocationPaths,
        timeout: Duration,
    ) -> anyhow::Result<BuckdLifecycle<'a>> {
        Ok(BuckdLifecycle::<'a> {
            paths,
            _lock: BuckdLifecycleLock::lock_with_timeout(paths.daemon_dir()?, timeout).await?,
        })
    }

    async fn start_server(&self) -> anyhow::Result<()> {
        if cfg!(unix) {
            // On Unix we spawn a process which forks and exits,
            // and here we wait for that spawned process to terminate.

            self.start_server_unix().await
        } else {
            spawn_background_process_on_windows(
                self.paths.project_root().root(),
                &env::current_exe()?,
                [
                    "--isolation-dir",
                    self.paths.isolation.as_str(),
                    "daemon",
                    "--dont-daemonize",
                ],
            )
        }
    }

    async fn start_server_unix(&self) -> anyhow::Result<()> {
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
            // --isolation-dir is an option on the root `buck` cli, not the subcommand.
            .arg("--isolation-dir")
            .arg(self.paths.isolation.as_str())
            .arg("daemon");

        // It is the responsibility of processes that invoke buck2 to indicate via this environment variable whether or
        // not the child process should log to Scribe. The top-level buck2 CLI is invoked via the wrapper, which does
        // this; the `buck2 daemon` command must also be instructed to log to Scribe if the top-level CLI was itself
        // instructed to log.
        // This environment variable ensures that Scribe logging is enabled upon entry of the buck2 daemon command.
        if !buck2_events::sink::scribe::is_enabled() {
            cmd.env("BUCK2_ENABLE_SCRIBE", "0");
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
                        .context("When killing process after buck2 daemon launch timing out")?;
                    // This should return immeditately as kill() waits for the process to end. We wait here again to fetch the ExitStatus
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
                .context("When reading stdout of child")?;
            Ok(buf)
        };
        let stderr_fut = async {
            let mut buf = Vec::new();
            stderr_taken
                .read_to_end(&mut buf)
                .await
                .context("When reading stderr of child")?;
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

/// Type-safe indicator that the buckd client must be given the expected subscribers
/// prior to being used outside of startup.
struct BootstrapBuckdClient(BuckdClientConnector);

impl BootstrapBuckdClient {
    pub fn new(
        client: DaemonApiClient<Channel>,
        info: DaemonProcessInfo,
        daemon_dir: DaemonDir,
    ) -> Self {
        // Start with basic output forwarding to catch any output (usually errors or panics) at startup.
        // This subscriber gets replaced with the actual subscribers once the startup stage of the daemon lifecycle is complete.
        let events_ctx = EventsCtx::new(daemon_dir, vec![box StdoutStderrForwarder]);
        let client = BuckdClient {
            info,
            client: ClientKind::Daemon(client),
            events_ctx,
            tailers: None,
        };
        Self(BuckdClientConnector { client })
    }

    pub fn with_subscribers(
        self,
        subscribers: Vec<Box<dyn EventSubscriber>>,
    ) -> BuckdClientConnector {
        let mut client = self.0;
        client.client.events_ctx.subscribers = subscribers;
        client
    }
}

/// The settings prior to connecting to the Buck daemon.
/// By default, attempts to connect to a daemon with the same version as the client.
/// If the daemon has a different version, it will kill it and restart it with the correct version.
/// This behavior can be overridden by calling the `existing_only` method.
/// If the `existing_only` method is called, then any existing buck daemon (regardless of version) is accepted.
///
/// The default set of subscribers is *not* empty, but rather forwards stdout and stderr, which captures panics, for example.
pub struct BuckdConnectOptions {
    pub existing_only: bool,
    /// Subscribers manage the way that incoming events from the server are handled.
    /// The client will forward events and stderr/stdout output from the server to each subscriber.
    /// By default, this list is set to a single subscriber that notifies the user of basic output from the server.
    pub(crate) subscribers: Vec<Box<dyn EventSubscriber>>,
}

impl BuckdConnectOptions {
    pub fn existing_only_no_console() -> Self {
        Self {
            existing_only: true,
            subscribers: vec![box StdoutStderrForwarder],
        }
    }

    pub async fn connect(self, paths: &InvocationPaths) -> anyhow::Result<BuckdClientConnector> {
        let daemon_dir = paths.daemon_dir()?;
        buck2_core::fs::fs_util::create_dir_all(&daemon_dir.path)
            .with_context(|| format!("When creating daemon dir: {}", daemon_dir))?;
        let client = self
            .establish_connection(paths)
            .await
            .context("When establishing connection to buckd")?;

        // after startup is complete, replace the basic readers with our own.
        Ok(client.with_subscribers(self.subscribers))
    }

    pub fn replay(
        self,
        replayer: Replayer,
        paths: &InvocationPaths,
    ) -> anyhow::Result<BuckdClientConnector> {
        let fake_info = DaemonProcessInfo {
            pid: 0,
            endpoint: "".to_owned(),
            version: "".to_owned(),
        };
        let events_ctx = EventsCtx::new(paths.daemon_dir()?, self.subscribers);
        let client = BuckdClient {
            client: ClientKind::Replayer(Box::pin(replayer)),
            events_ctx,
            info: fake_info,
            tailers: None,
        };

        Ok(BuckdClientConnector { client })
    }

    async fn establish_connection(
        &self,
        paths: &InvocationPaths,
    ) -> anyhow::Result<BootstrapBuckdClient> {
        // There are many places where `establish_connection_inner` may hang.
        // If it does, better print something to the user instead of hanging quietly forever.
        let timeout = buckd_startup_timeout()? * 3;
        match tokio::time::timeout(timeout, self.establish_connection_inner(paths)).await {
            Ok(result) => result,
            Err(_) => Err(anyhow::anyhow!(
                "Failed to establish a connection to the Buck daemon or start a daemon within {}s",
                timeout.as_secs()
            )),
        }
    }

    async fn establish_connection_inner(
        &self,
        paths: &InvocationPaths,
    ) -> anyhow::Result<BootstrapBuckdClient> {
        match self.try_connect_existing(&paths.daemon_dir()?).await {
            Ok(mut client) => {
                if self.existing_only || client.0.client.check_version().await?.is_match() {
                    // either the version matches or we don't care about the version, return the client.
                    return Ok(client);
                }
                // fallthrough to the more complicated startup case.
            }
            Err(e) if self.existing_only => {
                return Err(e.context("No existing connection and not asked to start one"));
            }
            Err(_) => {
                // fallthrough to the startup case
            }
        }

        // At this point, we've either failed to connect to buckd or buckd had the wrong version. At this point,
        // we'll get the lifecycle lock to ensure we don't have races with other processes as we check and change things.

        let lifecycle_lock = BuckdLifecycle::lock_with_timeout(paths, Duration::from_secs(10))
            .await
            .with_context(|| "when locking buckd lifecycle.lock")?;

        // Even if we didn't connect before, it's possible that we just raced with another invocation
        // starting the server, so we try to connect again while holding the lock.
        if let Ok(mut client) = self.try_connect_existing(&paths.daemon_dir()?).await {
            if self.existing_only || client.0.with_flushing().check_version().await?.is_match() {
                // either the version matches or we don't care about the version, return the client.
                return Ok(client);
            }
            client
                .0
                .with_flushing()
                .kill("client expected different buck version")
                .await?;
        }
        // Now there's definitely no server that can be connected to
        // TODO(cjhopman): a non-responsive buckd process may be somehow lingering around and we should probably kill it off here.
        lifecycle_lock.start_server().await?;
        // It might take a little bit for the daemon server to start up. We could wait for the buckd.info
        // file to appear, but it's just as easy to just retry the connection itself.

        let mut client = retrying(
            Duration::from_millis(5),
            Duration::from_millis(100),
            buckd_startup_timeout()?,
            async || {
                self.try_connect_existing(&paths.daemon_dir()?)
                    .await
                    .with_context(|| "Failed to start server")
            },
        )
        .await?;

        if self.existing_only {
            return Ok(client);
        }

        match client.0.with_flushing().check_version().await? {
            VersionCheckResult::Match => Ok(client),
            VersionCheckResult::Mismatch { expected, actual } => {
                Err(BuckdConnectError::BuckDaemonVersionWrongAfterStart { expected, actual }.into())
            }
        }
    }

    async fn try_connect_existing(
        &self,
        daemon_dir: &DaemonDir,
    ) -> anyhow::Result<BootstrapBuckdClient> {
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

        let client = DaemonApiClient::new(get_channel(connection_type, true).await?);

        Ok(BootstrapBuckdClient::new(client, info, daemon_dir.clone()))
    }
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
    #[error("during buck daemon startup, the started process had the wrong version.")]
    BuckDaemonVersionWrongAfterStart { expected: String, actual: String },
}
