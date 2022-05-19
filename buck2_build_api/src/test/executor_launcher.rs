/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[cfg(unix)]
use std::io;
#[cfg(unix)]
use std::os::unix::io::AsRawFd;
#[cfg(windows)]
use std::os::windows::prelude::AsRawHandle;
use std::{pin::Pin, process::Stdio};

use anyhow::Context as _;
use async_trait::async_trait;
use derive_more::Display;
use futures::future::{try_join3, Future, FutureExt};
use test_api::grpc::{spawn_orchestrator_server, DuplexChannel, ServerHandle, TestExecutorClient};
#[cfg(windows)]
use tokio::net::windows::named_pipe::{ClientOptions, ServerOptions};
#[cfg(unix)]
use tokio::net::UnixStream;
use tokio::process::Command;

use crate::test::{downward_api::BuckTestDownwardApi, orchestrator::BuckTestOrchestrator};

pub struct ExecutorLaunch {
    pub handle: Pin<Box<dyn Future<Output = anyhow::Result<ExecutorOutput>> + Send>>,
    pub client: TestExecutorClient,
    pub make_server:
        Box<dyn FnOnce(BuckTestOrchestrator, BuckTestDownwardApi) -> ServerHandle + Send>,
}

#[derive(Debug, Display)]
#[display(
    fmt = "Test executor exited unexpectedly with status {}.\nStdout:\n{}\nStderr:\n{}",
    exit_code,
    stdout,
    stderr
)]
pub struct ExecutorOutput {
    pub exit_code: i32,
    pub stdout: String,
    pub stderr: String,
}

#[async_trait]
pub trait ExecutorLauncher: Send + Sync {
    async fn launch(&self, tpx_args: Vec<String>) -> anyhow::Result<ExecutorLaunch>;
}

pub struct OutOfProcessTestExecutor {
    pub name: String,
}

#[async_trait]
impl ExecutorLauncher for OutOfProcessTestExecutor {
    async fn launch(&self, tpx_args: Vec<String>) -> anyhow::Result<ExecutorLaunch> {
        let (executor_client_io, executor_server_io);
        let (orchestrator_client_io, orchestrator_server_io);
        let (executor_server_fd, orchestrator_client_fd);
        #[cfg(unix)]
        {
            let (executor_client_async_io, executor_server_async_io) =
                UnixStream::pair().context("Failed to create executor channel")?;

            let (orchestrator_client_async_io, orchestrator_server_async_io) =
                UnixStream::pair().context("Failed to create orchestrator channel")?;

            executor_client_io = executor_client_async_io;
            executor_server_io = executor_server_async_io
                .into_std()
                .context("Failed to convert executor_server_io to std")?;
            executor_server_fd = executor_server_io.as_raw_fd().to_string();

            orchestrator_server_io = orchestrator_server_async_io;
            orchestrator_client_io = orchestrator_client_async_io
                .into_std()
                .context("Failed to convert orchestrator_client_io to std")?;
            orchestrator_client_fd = orchestrator_client_io.as_raw_fd().to_string();
        }
        #[cfg(windows)]
        {
            const EXECUTOR_PIPE_NAME: &str = r"\\.\pipe\buck2-executor-pipe";
            const ORCHESTRATOR_PIPE_NAME: &str = r"\\.\pipe\buck2-orchestrator-pipe";
            executor_server_io = ServerOptions::new()
                .first_pipe_instance(true)
                .create(EXECUTOR_PIPE_NAME)?;
            executor_client_io = ClientOptions::new().open(EXECUTOR_PIPE_NAME)?;
            executor_server_fd = (executor_server_io.as_raw_handle() as usize).to_string();
            executor_server_io.connect().await?;

            orchestrator_server_io = ServerOptions::new()
                .first_pipe_instance(true)
                .create(ORCHESTRATOR_PIPE_NAME)?;
            orchestrator_client_io = ClientOptions::new().open(ORCHESTRATOR_PIPE_NAME)?;
            orchestrator_client_fd = (orchestrator_client_io.as_raw_handle() as usize).to_string();
            orchestrator_server_io.connect().await?;
        }

        let client = TestExecutorClient::new(executor_client_io)
            .await
            .context("Failed to create TestExecutorClient")?;

        let mut command = Command::new(&self.name);
        command
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .arg("--executor-fd")
            .arg(executor_server_fd)
            .arg("--orchestrator-fd")
            .arg(orchestrator_client_fd)
            .arg("--")
            .args(tpx_args);

        #[cfg(unix)]
        {
            let fds = [
                executor_server_io.as_raw_fd(),
                orchestrator_client_io.as_raw_fd(),
            ];

            unsafe {
                command.pre_exec(move || {
                    // Clear CLOEXEC on the 2 FDs we intend to pass. It's better to set CLOEXEC and
                    // clear it specifically here since that ensure that if we spawn anything else
                    // concurrently, those FDs will be released on exec.

                    for fd in &fds {
                        let flags = libc::fcntl(*fd, libc::F_GETFD);
                        if flags < 0 {
                            return Err(io::Error::last_os_error());
                        }

                        if libc::fcntl(*fd, libc::F_SETFD, flags & !libc::FD_CLOEXEC) < 0 {
                            return Err(io::Error::last_os_error());
                        }
                    }

                    Ok(())
                });
            }
        }

        let mut proc = command.spawn().with_context(|| {
            format!("Failed to start {} for OutOfProcessTestExecutor", self.name)
        })?;

        let service_task = async move {
            let stdout_fut = read_and_log::read_to_end("stdout", proc.stdout.take());
            let stderr_fut = read_and_log::read_to_end("stderr", proc.stderr.take());

            let (status, stdout, stderr) = try_join3(proc.wait(), stdout_fut, stderr_fut)
                .await
                .context("Failed to run OutOfProcessTestExecutor")?;

            let exit_code = status.code().unwrap_or(1);

            Ok(ExecutorOutput {
                exit_code,
                stdout,
                stderr,
            })
        };

        let make_server = box move |orchestrator, downward_api| {
            let (read, write) = tokio::io::split(orchestrator_server_io);
            let orchestrator_server_io = DuplexChannel::new(read, write);
            spawn_orchestrator_server(orchestrator_server_io, orchestrator, downward_api)
        };

        Ok(ExecutorLaunch {
            handle: service_task.boxed(),
            client,
            make_server,
        })
    }
}

mod read_and_log {
    use std::io;

    use tokio::io::{AsyncBufReadExt, AsyncRead, BufReader};

    pub async fn read_to_end<A: AsyncRead + Unpin>(
        channel: &str,
        io: Option<A>,
    ) -> io::Result<String> {
        let mut ret = Vec::new();

        if let Some(io) = io {
            let reader = BufReader::new(io);
            let mut lines = reader.lines();

            while let Some(line) = lines.next_line().await? {
                tracing::debug!(channel = channel, "{}", line);
                ret.push(line);
            }
        }

        // Get a trailing newline.
        ret.push("".into());

        Ok(ret.join("\n"))
    }
}
