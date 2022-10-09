/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_events::dispatch::EventDispatcher;
use buck2_grpc::DuplexChannel;
use buck2_grpc::ServerHandle;
use buck2_test_api::grpc::spawn_orchestrator_server;
use buck2_test_api::grpc::TestExecutorClient;
use derive_more::Display;
use futures::future::try_join3;
use futures::future::Future;
use futures::future::FutureExt;
use gazebo::prelude::*;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::process::Child;

use crate::downward_api::BuckTestDownwardApi;
use crate::orchestrator::BuckTestOrchestrator;

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
    pub dispatcher: EventDispatcher,
}

#[async_trait]
impl ExecutorLauncher for OutOfProcessTestExecutor {
    async fn launch(&self, tpx_args: Vec<String>) -> anyhow::Result<ExecutorLaunch> {
        #[cfg(unix)]
        {
            use buck2_core::env_helper::EnvHelper;

            static BUCK2_TEST_TPX_USE_TCP: EnvHelper<bool> =
                EnvHelper::new("BUCK2_TEST_TPX_USE_TCP");
            let use_tcp = BUCK2_TEST_TPX_USE_TCP.get_copied()?.unwrap_or_default();
            if !use_tcp {
                return spawn_orchestrator(
                    crate::unix::executor::spawn(&self.name, tpx_args).await?,
                    self.dispatcher.dupe(),
                )
                .await;
            }
        }

        spawn_orchestrator(
            crate::tcp::executor::spawn(&self.name, tpx_args).await?,
            self.dispatcher.dupe(),
        )
        .await
    }
}
async fn spawn_orchestrator<T: AsyncRead + AsyncWrite + Send + Sync + Unpin + 'static>(
    (mut executor_proc, executor_client_io, orchestrator_server_io): (Child, T, T),
    dispatcher: EventDispatcher,
) -> anyhow::Result<ExecutorLaunch> {
    let client = TestExecutorClient::new(executor_client_io)
        .await
        .context("Failed to create TestExecutorClient")?;

    let service_task = async move {
        let stdout_fut = read_and_log::read_to_end("stdout", executor_proc.stdout.take());
        let stderr_fut = read_and_log::read_to_end("stderr", executor_proc.stderr.take());

        let (status, stdout, stderr) = try_join3(executor_proc.wait(), stdout_fut, stderr_fut)
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
        spawn_orchestrator_server(
            orchestrator_server_io,
            orchestrator,
            downward_api,
            dispatcher,
        )
    };

    Ok(ExecutorLaunch {
        handle: service_task.boxed(),
        client,
        make_server,
    })
}

mod read_and_log {
    use std::io;

    use tokio::io::AsyncBufReadExt;
    use tokio::io::AsyncRead;
    use tokio::io::BufReader;

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
