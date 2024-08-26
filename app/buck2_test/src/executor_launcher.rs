/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::path::PathBuf;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::Mutex;
use std::task::Context;
use std::task::Poll;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::buck2_env;
use buck2_events::dispatch::EventDispatcher;
use buck2_grpc::DuplexChannel;
use buck2_grpc::ServerHandle;
use buck2_test_api::grpc::spawn_orchestrator_server;
use buck2_test_api::grpc::TestExecutorClient;
use buck2_test_api::protocol::TestExecutor;
use derive_more::Display;
use dupe::Dupe;
use futures::future::try_join3;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use once_cell::sync::Lazy;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tokio::process::Child;

use crate::downward_api::BuckTestDownwardApi;
use crate::orchestrator::BuckTestOrchestrator;

static TEST_EXECUTOR_CLIENTS: Lazy<Mutex<HashMap<u16, Arc<dyn TestExecutor>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

pub struct TestExecutorClientWrapper(u16);
impl TestExecutorClientWrapper {
    pub fn new(client: Arc<dyn TestExecutor>) -> Self {
        let mut clients = TEST_EXECUTOR_CLIENTS.lock().unwrap();
        let id = clients.keys().max().unwrap_or(&0) + 1;
        tracing::debug!(id = id, "Adding test executor");
        clients.insert(id, client);
        Self(id)
    }
}
impl Drop for TestExecutorClientWrapper {
    fn drop(&mut self) {
        tracing::debug!(id = self.0, "Removing test executor");
        TEST_EXECUTOR_CLIENTS.lock().unwrap().remove(&self.0);
    }
}

pub fn get_all_test_executors() -> Vec<Arc<dyn TestExecutor>> {
    TEST_EXECUTOR_CLIENTS
        .lock()
        .unwrap()
        .iter()
        .map(|(_, exe)| exe.clone())
        .collect()
}

pub struct ExecutorLaunch {
    pub handle: ExecutorFuture,
    pub client: TestExecutorClient,
    pub make_server:
        Box<dyn FnOnce(BuckTestOrchestrator<'static>, BuckTestDownwardApi) -> ServerHandle + Send>,
}

pub struct ExecutorFuture {
    fut: BoxFuture<'static, anyhow::Result<ExecutorOutput>>,
}

impl ExecutorFuture {
    pub(crate) fn new(mut child: Child) -> Self {
        let fut = async move {
            let stdout_fut = read_and_log::read_to_end("stdout", child.stdout.take());
            let stderr_fut = read_and_log::read_to_end("stderr", child.stderr.take());

            let (status, stdout, stderr) = try_join3(child.wait(), stdout_fut, stderr_fut)
                .await
                .context("Failed to run OutOfProcessTestExecutor")?;

            let exit_code = status.code().unwrap_or(1);

            Ok(ExecutorOutput {
                exit_code,
                stdout,
                stderr,
            })
        };

        Self { fut: fut.boxed() }
    }
}

impl Future for ExecutorFuture {
    type Output = anyhow::Result<ExecutorOutput>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.fut.poll_unpin(cx)
    }
}

#[derive(Debug, Display)]
#[display(
    "Test executor exited unexpectedly with status {}.\nStdout:\n{}\nStderr:\n{}",
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
    pub executable: PathBuf,
    pub args: Vec<String>,
    pub dispatcher: EventDispatcher,
}

#[async_trait]
impl ExecutorLauncher for OutOfProcessTestExecutor {
    async fn launch(&self, tpx_args: Vec<String>) -> anyhow::Result<ExecutorLaunch> {
        // Declare outside of `cfg(unix)` so `buck2 help-env` would include it on Windows
        // even if it is no-op on Windows.
        let use_tcp = buck2_env!("BUCK2_TEST_TPX_USE_TCP", bool)?;

        if !use_tcp {
            #[cfg(unix)]
            {
                return spawn_orchestrator(
                    crate::unix::executor::spawn(
                        self.executable.as_ref(),
                        self.args.clone(),
                        tpx_args,
                    )
                    .await?,
                    self.dispatcher.dupe(),
                )
                .await;
            }
        }

        spawn_orchestrator(
            crate::tcp::executor::spawn(self.executable.as_ref(), self.args.clone(), tpx_args)
                .await?,
            self.dispatcher.dupe(),
        )
        .await
    }
}
async fn spawn_orchestrator<T: AsyncRead + AsyncWrite + Send + Sync + Unpin + 'static>(
    (handle, executor_client_io, orchestrator_server_io): (ExecutorFuture, T, T),
    dispatcher: EventDispatcher,
) -> anyhow::Result<ExecutorLaunch> {
    let client = TestExecutorClient::new(executor_client_io)
        .await
        .context("Failed to create TestExecutorClient")?;

    let make_server = Box::new(move |orchestrator, downward_api| {
        let (read, write) = tokio::io::split(orchestrator_server_io);
        let orchestrator_server_io = DuplexChannel::new(read, write);
        spawn_orchestrator_server(
            orchestrator_server_io,
            orchestrator,
            downward_api,
            dispatcher,
        )
    });

    Ok(ExecutorLaunch {
        handle,
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
