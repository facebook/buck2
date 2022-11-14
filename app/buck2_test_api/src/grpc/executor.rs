/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context as _;
use buck2_grpc::make_channel;
use buck2_grpc::spawn_oneshot;
use buck2_grpc::to_tonic;
use buck2_grpc::ServerHandle;
use buck2_test_proto::test_executor_client;
use buck2_test_proto::test_executor_server;
use buck2_test_proto::Empty;
use buck2_test_proto::ExternalRunnerSpecRequest;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;
use tonic::transport::Channel;

use crate::data::ExternalRunnerSpec;
use crate::protocol::TestExecutor;

pub struct TestExecutorClient {
    client: test_executor_client::TestExecutorClient<Channel>,
}

impl TestExecutorClient {
    pub async fn new<T>(io: T) -> anyhow::Result<Self>
    where
        T: AsyncRead + AsyncWrite + Send + Sync + Unpin + 'static,
    {
        let channel = make_channel(io, "executor").await?;

        Ok(Self {
            client: test_executor_client::TestExecutorClient::new(channel),
        })
    }
}

#[async_trait::async_trait]
impl TestExecutor for TestExecutorClient {
    async fn external_runner_spec(&self, s: ExternalRunnerSpec) -> anyhow::Result<()> {
        self.client
            .clone()
            .external_runner_spec(ExternalRunnerSpecRequest {
                test_spec: Some(s.try_into().context("Invalid `test_spec`")?),
            })
            .await?;

        Ok(())
    }

    async fn end_of_test_requests(&self) -> anyhow::Result<()> {
        self.client.clone().end_of_test_requests(Empty {}).await?;
        Ok(())
    }
}

pub struct Service<T> {
    inner: T,
}

#[async_trait::async_trait]
impl<T> test_executor_server::TestExecutor for Service<T>
where
    T: TestExecutor + Send + Sync + 'static,
{
    async fn external_runner_spec(
        &self,
        request: tonic::Request<ExternalRunnerSpecRequest>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            let ExternalRunnerSpecRequest { test_spec } = request.into_inner();

            let test_spec = test_spec
                .context("Missing `test_spec`")?
                .try_into()
                .context("Invalid `test_spec`")?;

            self.inner
                .external_runner_spec(test_spec)
                .await
                .context("Failed to dispatch test_spec")?;

            Ok(Empty {})
        })
        .await
    }

    async fn end_of_test_requests(
        &self,
        _: tonic::Request<Empty>,
    ) -> Result<tonic::Response<Empty>, tonic::Status> {
        to_tonic(async move {
            self.inner
                .end_of_test_requests()
                .await
                .context("Failed to report end-of-tests")?;

            Ok(Empty {})
        })
        .await
    }
}

pub fn spawn_executor_server<I, E>(io: I, executor: E) -> ServerHandle
where
    I: AsyncRead + AsyncWrite + Send + Unpin + 'static + tonic::transport::server::Connected,
    E: TestExecutor + Send + Sync + 'static,
{
    let router = tonic::transport::Server::builder().add_service(
        test_executor_server::TestExecutorServer::new(Service { inner: executor }),
    );

    spawn_oneshot(io, router)
}
