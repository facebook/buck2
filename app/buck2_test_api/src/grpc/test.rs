/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context as _;
use assert_matches::assert_matches;
use buck2_grpc::DuplexChannel;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;

use crate::data::ExternalRunnerSpec;
use crate::grpc::spawn_executor_server;
use crate::grpc::TestExecutorClient;
use crate::protocol::TestExecutor;

struct MockExecutor;

#[async_trait::async_trait]
impl TestExecutor for MockExecutor {
    async fn external_runner_spec(&self, _: ExternalRunnerSpec) -> anyhow::Result<()> {
        Ok(())
    }

    async fn end_of_test_requests(&self) -> anyhow::Result<()> {
        Ok(())
    }
}

#[tokio::test]
async fn test_basic() -> anyhow::Result<()> {
    let (client_io, server_io) = tokio::io::duplex(64);
    let server = spawn_executor_server(to_duplex_channel(server_io), MockExecutor);
    let client = TestExecutorClient::new(client_io)
        .await
        .context("Failed to create client")?;

    client.end_of_test_requests().await.context("Call 1")?;
    client.end_of_test_requests().await.context("Call 2")?;
    client.end_of_test_requests().await.context("Call 3")?;

    server.shutdown().await.context("Failed to shutdown")?;

    assert_matches!(client.end_of_test_requests().await, Err(..));

    Ok(())
}

#[tokio::test]
async fn test_client_disconnect() -> anyhow::Result<()> {
    let (client_io, server_io) = tokio::io::duplex(64);
    let mut server =
        spawn_executor_server(to_duplex_channel(server_io), MockExecutor).into_join_handle();

    assert_matches!(
        tokio::time::timeout(Duration::from_millis(10), &mut server).await,
        Err(..)
    );

    drop(client_io);

    assert_matches!(
        tokio::time::timeout(Duration::from_millis(10), &mut server).await,
        Ok(..)
    );

    Ok(())
}

fn to_duplex_channel(
    io: impl AsyncRead + AsyncWrite + Send + Unpin + 'static,
) -> impl AsyncRead + AsyncWrite + Send + Unpin + 'static + tonic::transport::server::Connected {
    let (read, write) = tokio::io::split(io);
    DuplexChannel::new(read, write)
}
