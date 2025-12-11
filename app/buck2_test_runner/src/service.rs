/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_error::BuckErrorContext;
use buck2_grpc::DuplexChannel;
use buck2_test_api::grpc::TestOrchestratorClient;
use buck2_test_api::grpc::spawn_executor_server;
use tokio::io::AsyncRead;
use tokio::io::AsyncWrite;

use crate::executor::Buck2TestExecutor;
use crate::runner::Buck2TestRunner;

pub async fn run<OC, ER, EW>(
    orchestrator_channel: OC,
    executor_channel: DuplexChannel<ER, EW>,
    args: Vec<String>,
) -> buck2_error::Result<()>
where
    OC: AsyncRead + AsyncWrite + Unpin + Send + Sync + 'static,
    ER: AsyncRead + Send + Unpin + 'static,
    EW: AsyncWrite + Send + Unpin + 'static,
{
    let (spec_sender, spec_receiver) = futures::channel::mpsc::unbounded();

    let executor_server =
        spawn_executor_server(executor_channel, Buck2TestExecutor::new(spec_sender));

    let orchestrator_client = TestOrchestratorClient::new(orchestrator_channel)
        .await
        .buck_error_context("Failed to TestOrchestratorClient")?;

    let runner = Buck2TestRunner::new(orchestrator_client, spec_receiver, args)?;

    runner.run_all_tests().await?;

    executor_server
        .shutdown()
        .await
        .buck_error_context("Failed to shutdown server")?;

    Ok(())
}
