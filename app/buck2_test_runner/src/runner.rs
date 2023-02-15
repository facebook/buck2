/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_test_api::data::ExternalRunnerSpec;
use buck2_test_api::data::TestResult;
use buck2_test_api::data::TestStatus;
use buck2_test_api::grpc::TestOrchestratorClient;
use buck2_test_api::protocol::TestOrchestrator;
use futures::channel::mpsc::UnboundedReceiver;
use futures::StreamExt;
use parking_lot::Mutex;

pub type SpecReceiver = UnboundedReceiver<ExternalRunnerSpec>;

/// Internal test runner implementation for Buck2.
///
/// This is a basic test runner intended to be used by the open-source Buck2 build
/// if no external test runner is provided. This ensures that `buck2 test` works
/// out-of-the-box for open-source users.
///
/// **This is intended for open-source use only.**
pub struct Buck2TestRunner {
    orchestrator_client: TestOrchestratorClient,
    spec_receiver: Mutex<Option<SpecReceiver>>,
}

impl Buck2TestRunner {
    pub fn new(orchestrator_client: TestOrchestratorClient, spec_receiver: SpecReceiver) -> Self {
        Self {
            orchestrator_client,
            spec_receiver: Mutex::new(Some(spec_receiver)),
        }
    }

    pub async fn run_all_tests(&self) -> anyhow::Result<()> {
        let receiver;
        {
            let mut maybe_receiver = self.spec_receiver.lock();
            receiver = maybe_receiver
                .take()
                .context("Spec channel has already been consumed")?;
            drop(maybe_receiver);
        }
        receiver
            .for_each(async move |spec| {
                // TODO: Execute test spec
                self.report_test_result(spec)
                    .await
                    .expect("Test result reporting failed");
            })
            .await;

        // TODO: Decide if the exit_code for the run should ever be non-zero
        let exit_code = 0;

        self.orchestrator_client
            .end_of_test_results(exit_code)
            .await
    }

    async fn report_test_result(&self, spec: ExternalRunnerSpec) -> anyhow::Result<()> {
        self.orchestrator_client
            .report_test_result(TestResult {
                target: spec.target.handle,
                name: spec.target.legacy_name,
                // TODO: Get status from execution_result
                status: TestStatus::UNKNOWN,
                msg: None,
                // TODO:: Get status from execution_result
                duration: None,
                // TODO: Get details from execution_result
                details: "TODO".to_owned(),
            })
            .await
    }
}
