/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context;
use buck2_test_api::data::ArgValue;
use buck2_test_api::data::ArgValueContent;
use buck2_test_api::data::ConfiguredTargetHandle;
use buck2_test_api::data::DisplayMetadata;
use buck2_test_api::data::ExecutionResult2;
use buck2_test_api::data::ExecutionStatus;
use buck2_test_api::data::ExternalRunnerSpec;
use buck2_test_api::data::TestResult;
use buck2_test_api::data::TestStatus;
use buck2_test_api::grpc::TestOrchestratorClient;
use buck2_test_api::protocol::TestOrchestrator;
use futures::channel::mpsc::UnboundedReceiver;
use futures::StreamExt;
use host_sharing::HostSharingRequirements;
use parking_lot::Mutex;

const DEFAULT_TEST_TIMEOUT_SECS: u64 = 600;

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
                let name = spec.target.legacy_name.to_owned();
                let target_handle = spec.target.handle.to_owned();
                let result = self
                    .execute_test_from_spec(spec, Duration::from_secs(DEFAULT_TEST_TIMEOUT_SECS))
                    .await
                    .expect("Test execution request failed");
                self.report_test_result(name, target_handle, result)
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

    async fn execute_test_from_spec(
        &self,
        spec: ExternalRunnerSpec,
        timeout: Duration,
    ) -> anyhow::Result<ExecutionResult2> {
        let display_metadata = DisplayMetadata::Testing {
            suite: spec.target.target,
            testcases: Vec::new(),
        };

        let command = spec
            .command
            .into_iter()
            .map(|spec_value| ArgValue {
                content: ArgValueContent::ExternalRunnerSpecValue(spec_value),
                format: None,
            })
            .collect();

        let env = spec
            .env
            .into_iter()
            .map(|(key, value)| {
                (
                    key,
                    ArgValue {
                        content: ArgValueContent::ExternalRunnerSpecValue(value),
                        format: None,
                    },
                )
            })
            .collect();

        let target_handle = spec.target.handle;
        let host_sharing_requirements = HostSharingRequirements::default();
        let pre_create_dirs = Vec::new();
        let executor_override = None;

        self.orchestrator_client
            .execute2(
                display_metadata,
                target_handle,
                command,
                env,
                timeout,
                host_sharing_requirements,
                pre_create_dirs,
                executor_override,
            )
            .await
    }

    async fn report_test_result(
        &self,
        name: String,
        target: ConfiguredTargetHandle,
        execution_result: ExecutionResult2,
    ) -> anyhow::Result<()> {
        let status = match execution_result.status {
            ExecutionStatus::Finished { exitcode } => match exitcode {
                0 => TestStatus::PASS,
                _ => TestStatus::FAIL,
            },
            ExecutionStatus::TimedOut { .. } => TestStatus::TIMEOUT,
        };
        self.orchestrator_client
            .report_test_result(TestResult {
                target,
                name,
                status,
                msg: None,
                duration: Some(execution_result.execution_time),
                details: format!(
                    "---- STDOUT ----\n{:?}\n---- STDERR ----\n{:?}\n",
                    execution_result.stdout, execution_result.stderr
                ),
            })
            .await
    }
}
