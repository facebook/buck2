/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_test_api::data::ArgValue;
use buck2_test_api::data::ArgValueContent;
use buck2_test_api::data::ConfiguredTargetHandle;
use buck2_test_api::data::DisplayMetadata;
use buck2_test_api::data::ExecutionResult2;
use buck2_test_api::data::ExecutionStatus;
use buck2_test_api::data::ExternalRunnerSpec;
use buck2_test_api::data::ExternalRunnerSpecValue;
use buck2_test_api::data::RequiredLocalResources;
use buck2_test_api::data::TestResult;
use buck2_test_api::data::TestStatus;
use buck2_test_api::grpc::TestOrchestratorClient;
use buck2_test_api::protocol::TestOrchestrator;
use clap::Parser;
use futures::channel::mpsc::UnboundedReceiver;
use futures::StreamExt;
use host_sharing::HostSharingRequirements;
use parking_lot::Mutex;

use crate::config::Config;
use crate::config::EnvValue;

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
    config: Config,
}

impl Buck2TestRunner {
    pub fn new(
        orchestrator_client: TestOrchestratorClient,
        spec_receiver: SpecReceiver,
        args: Vec<String>,
    ) -> anyhow::Result<Self> {
        let config = Config::try_parse_from(args).context("Error parsing test runner arguments")?;
        Ok(Self {
            orchestrator_client,
            spec_receiver: Mutex::new(Some(spec_receiver)),
            config,
        })
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
        let run_verdict = receiver
            .map(async move |spec| {
                let name = format!(
                    "{}//{}:{}",
                    spec.target.cell, spec.target.package, spec.target.target
                );
                let target_handle = spec.target.handle.to_owned();

                let execution_result = self
                    .execute_test_from_spec(spec)
                    .await
                    .expect("Test execution request failed");

                let test_result = get_test_result(name, target_handle, execution_result);
                let test_status = test_result.status.clone();

                self.report_test_result(test_result)
                    .await
                    .expect("Test result reporting failed");

                test_status
            })
            // Use an arbitrarily large buffer -- execution throttling will be handled by the Buck2
            // executor, so no need to hold back on requests here.
            .buffer_unordered(10000)
            // If any individual test failed, consider the entire run to have failed.
            .fold(
                RunVerdict::Pass,
                async move |mut run_verdict, test_status| {
                    if test_status != TestStatus::PASS {
                        run_verdict = RunVerdict::Fail;
                    }
                    run_verdict
                },
            )
            .await;

        self.orchestrator_client
            .end_of_test_results(run_verdict.exit_code())
            .await
    }

    async fn execute_test_from_spec(
        &self,
        spec: ExternalRunnerSpec,
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

        let config_env = self.config.env.iter().map(|EnvValue { name, value }| {
            (
                name.to_owned(),
                ArgValue {
                    content: ArgValueContent::ExternalRunnerSpecValue(
                        ExternalRunnerSpecValue::Verbatim(value.to_owned()),
                    ),
                    format: None,
                },
            )
        });

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
            .chain(config_env)
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
                self.config.timeout,
                host_sharing_requirements,
                pre_create_dirs,
                executor_override,
                RequiredLocalResources { resources: vec![] },
            )
            .await
    }

    async fn report_test_result(&self, test_result: TestResult) -> anyhow::Result<()> {
        self.orchestrator_client
            .report_test_result(test_result)
            .await
    }
}

fn get_test_result(
    name: String,
    target: ConfiguredTargetHandle,
    execution_result: ExecutionResult2,
) -> TestResult {
    let status = match execution_result.status {
        ExecutionStatus::Finished { exitcode } => match exitcode {
            0 => TestStatus::PASS,
            _ => TestStatus::FAIL,
        },
        ExecutionStatus::TimedOut { .. } => TestStatus::TIMEOUT,
    };
    TestResult {
        target,
        name,
        status,
        msg: None,
        duration: Some(execution_result.execution_time),
        details: format!(
            "---- STDOUT ----\n{:?}\n---- STDERR ----\n{:?}\n",
            execution_result.stdout, execution_result.stderr
        ),
    }
}

#[derive(Debug)]
enum RunVerdict {
    Pass,
    Fail,
}

impl RunVerdict {
    fn exit_code(&self) -> i32 {
        match self {
            RunVerdict::Pass => 0,
            RunVerdict::Fail => 32,
        }
    }
}
