/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::ops::ControlFlow;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::calculation::get_target_rule_type_name;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::build::build_configured_label;
use buck2_build_api::build::build_report::build_report_opts;
use buck2_build_api::build::build_report::generate_build_report;
use buck2_build_api::build::BuildConfiguredLabelOptions;
use buck2_build_api::build::BuildEvent;
use buck2_build_api::build::BuildTargetResult;
use buck2_build_api::build::ConfiguredBuildEventVariant;
use buck2_build_api::build::ProvidersToBuild;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::test_provider::TestProvider;
use buck2_build_api::materialize::MaterializationAndUploadContext;
use buck2_cli_proto::representative_config_flag;
use buck2_cli_proto::HasClientContext;
use buck2_cli_proto::TestRequest;
use buck2_cli_proto::TestResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::events::HasEvents;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::liveliness_observer::LivelinessObserverExt;
use buck2_common::liveliness_observer::TimeoutLivelinessObserver;
use buck2_common::pattern::parse_from_cli::parse_patterns_from_cli_args;
use buck2_common::pattern::resolve::ResolveTargetPatterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern::PackageSpec;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::tag_result;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::errors::create_error_report;
use buck2_futures::cancellation::CancellationContext;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter_for_build::interpreter::context::HasInterpreterContext;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::commands::send_target_cfg_event;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::global_cfg_options::global_cfg_options_from_client_context;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_server_ctx::test_command::TEST_COMMAND;
use buck2_test_api::data::TestResult;
use buck2_test_api::data::TestStatus;
use buck2_test_api::protocol::TestExecutor;
use dice::DiceTransaction;
use dice::LinearRecomputeDiceComputations;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::channel::mpsc;
use futures::future;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use indexmap::IndexSet;
use itertools::Itertools;

use crate::downward_api::BuckTestDownwardApi;
use crate::executor_launcher::ExecutorLaunch;
use crate::executor_launcher::ExecutorLauncher;
use crate::executor_launcher::OutOfProcessTestExecutor;
use crate::executor_launcher::TestExecutorClientWrapper;
use crate::local_resource_registry::HasLocalResourceRegistry;
use crate::orchestrator::BuckTestOrchestrator;
use crate::orchestrator::ExecutorMessage;
use crate::session::TestSession;
use crate::session::TestSessionOptions;
use crate::translations::build_configured_target_handle;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = TestExecutor)]
enum TestError {
    #[error("Test execution completed but the tests failed")]
    #[buck2(tag = Input)]
    TestFailed,
    #[error("Test execution completed but tests were skipped")]
    #[buck2(tag = Input)]
    TestSkipped,
    #[error("Test listing failed")]
    #[buck2(tag = Input)]
    ListingFailed,
    #[error("Fatal error encountered during test execution")]
    Fatal,
}

struct TestOutcome {
    errors: Vec<buck2_data::ErrorReport>,
    executor_report: ExecutorReport,
    executor_stdout: String,
    executor_stderr: String,
    build_target_result: BuildTargetResult,
}

impl TestOutcome {
    fn exit_code(&self) -> anyhow::Result<Option<i32>> {
        self.executor_report
            .exit_code
            .context("Test executor did not provide an exit code")
            .map(Some)
    }
}

#[derive(Default)]
struct ExecutorReport {
    exit_code: Option<i32>,
    statuses: TestStatuses,
    info_messages: Vec<String>,
}

impl ExecutorReport {
    fn ingest(&mut self, status: &ExecutorMessage) {
        match status {
            ExecutorMessage::TestResult(res) => {
                self.statuses.ingest(res);
            }
            ExecutorMessage::ExitCode(exit_code) => {
                self.exit_code = Some(*exit_code);
            }
            ExecutorMessage::InfoMessage(message) => {
                self.info_messages.push(message.clone());
            }
        }
    }
}

const MAX_EXAMPLE_VALUES: u64 = 10;
struct CounterWithExamples {
    count: u64,
    max: u64,
    example_tests: Vec<String>,
}

impl CounterWithExamples {
    fn add(&mut self, val: &str) {
        self.count += 1;
        if self.count <= self.max {
            self.example_tests.push(val.to_owned());
        }
    }

    fn to_cli_proto_counter(self) -> buck2_cli_proto::CounterWithExamples {
        buck2_cli_proto::CounterWithExamples {
            count: self.count,
            max: self.max,
            example_tests: self.example_tests,
        }
    }
}
impl Default for CounterWithExamples {
    fn default() -> Self {
        Self {
            count: 0,
            max: MAX_EXAMPLE_VALUES,
            example_tests: vec![],
        }
    }
}

#[derive(Default)]
struct TestStatuses {
    passed: CounterWithExamples,
    skipped: CounterWithExamples,
    failed: CounterWithExamples,
    fatals: CounterWithExamples,
    listing_success: CounterWithExamples,
    listing_failed: CounterWithExamples,
}
impl TestStatuses {
    fn ingest(&mut self, result: &TestResult) {
        match result.status {
            TestStatus::PASS => self.passed.add(&result.name),
            TestStatus::FAIL => self.failed.add(&result.name),
            TestStatus::SKIP => self.skipped.add(&result.name),
            TestStatus::OMITTED => self.skipped.add(&result.name),
            TestStatus::FATAL => self.fatals.add(&result.name),
            TestStatus::TIMEOUT => self.failed.add(&result.name),
            TestStatus::UNKNOWN => {}
            TestStatus::RERUN => {}
            TestStatus::LISTING_SUCCESS => self.listing_success.add(&result.name),
            TestStatus::LISTING_FAILED => self.listing_failed.add(&result.name),
        }
    }
}

#[derive(Debug, buck2_error_derive::Error)]
#[buck2(tag = TestDeadlineExpired)]
#[error("This test run exceeded the deadline that was provided")]
struct DeadlineExpired;

async fn test_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: TestRequest,
) -> buck2_error::Result<TestResponse> {
    run_server_command(TestServerCommand { req }, ctx, partial_result_dispatcher).await
}

pub(crate) fn init_test_command() {
    TEST_COMMAND.init(|ctx, partial_result_dispatcher, req| {
        Box::pin(test_command(ctx, partial_result_dispatcher, req))
    })
}

struct TestServerCommand {
    req: buck2_cli_proto::TestRequest,
}

#[async_trait]
impl ServerCommandTemplate for TestServerCommand {
    type StartEvent = buck2_data::TestCommandStart;
    type EndEvent = buck2_data::TestCommandEnd;
    type Response = buck2_cli_proto::TestResponse;
    type PartialResult = NoPartialResult;

    fn is_success(&self, response: &Self::Response) -> bool {
        matches!(response.exit_code, Some(0)) && response.errors.is_empty()
    }

    fn end_event(&self, _response: &buck2_error::Result<Self::Response>) -> Self::EndEvent {
        buck2_data::TestCommandEnd {
            unresolved_target_patterns: self
                .req
                .target_patterns
                .iter()
                .map(|p| buck2_data::TargetPattern { value: p.clone() })
                .collect(),
        }
    }

    fn additional_telemetry_errors(
        &self,
        response: &Self::Response,
    ) -> Vec<buck2_data::ErrorReport> {
        if let Some(test_status) = &response.test_statuses {
            [
                response.errors.clone(),
                error_report_for_test_errors(response.exit_code, test_status),
            ]
            .concat()
        } else {
            response.errors.clone()
        }
    }
    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> buck2_error::Result<Self::Response> {
        test(server_ctx, ctx, &self.req).await
    }
}

async fn test(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &TestRequest,
) -> buck2_error::Result<TestResponse> {
    // TODO (torozco): Should the --fail-fast flag work here?

    let cwd = server_ctx.working_dir();
    let cell_resolver = ctx.get_cell_resolver().await?;
    let working_dir_cell = cell_resolver.find(cwd)?;

    let client_ctx = request.client_context()?;
    let global_cfg_options = global_cfg_options_from_client_context(
        request
            .target_cfg
            .as_ref()
            .internal_error("target_cfg must be set")?,
        server_ctx,
        &mut ctx,
    )
    .await?;

    // Get the test runner from the config. Note that we use a different key from v1 since the API
    // is completely different, so there is not expectation that the same binary works for both.
    let test_executor_config = ctx
        .get_legacy_config_property(
            cell_resolver.root_cell(),
            BuckconfigKeyRef {
                section: "test",
                property: "v2_test_executor",
            },
        )
        .await?
        .filter(|s| !s.is_empty());

    let (test_executor, test_executor_args) = match test_executor_config {
        Some(config) => {
            let test_executor = post_process_test_executor(config.as_ref())
                .with_context(|| format!("Invalid `test.v2_test_executor`: {}", config))
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Environment))?;
            let mut test_executor_args =
                vec!["--buck-trace-id".to_owned(), client_ctx.trace_id.clone()];
            let platform = match (*ctx)
                .get_interpreter_configuror()
                .await?
                .host_info()
                .platform
            {
                InterpreterHostPlatform::Linux => "linux",
                InterpreterHostPlatform::MacOS => "mac",
                InterpreterHostPlatform::Windows => "windows",
                _ => "",
            };
            test_executor_args.push("--config-entry".to_owned());
            test_executor_args.push(format!("host={}", platform));

            let config_flags = client_ctx
                .representative_config_flags
                .iter()
                .filter_map(|s| {
                    s.source.as_ref().and_then(|source| {
                        if let representative_config_flag::Source::ConfigFlag(s) = source {
                            Some(s)
                        } else {
                            None
                        }
                    })
                })
                .sorted()
                .join(";");

            test_executor_args.push("--config-entry".to_owned());
            test_executor_args.push(format!("config={}", config_flags));

            let flagfiles = client_ctx
                .representative_config_flags
                .iter()
                .filter_map(|s| {
                    s.source.as_ref().and_then(|source| {
                        if let representative_config_flag::Source::ModeFile(s) = source {
                            Some(s)
                        } else {
                            None
                        }
                    })
                })
                .join(";");
            test_executor_args.push("--config-entry".to_owned());
            test_executor_args.push(format!("mode={}", flagfiles));

            (test_executor, test_executor_args)
        }
        None => {
            // If no v2_test_executor config was set, fall back to the internal test runner.
            let test_executor = std::env::current_exe()?;
            let test_executor_args = vec!["internal-test-runner".to_owned()];
            (test_executor, test_executor_args)
        }
    };

    let parsed_patterns =
        parse_patterns_from_cli_args(&mut ctx, &request.target_patterns, cwd).await?;
    server_ctx.log_target_pattern(&parsed_patterns);

    let resolved_pattern = ResolveTargetPatterns::resolve(&mut ctx, &parsed_patterns).await?;

    let launcher: Box<dyn ExecutorLauncher> = Box::new(OutOfProcessTestExecutor {
        executable: test_executor,
        args: test_executor_args,
        dispatcher: ctx.per_transaction_data().get_dispatcher().dupe(),
    });

    let options = request
        .session_options
        .as_ref()
        .context("Missing `options`")
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Input))?;

    let session = TestSession::new(TestSessionOptions {
        allow_re: options.allow_re,
        force_use_project_relative_paths: options.force_use_project_relative_paths,
        force_run_from_project_root: options.force_run_from_project_root,
    });

    let build_opts = request
        .build_opts
        .as_ref()
        .buck_error_context("should have build options")?;

    let timeout = request
        .timeout
        .as_ref()
        .map(|t| t.clone().try_into())
        .transpose()
        .context("Invalid `duration`")
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Input))?;

    let test_outcome = test_targets(
        ctx.dupe(),
        resolved_pattern,
        global_cfg_options,
        request.test_executor_args.clone(),
        Arc::new(TestLabelFiltering::new(
            request.included_labels.clone(),
            request.excluded_labels.clone(),
            request.always_exclude,
            request.build_filtered_targets,
        )),
        &*launcher,
        session,
        cell_resolver.dupe(),
        working_dir_cell,
        build_opts.skip_incompatible_targets,
        MissingTargetBehavior::from_skip(build_opts.skip_missing_targets),
        timeout,
        request.ignore_tests_attribute,
    )
    .await
    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::TestExecutor))?;

    send_target_cfg_event(
        server_ctx.events(),
        test_outcome.build_target_result.configured.keys(),
        &request.target_cfg,
    );

    // TODO(bobyf) remap exit code for buck reserved exit code
    let exit_code = test_outcome
        .exit_code()
        .context("No exit code available")
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::TestExecutor))?;

    let test_statuses = buck2_cli_proto::test_response::TestStatuses {
        passed: Some(
            test_outcome
                .executor_report
                .statuses
                .passed
                .to_cli_proto_counter(),
        ),
        skipped: Some(
            test_outcome
                .executor_report
                .statuses
                .skipped
                .to_cli_proto_counter(),
        ),
        failed: Some(
            test_outcome
                .executor_report
                .statuses
                .failed
                .to_cli_proto_counter(),
        ),
        fatals: Some(
            test_outcome
                .executor_report
                .statuses
                .fatals
                .to_cli_proto_counter(),
        ),
        listing_success: Some(
            test_outcome
                .executor_report
                .statuses
                .listing_success
                .to_cli_proto_counter(),
        ),
        listing_failed: Some(
            test_outcome
                .executor_report
                .statuses
                .listing_failed
                .to_cli_proto_counter(),
        ),
    };

    let serialized_build_report = if build_opts.unstable_print_build_report {
        let artifact_fs = ctx.get_artifact_fs().await?;
        let build_report_opts = build_report_opts(&mut ctx, &cell_resolver, build_opts).await?;

        generate_build_report(
            build_report_opts,
            &artifact_fs,
            &cell_resolver,
            server_ctx.project_root(),
            cwd,
            server_ctx.events().trace_id(),
            &test_outcome.build_target_result.configured,
            &test_outcome.build_target_result.other_errors,
        )?
    } else {
        None
    };

    let mut target_rule_type_names: Vec<String> = Vec::new();
    for configured in test_outcome.build_target_result.configured.keys() {
        target_rule_type_names
            .push(get_target_rule_type_name(&mut ctx, &configured.target()).await?);
    }

    Ok(TestResponse {
        exit_code,
        errors: test_outcome.errors,
        test_statuses: Some(test_statuses),
        executor_stdout: test_outcome.executor_stdout,
        executor_stderr: test_outcome.executor_stderr,
        executor_info_messages: test_outcome.executor_report.info_messages,
        serialized_build_report,
        target_rule_type_names,
    })
}

fn error_report_for_test_errors(
    exit_code: Option<i32>,
    status: &buck2_cli_proto::test_response::TestStatuses,
) -> Vec<buck2_data::ErrorReport> {
    let mut errors = vec![];

    if let Some(failed) = &status.failed {
        if failed.count > 0 {
            errors.push(create_error_report(&buck2_error::Error::from(
                TestError::TestFailed,
            )));
        }
    }
    if let Some(fatal) = &status.fatals {
        if fatal.count > 0 {
            errors.push(create_error_report(&buck2_error::Error::from(
                TestError::Fatal,
            )));
        }
    }
    if let Some(listing_failed) = &status.listing_failed {
        if listing_failed.count > 0 {
            errors.push(create_error_report(&buck2_error::Error::from(
                TestError::ListingFailed,
            )));
        }
    }
    // If a test was skipped due to condition not being met a non-zero exit code will be returned,
    // this doesn't seem quite right, but for now just tag it with TestSkipped to track occurrence.
    if let Some(skipped) = &status.skipped {
        if skipped.count > 0 && exit_code.is_none_or(|code| code != 0) {
            errors.push(create_error_report(&buck2_error::Error::from(
                TestError::TestSkipped,
            )));
        }
    }

    if let Some(code) = exit_code {
        if errors.is_empty() && code != 0 {
            errors.push(create_error_report(&buck2_error::buck2_error!(
                buck2_error::ErrorTag::TestExecutor,
                "Test Executor Failed with exit code {code}"
            )))
        }
    }

    errors
}

async fn test_targets(
    ctx: DiceTransaction,
    pattern: ResolvedPattern<ConfiguredProvidersPatternExtra>,
    global_cfg_options: GlobalCfgOptions,
    external_runner_args: Vec<String>,
    label_filtering: Arc<TestLabelFiltering>,
    launcher: &dyn ExecutorLauncher,
    session: TestSession,
    cell_resolver: CellResolver,
    working_dir_cell: CellName,
    skip_incompatible_targets: bool,
    missing_target_behavior: MissingTargetBehavior,
    timeout: Option<Duration>,
    ignore_tests_attribute: bool,
) -> anyhow::Result<TestOutcome> {
    let session = Arc::new(session);

    let (mut liveliness_observer, _guard) = LivelinessGuard::create();
    let timeout_observer = timeout.map(|timeout| {
        Arc::new(TimeoutLivelinessObserver::new(timeout)) as Arc<dyn LivelinessObserver>
    });
    if let Some(timeout_observer) = &timeout_observer {
        liveliness_observer = Arc::new(liveliness_observer.and(timeout_observer.dupe())) as _;
    }

    let tpx_args = {
        let mut args = vec![
            "ignored".to_owned(),
            "--buck-test-info".to_owned(),
            "ignored".to_owned(),
        ];
        args.extend(external_runner_args);

        args
    };

    let res = launcher
        .launch(tpx_args)
        .await
        .context("Failed to launch executor");

    let res = tag_result!(
        "executor_launch_failed",
        res.map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tpx)),
        quiet: true,
        daemon_in_memory_state_is_corrupted: true,
        task: false
    )?;

    let ExecutorLaunch {
        handle: executor_handle,
        client: test_executor,
        make_server,
    } = res;

    let test_executor = Arc::new(test_executor) as Arc<dyn TestExecutor>;
    let test_executor_wrapper = TestExecutorClientWrapper::new(test_executor.dupe());

    let (test_status_sender, test_status_receiver) = mpsc::unbounded();

    let test_server = tokio::spawn({
        let test_status_sender = test_status_sender.clone();
        let liveliness_observer = liveliness_observer.dupe();
        with_dispatcher_async(
            ctx.per_transaction_data().get_dispatcher().dupe(),
            // NOTE: This is will cancel if the liveliness guard indicates we should.
            async move {
                // Spawn our server to listen to the test runner's requests for execution.

                // Keep wrapper alive for the lifetime of the executor to ensure it stays registered.
                let _test_executor_wrapper = test_executor_wrapper;

                let orchestrator = BuckTestOrchestrator::new(
                    ctx.dupe(),
                    session.dupe(),
                    liveliness_observer.dupe(),
                    test_status_sender,
                    CancellationContext::never_cancelled(), // sending the orchestrator directly to be spawned by make_server, which never calls it.
                )
                .await
                .context("Failed to create a BuckTestOrchestrator")?;

                let server_handle = make_server(orchestrator, BuckTestDownwardApi);

                let mut driver = TestDriver::new(TestDriverState {
                    ctx: &ctx,
                    label_filtering: &label_filtering,
                    global_cfg_options: &global_cfg_options,
                    session: &session,
                    test_executor: &test_executor,
                    cell_resolver: &cell_resolver,
                    working_dir_cell,
                    missing_target_behavior,
                    ignore_tests_attribute,
                });

                driver.push_pattern(
                    pattern.convert_pattern().buck_error_context(
                        "Test with explicit configuration pattern is not supported yet",
                    )?,
                    skip_incompatible_targets,
                );

                {
                    let drive = driver.drive_to_completion();
                    let alive = liveliness_observer.while_alive();
                    futures::pin_mut!(drive);
                    futures::pin_mut!(alive);
                    match futures::future::select(drive, alive).await {
                        futures::future::Either::Left(..) => {}
                        futures::future::Either::Right(..) => {
                            tracing::warn!("Test run was cancelled");
                        }
                    }
                }

                test_executor
                    .end_of_test_requests()
                    .await
                    .context("Failed to notify test executor of end-of-tests")?;

                // Wait for the tests to finish running.
                let test_statuses = test_status_receiver
                    .try_fold(ExecutorReport::default(), |mut acc, result| {
                        acc.ingest(&result);
                        future::ready(Ok(acc))
                    })
                    .await
                    .context("Did not receive all results from executor")?;

                // Shutdown our server. This is technically not *required* since dropping it would shut it
                // down implicitly, but let's do it anyway so we can collect any errors.
                server_handle
                    .shutdown()
                    .await
                    .context("Failed to shutdown orchestrator")?;

                let local_resource_registry = ctx.get_local_resource_registry()?;

                local_resource_registry
                    .release_all_resources()
                    .await
                    .context("Failed to release local resources")?;

                // Process the build errors we've collected.
                let error_stream = futures::stream::iter(driver.error_events);
                let error_target_result = BuildTargetResult::collect_stream(error_stream, false)
                    .await
                    .buck_error_context_anyhow("Failed to collect error events")?;

                driver.build_target_result.extend(error_target_result);

                // And finally return our results;
                anyhow::Ok((driver.build_target_result, test_statuses))
            },
        )
    });

    let executor_output = executor_handle
        .await
        .context("Failed to retrieve executor exit code")?;

    if executor_output.exit_code != 0 {
        return Err(anyhow::Error::msg(executor_output.to_string()));
    }

    // Now that the executor has exited, we notify our results channel. Two things can happen:
    // - The executor has reported end-of-tests. In this case the channel is closed and nothing
    // happened.
    // - The executor had not reported end-of-tests. Assuming we didn't crash on our end (in which
    // case we're about to get this Err out of test_statuses), then this will ensure we don't wait
    // forever on the executor to notify us!
    let _ignored = test_status_sender.unbounded_send(Err(anyhow::Error::msg(
        "Executor exited without reporting end-of-tests",
    )));

    // TODO(bobyf, torozco) we can use cancellation handle here instead of liveliness observer
    let (build_target_result, executor_report) = test_server
        .await
        .context("Failed to collect executor report")??;

    let mut errors = convert_error(&build_target_result)
        .iter()
        .map(create_error_report)
        .unique_by(|e| e.message.clone())
        .collect::<Vec<_>>();

    if let Some(timeout_observer) = timeout_observer {
        if !timeout_observer.is_alive().await {
            errors.push(create_error_report(&DeadlineExpired.into()));
        }
    }

    Ok(TestOutcome {
        errors,
        executor_stdout: executor_output.stdout,
        executor_stderr: executor_output.stderr,
        executor_report,
        build_target_result,
    })
}

enum TestDriverTask {
    InterpretTarget {
        package: PackageLabel,
        spec: PackageSpec<ProvidersPatternExtra>,
        skip_incompatible_targets: bool,
    },
    ConfigureTarget {
        label: ProvidersLabel,
        skippable: bool,
    },
    BuildTarget {
        label: ConfiguredProvidersLabel,
    },
    TestTarget {
        label: ConfiguredProvidersLabel,
        providers: FrozenProviderCollectionValue,
        build_target_result: BuildTargetResult,
    },
}

#[derive(Copy, Clone, Dupe)]
struct TestDriverState<'a, 'e> {
    ctx: &'a DiceTransaction,
    label_filtering: &'a Arc<TestLabelFiltering>,
    global_cfg_options: &'a GlobalCfgOptions,
    session: &'a TestSession,
    test_executor: &'a Arc<dyn TestExecutor + 'e>,
    cell_resolver: &'a CellResolver,
    working_dir_cell: CellName,
    missing_target_behavior: MissingTargetBehavior,
    ignore_tests_attribute: bool,
}

/// Maintains the state of an ongoing test execution.
struct TestDriver<'a, 'e> {
    state: TestDriverState<'a, 'e>,
    work: FuturesUnordered<BoxFuture<'a, ControlFlow<Vec<BuildEvent>, Vec<TestDriverTask>>>>,
    labels_configured: HashSet<(ProvidersLabel, bool)>,
    labels_tested: HashSet<ConfiguredProvidersLabel>,
    error_events: Vec<BuildEvent>,
    build_target_result: BuildTargetResult,
}

impl<'a, 'e> TestDriver<'a, 'e> {
    fn new(state: TestDriverState<'a, 'e>) -> Self {
        Self {
            state,
            work: FuturesUnordered::new(),
            labels_configured: HashSet::new(),
            labels_tested: HashSet::new(),
            error_events: Vec::new(),
            build_target_result: BuildTargetResult::new(),
        }
    }

    /// Add new patterns for the test driver to process.
    fn push_pattern(
        &mut self,
        pattern: ResolvedPattern<ProvidersPatternExtra>,
        skip_incompatible_targets: bool,
    ) {
        for (package, spec) in pattern.specs.into_iter() {
            let fut = future::ready(ControlFlow::Continue(vec![
                TestDriverTask::InterpretTarget {
                    package,
                    spec,
                    skip_incompatible_targets,
                },
            ]))
            .boxed();

            self.work.push(fut);
        }
    }

    /// Drive the test loop until all work is complete.
    async fn drive_to_completion(&mut self) {
        while let Some(tasks) = self.work.next().await {
            match tasks {
                ControlFlow::Continue(tasks) => {
                    for task in tasks {
                        match task {
                            TestDriverTask::InterpretTarget {
                                package,
                                spec,
                                skip_incompatible_targets,
                            } => {
                                self.interpret_targets(package, spec, skip_incompatible_targets);
                            }
                            TestDriverTask::ConfigureTarget { label, skippable } => {
                                self.configure_target(label, skippable);
                            }
                            TestDriverTask::BuildTarget { label } => {
                                self.build_target(label);
                            }
                            TestDriverTask::TestTarget {
                                label,
                                providers,
                                build_target_result,
                            } => {
                                self.test_target(label, providers, build_target_result);
                            }
                        }
                    }
                }
                ControlFlow::Break(events) => self.error_events.extend(events),
            }
        }
    }

    fn interpret_targets(
        &mut self,
        package: PackageLabel,
        spec: PackageSpec<ProvidersPatternExtra>,
        skip_incompatible_targets: bool,
    ) {
        let state = self.state;

        self.work.push(
            async move {
                let res = match state
                    .ctx
                    .clone()
                    .get_interpreter_results(package.dupe())
                    .await
                {
                    Ok(res) => res,
                    Err(e) => {
                        let e: buck2_error::Error = e.into();
                        let mut events = Vec::new();
                        // Try to associate the error to concrete targets, if possible
                        match spec {
                            PackageSpec::Targets(targets) => {
                                for (target, providers) in targets {
                                    let label = Some(ProvidersLabel::new(
                                        TargetLabel::new(package.dupe(), target.as_ref()),
                                        providers.providers,
                                    ));

                                    events.push(BuildEvent::OtherError {
                                        label,
                                        err: e.dupe(),
                                    });
                                }
                            }
                            PackageSpec::All => events.push(BuildEvent::OtherError {
                                label: None,
                                err: e,
                            }),
                        };

                        return ControlFlow::Break(events);
                    }
                };

                // Indicates whether this should be skipped if incompatible.
                let skippable = match spec {
                    PackageSpec::Targets(..) => skip_incompatible_targets,
                    PackageSpec::All => true,
                };

                let (targets, missing) = res.apply_spec(spec);

                if let Some(missing) = missing {
                    match state.missing_target_behavior {
                        MissingTargetBehavior::Fail => {
                            let err = missing.into_errors().0;
                            let events = vec![BuildEvent::OtherError {
                                label: Some(ProvidersLabel::new(
                                    TargetLabel::new(err.package.dupe(), err.target.as_ref()),
                                    buck2_core::provider::label::ProvidersName::Default,
                                )),
                                err: err.into(),
                            }];

                            return ControlFlow::Break(events);
                        }
                        MissingTargetBehavior::Warn => {
                            console_message(missing.missing_targets_warning());
                        }
                    }
                }

                let labels = targets.into_keys().map(|(target_name, providers_pattern)| {
                    providers_pattern.into_providers_label(package.dupe(), target_name.as_ref())
                });

                let work = labels
                    .into_iter()
                    .map(|label| TestDriverTask::ConfigureTarget { label, skippable })
                    .collect();

                ControlFlow::Continue(work)
            }
            .boxed(),
        );
    }

    fn configure_target(&mut self, label: ProvidersLabel, skippable: bool) {
        if !self.labels_configured.insert((label.dupe(), skippable)) {
            return;
        }

        let state = self.state;

        let fut = async move {
            let label = match state
                .ctx
                .clone()
                .get_configured_provider_label(&label, state.global_cfg_options)
                .await
            {
                Ok(label) => label,
                Err(e) => {
                    return ControlFlow::Break(vec![BuildEvent::OtherError {
                        label: Some(label),
                        err: e.into(),
                    }]);
                }
            };

            let node = match state
                .ctx
                .clone()
                .get_configured_target_node(label.target())
                .await
            {
                Ok(node) => node,
                Err(e) => {
                    return ControlFlow::Break(vec![BuildEvent::new_configured(
                        label,
                        ConfiguredBuildEventVariant::Error { err: e.into() },
                    )]);
                }
            };

            let node = match node {
                MaybeCompatible::Incompatible(reason) => {
                    if skippable {
                        eprintln!("{}", reason.skipping_message(label.target()));
                        return ControlFlow::Continue(vec![]);
                    } else {
                        return ControlFlow::Break(vec![BuildEvent::new_configured(
                            label,
                            ConfiguredBuildEventVariant::Error {
                                err: reason.to_err().into(),
                            },
                        )]);
                    }
                }
                MaybeCompatible::Compatible(node) => node,
            };

            // Build and then test this: it's compatible.
            let mut work = vec![TestDriverTask::BuildTarget { label }];

            // If this node is a forward, it'll get flattened when we do analysis and run the
            // test later, but its `tests` attribute here will not be, and that means we'll
            // just ignore it (since we don't traverse `tests` recursively). So ... just
            // flatten it?
            let node = node.forward_target().unwrap_or(&node);

            // Look up `tests` in the the target we're testing, and if we find any tests, add them to the test backlog.
            if !state.ignore_tests_attribute {
                for test in node.tests() {
                    work.push(TestDriverTask::ConfigureTarget {
                        label: test.unconfigured(),
                        // Historically `skippable: false` is what we enforced here, perhaps that
                        // should change.
                        skippable: false,
                    });
                }
            }

            ControlFlow::Continue(work)
        }
        .boxed();

        self.work.push(fut);
    }

    fn build_target(&mut self, label: ConfiguredProvidersLabel) {
        if !self.labels_tested.insert(label.dupe()) {
            return;
        }

        let state = self.state;
        let build_label = label.dupe();
        let fut = async move {
            let ctx = &mut state.ctx.clone();

            let result = match ctx
                .with_linear_recompute(|ctx| async move {
                    build_target_result(&ctx, &state.label_filtering, build_label).await
                })
                .await
            {
                Ok(result) => result,
                Err(e) => {
                    return ControlFlow::Break(vec![BuildEvent::new_configured(
                        label,
                        ConfiguredBuildEventVariant::Error {
                            err: from_any_with_tag(e, buck2_error::ErrorTag::Tier0),
                        },
                    )]);
                }
            };

            ControlFlow::Continue(vec![TestDriverTask::TestTarget {
                label,
                build_target_result: result.0,
                providers: result.1,
            }])
        }
        .boxed();

        self.work.push(fut);
    }

    fn test_target(
        &mut self,
        label: ConfiguredProvidersLabel,
        providers: FrozenProviderCollectionValue,
        build_target_result: BuildTargetResult,
    ) {
        let should_test = !build_target_result.build_failed && !build_target_result.is_empty();
        self.build_target_result.extend(build_target_result);

        // Build has failed, no need to continue with test.
        if !should_test {
            return;
        }

        let state = self.state;
        let fut = async move {
            if let Err(e) = test_target(
                label.dupe(),
                providers,
                state.test_executor.dupe(),
                state.session,
                state.label_filtering.dupe(),
                state.cell_resolver,
                state.working_dir_cell,
            )
            .await
            {
                return ControlFlow::Break(vec![BuildEvent::new_configured(
                    label,
                    ConfiguredBuildEventVariant::Error {
                        err: from_any_with_tag(e, buck2_error::ErrorTag::TestExecutor),
                    },
                )]);
            }

            ControlFlow::Continue(vec![])
        }
        .boxed();

        self.work.push(fut);
    }
}

async fn build_target_result(
    ctx: &LinearRecomputeDiceComputations<'_>,
    label_filtering: &TestLabelFiltering,
    label: ConfiguredProvidersLabel,
) -> anyhow::Result<(BuildTargetResult, FrozenProviderCollectionValue)> {
    // NOTE: We fail if we hit an incompatible target here. This can happen if we reach an
    // incompatible target via `tests = [...]`. This should perhaps change, but that's how it works
    // in v1: https://fb.workplace.com/groups/buckeng/posts/8520953297953210
    let providers = ctx
        .get()
        .get_providers(&label)
        .await?
        .require_compatible()?;
    let collections = providers.provider_collection();

    let build_target_result = match <dyn TestProvider>::from_collection(collections) {
        Some(test_info) => {
            if skip_build_based_on_labels(test_info, label_filtering) {
                return Ok((BuildTargetResult::new(), providers));
            }
            let materialization_and_upload = MaterializationAndUploadContext::skip();
            let stream = build_configured_label(
                &ctx,
                &materialization_and_upload,
                label,
                &ProvidersToBuild {
                    default: false,
                    default_other: false,
                    run: false,
                    tests: true,
                },
                BuildConfiguredLabelOptions {
                    skippable: false,
                    want_configured_graph_size: false,
                },
                None, // TODO: is this right?
            )
            .await
            .map(BuildEvent::Configured);

            BuildTargetResult::collect_stream(stream, false).await?
        }
        None => {
            // not a test
            BuildTargetResult::new()
        }
    };
    Ok((build_target_result, providers))
}

async fn test_target(
    target: ConfiguredProvidersLabel,
    providers: FrozenProviderCollectionValue,
    test_executor: Arc<dyn TestExecutor + '_>,
    session: &TestSession,
    label_filtering: Arc<TestLabelFiltering>,
    cell_resolver: &CellResolver,
    working_dir_cell: CellName,
) -> anyhow::Result<Option<ConfiguredProvidersLabel>> {
    let collection = providers.provider_collection();

    let fut = match <dyn TestProvider>::from_collection(collection) {
        Some(test_info) => {
            if skip_run_based_on_labels(test_info, &label_filtering) {
                return Ok(None);
            }
            run_tests(
                test_executor,
                target,
                test_info,
                session,
                cell_resolver,
                working_dir_cell,
            )
            .map(|l| Some(l).transpose())
            .left_future()
        }
        None => {
            // not a test
            future::ready(Ok(None)).right_future()
        }
    };

    // NOTE: We produce the future above, then await it here because Module & TestInfo aren't Sync.
    // That's OK, because we don't need them across an await point, but we do need to structure the
    // code this way for the compiler to see it.
    fut.await
}

fn convert_error(build_result: &BuildTargetResult) -> Vec<buck2_error::Error> {
    let mut errors = Vec::new();
    errors.extend(build_result.other_errors.values().flatten().duped());

    for v in build_result.configured.values().flatten() {
        errors.extend(v.errors.iter().duped());
        errors.extend(v.outputs.iter().filter_map(|x| x.as_ref().err()).duped());
    }

    errors
}

fn skip_run_based_on_labels(
    provider: &dyn TestProvider,
    label_filtering: &TestLabelFiltering,
) -> bool {
    let target_labels = provider.labels();
    label_filtering.is_excluded(target_labels)
}

fn skip_build_based_on_labels(
    provider: &dyn TestProvider,
    label_filtering: &TestLabelFiltering,
) -> bool {
    !label_filtering.build_filtered_targets && skip_run_based_on_labels(provider, label_filtering)
}

fn run_tests<'a, 'b>(
    test_executor: Arc<dyn TestExecutor + 'a>,
    providers_label: ConfiguredProvidersLabel,
    test_info: &'b dyn TestProvider,
    session: &'b TestSession,
    cell_resolver: &'b CellResolver,
    working_dir_cell: CellName,
) -> BoxFuture<'a, anyhow::Result<ConfiguredProvidersLabel>> {
    let maybe_handle =
        build_configured_target_handle(providers_label.dupe(), session, cell_resolver);

    match maybe_handle {
        Ok(handle) => {
            let fut = test_info.dispatch(handle, test_executor, working_dir_cell);

            (async move {
                fut.await
                    .buck_error_context_anyhow("Failed to notify test executor of a new test")?;
                Ok(providers_label)
            })
            .boxed()
        }
        Err(err) => future::ready(Err(err)).boxed(),
    }
}

struct TestLabelFiltering {
    /// These have the highest order of precedence. Order of precedence within the label is the
    /// iteration order.
    /// A single string label will mean match that label. A `!` prefix turns that label into an
    /// exclusion match.
    /// If positive include label filters are present, then this filter will ONLY match sets of
    /// labels that contains the label filter. Otherwise, if only exclusion filters are present, or
    /// no label filters are present, this will match any set of labels as long as its not excluded.
    included_labels: IndexSet<String>,
    /// Additional excluded labels. These have order of precedence after `included_labels`.
    excluded_labels: IndexSet<String>,
    /// If true, ignores order of precedence such that as long as an exclusion filter matches, we
    /// don't match the set of labels.
    always_exclude: bool,
    /// Whether to build targets that are filtered out, but don't run it.
    build_filtered_targets: bool,
}

impl TestLabelFiltering {
    fn is_excluded(&self, labels: Vec<&str>) -> bool {
        let mut matched = self.included_labels.is_empty();
        for include_label in &self.included_labels {
            if let Some(include) = include_label.strip_prefix('!') {
                // exclusion filters
                if labels.contains(&include) {
                    return true;
                }
            } else {
                // inclusion filters
                if labels.contains(&include_label.as_str()) {
                    if !self.always_exclude {
                        return false;
                    } else {
                        matched = true;
                    }
                }
            }
        }
        for exclude_label in &self.excluded_labels {
            if labels.contains(&exclude_label.as_str()) {
                return true;
            }
        }

        !matched
    }

    fn new(
        included_labels: Vec<String>,
        excluded_labels: Vec<String>,
        always_exclude: bool,
        build_filtered_targets: bool,
    ) -> Self {
        Self {
            included_labels: included_labels.into_iter().collect(),
            excluded_labels: excluded_labels.into_iter().collect(),
            always_exclude,
            build_filtered_targets,
        }
    }
}

fn post_process_test_executor(s: &str) -> anyhow::Result<PathBuf> {
    match s.split_once("$BUCK2_BINARY_DIR/") {
        Some(("", rest)) => {
            let exe =
                AbsPathBuf::new(std::env::current_exe().context("Cannot get Buck2 executable")?)?;
            let exe = fs_util::canonicalize(&exe).buck_error_context_anyhow(
                "Failed to canonicalize path to Buck2 executable. Try running `buck2 kill`.",
            )?;

            let exe = exe.as_abs_path();
            let exe_dir = exe
                .parent()
                .context("Buck2 executable directory has no parent")?;

            Ok(exe_dir.join(rest).to_path_buf())
        }
        Some(..) => Err(anyhow::anyhow!("Invalid value: {}", s)),
        None => Ok(s.into()),
    }
}

#[cfg(test)]
mod tests {
    use crate::command::TestLabelFiltering;

    #[test]
    fn only_include_labels_in_includes() {
        let filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!not_me".to_owned()],
            vec!["this_doesnt_affect_anything".to_owned()],
            false,
            false,
        );

        assert!(!filter.is_excluded(vec!["include_me"]));
        assert!(filter.is_excluded(vec!["not_me"]));
        assert!(filter.is_excluded(vec!["blah"]));
    }

    #[test]
    fn order_of_precedence() {
        let conflicting_filter = TestLabelFiltering::new(
            vec!["!not_me1".to_owned(), "not_me1".to_owned()],
            vec!["not_me2".to_owned()],
            false,
            false,
        );

        assert!(conflicting_filter.is_excluded(vec!["not_me1"]));

        let conflicting_filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!include_me".to_owned()],
            vec!["include_me".to_owned()],
            false,
            false,
        );

        assert!(!conflicting_filter.is_excluded(vec!["include_me"]));

        let conflicting_filter = TestLabelFiltering::new(
            vec!["include_me".to_owned()],
            vec!["!include_me".to_owned()],
            false,
            false,
        );

        assert!(!conflicting_filter.is_excluded(vec!["include_me"]));
    }

    #[test]
    fn always_excludes_overrides_precedence() {
        let filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!not_me1".to_owned()],
            vec!["not_me2".to_owned()],
            true,
            false,
        );

        assert!(!filter.is_excluded(vec!["include_me", "blah"]));
        assert!(filter.is_excluded(vec!["include_me", "not_me1"]));
        assert!(filter.is_excluded(vec!["include_me", "not_me2"]));

        let conflicting_filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!include_me".to_owned()],
            vec!["include_me".to_owned()],
            true,
            false,
        );

        assert!(conflicting_filter.is_excluded(vec!["include_me"]));
    }
}
