/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::test_provider::TestProvider;
use buck2_cli_proto::HasClientContext;
use buck2_cli_proto::TestRequest;
use buck2_cli_proto::TestResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::events::HasEvents;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::PackageSpec;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::tag_result;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_events::dispatch::console_message;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
use buck2_server_ctx::test_command::TEST_COMMAND;
use buck2_test_api::data::TestResult;
use buck2_test_api::data::TestStatus;
use buck2_test_api::protocol::TestExecutor;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::channel::mpsc;
use futures::future;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use indexmap::indexset;
use indexmap::IndexSet;
use more_futures::cancellation::CancellationContext;
use serde::Serialize;

use crate::downward_api::BuckTestDownwardApi;
use crate::executor_launcher::ExecutorLaunch;
use crate::executor_launcher::ExecutorLauncher;
use crate::executor_launcher::OutOfProcessTestExecutor;
use crate::local_resource_registry::LocalResourceRegistry;
use crate::orchestrator::BuckTestOrchestrator;
use crate::orchestrator::ExecutorMessage;
use crate::session::TestSession;
use crate::session::TestSessionOptions;
use crate::translations::build_configured_target_handle;

#[derive(Debug, Serialize)]
pub(crate) struct TestReport {
    project_root: AbsNormPathBuf,
    outputs: HashMap<TargetLabel, Vec<ProjectRelativePathBuf>>,
}

struct TestOutcome {
    error_messages: Vec<String>,
    executor_report: ExecutorReport,
    executor_stdout: String,
    executor_stderr: String,
}

impl TestOutcome {
    pub(crate) fn exit_code(&self) -> anyhow::Result<Option<i32>> {
        if !self.error_messages.is_empty() {
            // Some tests failed to build. Send `None` back to
            // the client to delegate the exit code generation.
            return Ok(None);
        }
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

async fn test_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: TestRequest,
) -> anyhow::Result<TestResponse> {
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
        matches!(response.exit_code, Some(0))
    }

    fn end_event(&self, _response: &anyhow::Result<Self::Response>) -> Self::EndEvent {
        buck2_data::TestCommandEnd {
            unresolved_target_patterns: self.req.target_patterns.clone(),
        }
    }

    async fn command(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        _partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response> {
        test(server_ctx, ctx, &self.req).await
    }
}

async fn test(
    server_ctx: &dyn ServerCommandContextTrait,
    mut ctx: DiceTransaction,
    request: &TestRequest,
) -> anyhow::Result<TestResponse> {
    // TODO (torozco): Should the --fail-fast flag work here?

    let cwd = server_ctx.working_dir();
    let cell_resolver = ctx.get_cell_resolver().await?;
    let working_dir_cell = cell_resolver.find(cwd)?;

    let client_ctx = request.client_context()?;
    let global_target_platform =
        target_platform_from_client_context(client_ctx, server_ctx, &mut ctx).await?;

    // Get the test runner from the config. Note that we use a different key from v1 since the API
    // is completely different, so there is not expectation that the same binary works for both.
    let test_executor_config = ctx
        .get_legacy_config_property(cell_resolver.root_cell(), "test", "v2_test_executor")
        .await?
        .filter(|s| !s.is_empty());

    let (test_executor, test_executor_args) = match test_executor_config {
        Some(config) => {
            let test_executor = post_process_test_executor(config.as_ref())
                .with_context(|| format!("Invalid `test.v2_test_executor`: {}", config))?;
            let test_executor_args = Vec::new();
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

    ctx.per_transaction_data()
        .get_materializer()
        .log_materializer_state(server_ctx.events());

    let resolved_pattern =
        resolve_target_patterns(&cell_resolver, &parsed_patterns, &ctx.file_ops()).await?;

    let launcher: Box<dyn ExecutorLauncher> = Box::new(OutOfProcessTestExecutor {
        executable: test_executor,
        args: test_executor_args,
        dispatcher: ctx.per_transaction_data().get_dispatcher().dupe(),
    });

    let options = request
        .session_options
        .as_ref()
        .context("Missing `options`")?;

    let session = TestSession::new(TestSessionOptions {
        allow_re: options.allow_re,
        force_use_project_relative_paths: options.force_use_project_relative_paths,
        force_run_from_project_root: options.force_run_from_project_root,
    });

    let build_opts = request
        .build_opts
        .as_ref()
        .expect("should have build options");
    let test_outcome = test_targets(
        ctx,
        resolved_pattern,
        global_target_platform,
        request.test_executor_args.clone(),
        Arc::new(TestLabelFiltering::new(
            request.included_labels.clone(),
            request.excluded_labels.clone(),
            request.always_exclude,
            request.build_filtered_targets,
        )),
        &*launcher,
        session,
        cell_resolver,
        working_dir_cell,
        build_opts.skip_incompatible_targets,
        MissingTargetBehavior::from_skip(build_opts.skip_missing_targets),
    )
    .await?;

    // TODO(bobyf) remap exit code for buck reserved exit code
    let exit_code = test_outcome.exit_code().context("No exit code available")?;

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

    Ok(TestResponse {
        exit_code,
        error_messages: test_outcome.error_messages,
        test_statuses: Some(test_statuses),
        executor_stdout: test_outcome.executor_stdout,
        executor_stderr: test_outcome.executor_stderr,
        executor_info_messages: test_outcome.executor_report.info_messages,
    })
}

async fn test_targets(
    ctx: DiceTransaction,
    pattern: ResolvedPattern<ConfiguredProvidersPatternExtra>,
    global_target_platform: Option<TargetLabel>,
    external_runner_args: Vec<String>,
    label_filtering: Arc<TestLabelFiltering>,
    launcher: &dyn ExecutorLauncher,
    session: TestSession,
    cell_resolver: CellResolver,
    working_dir_cell: CellName,
    skip_incompatible_targets: bool,
    missing_target_behavior: MissingTargetBehavior,
) -> anyhow::Result<TestOutcome> {
    let session = Arc::new(session);
    let (liveliness_observer, _guard) = LivelinessGuard::create();

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
        res,
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

    let (test_status_sender, test_status_receiver) = mpsc::unbounded();

    let test_server = tokio::spawn({
        let test_status_sender = test_status_sender.clone();
        with_dispatcher_async(
            ctx.per_transaction_data().get_dispatcher().dupe(),
            // NOTE: This is will cancel if the liveliness guard indicates we should.
            async move {
                // Spawn our server to listen to the test runner's requests for execution.

                let local_resource_registry = Arc::new(LocalResourceRegistry::new());

                let orchestrator = BuckTestOrchestrator::new(
                    ctx.dupe(),
                    session.dupe(),
                    liveliness_observer.dupe(),
                    test_status_sender,
                    CancellationContext::never_cancelled(), // sending the orchestrator directly to be spawned by make_server, which never calls it.
                    local_resource_registry.dupe(),
                )
                .await
                .context("Failed to create a BuckTestOrchestrator")?;

                let server_handle = make_server(orchestrator, BuckTestDownwardApi);

                let resolve_tests_platform_independently = ctx
                    .parse_legacy_config_property(
                        cell_resolver.root_cell(),
                        "buck2",
                        "independent_tests_platform_resolution",
                    )
                    .await?
                    .unwrap_or(false);

                let mut driver = TestDriver::new(TestDriverState {
                    ctx: &ctx,
                    label_filtering: &label_filtering,
                    global_target_platform: &global_target_platform,
                    session: &session,
                    test_executor: &test_executor,
                    cell_resolver: &cell_resolver,
                    working_dir_cell,
                    resolve_tests_platform_independently,
                    missing_target_behavior,
                });

                driver.push_pattern(
                    pattern
                        .convert_pattern()
                        .context("Test with explicit configuration pattern is not supported yet")?,
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

                local_resource_registry
                    .release_all_resources()
                    .await
                    .context("Failed to release local resources")?;

                // And finally return our results;

                anyhow::Ok((driver.build_errors, test_statuses))
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
    let (build_errors, executor_report) = test_server
        .await
        .context("Failed to collect executor report")??;

    Ok(TestOutcome {
        error_messages: build_errors,
        executor_stdout: executor_output.stdout,
        executor_stderr: executor_output.stderr,
        executor_report,
    })
}

enum TestDriverTask {
    InterpretTarget {
        package: PackageLabel,
        spec: PackageSpec<ProvidersPatternExtra>,
        skip_incompatible_targets: bool,
    },
    ConfigureTargets {
        labels: Vec<ProvidersLabel>,
        skippable: bool,
    },
    TestTargets {
        labels: Vec<ConfiguredProvidersLabel>,
    },
    Done,
}

#[derive(Copy, Clone, Dupe)]
pub(crate) struct TestDriverState<'a, 'e> {
    ctx: &'a DiceComputations,
    label_filtering: &'a Arc<TestLabelFiltering>,
    global_target_platform: &'a Option<TargetLabel>,
    session: &'a TestSession,
    test_executor: &'a Arc<dyn TestExecutor + 'e>,
    cell_resolver: &'a CellResolver,
    working_dir_cell: CellName,
    resolve_tests_platform_independently: bool,
    missing_target_behavior: MissingTargetBehavior,
}

/// Maintains the state of an ongoing test execution.
struct TestDriver<'a, 'e> {
    state: TestDriverState<'a, 'e>,
    work: FuturesUnordered<BoxFuture<'a, anyhow::Result<TestDriverTask>>>,
    labels_seen: HashSet<ConfiguredProvidersLabel>,
    build_errors: Vec<String>,
}

impl<'a, 'e> TestDriver<'a, 'e> {
    pub(crate) fn new(state: TestDriverState<'a, 'e>) -> Self {
        Self {
            state,
            work: FuturesUnordered::new(),
            labels_seen: HashSet::new(),
            build_errors: Vec::new(),
        }
    }

    /// Add new patterns for the test driver to process.
    fn push_pattern(
        &mut self,
        pattern: ResolvedPattern<ProvidersPatternExtra>,
        skip_incompatible_targets: bool,
    ) {
        for (package, spec) in pattern.specs.into_iter() {
            let fut = future::ready(anyhow::Ok(TestDriverTask::InterpretTarget {
                package,
                spec,
                skip_incompatible_targets,
            }))
            .boxed();

            self.work.push(fut);
        }
    }

    /// Drive the test loop until all work is complete.
    async fn drive_to_completion(&mut self) {
        while let Some(task) = self.work.next().await {
            match task {
                Ok(TestDriverTask::InterpretTarget {
                    package,
                    spec,
                    skip_incompatible_targets,
                }) => {
                    self.interpret_targets(package, spec, skip_incompatible_targets);
                }
                Ok(TestDriverTask::ConfigureTargets { labels, skippable }) => {
                    self.configure_targets(labels, skippable);
                }
                Ok(TestDriverTask::TestTargets { labels }) => {
                    self.test_targets(labels);
                }
                Ok(TestDriverTask::Done) => {
                    // Nothing to do here
                }
                Err(e) => {
                    // TODO(brasselsprouts): filter out duplicate errors.
                    self.build_errors.push(format!("{:#}", e));
                }
            };
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
                let res = state.ctx.get_interpreter_results(package.dupe()).await?;
                let SpecTargets { labels, skippable } = spec_to_targets(
                    spec,
                    res,
                    skip_incompatible_targets,
                    state.missing_target_behavior,
                )?;

                let labels = labels.into_map(|(target_name, providers_pattern)| {
                    providers_pattern.into_providers_label(package.dupe(), target_name.as_ref())
                });

                anyhow::Ok(TestDriverTask::ConfigureTargets { labels, skippable })
            }
            .boxed(),
        );
    }

    fn configure_targets(&mut self, labels: Vec<ProvidersLabel>, skippable: bool) {
        let state = self.state;

        self.work.extend(labels.into_iter().map(|label| {
            async move {
                let label = state
                    .ctx
                    .get_configured_provider_label(&label, state.global_target_platform.as_ref())
                    .await?;

                let node = state.ctx.get_configured_target_node(label.target()).await?;

                let node = match node {
                    MaybeCompatible::Incompatible(reason) => {
                        if skippable {
                            eprintln!("{}", reason.skipping_message(label.target()));
                            return Ok(TestDriverTask::Done);
                        } else {
                            return Err(reason.to_err());
                        }
                    }
                    MaybeCompatible::Compatible(node) => node,
                };

                // If this node is a forward, it'll get flattened when we do analysis and run the
                // test later, but its `tests` attribute here will not be, and that means we'll
                // just ignore it (since we don't traverse `tests` recursively). So ... just
                // flatten it?
                let node = node.forward_target().unwrap_or(&node);

                // Test this, it's compatible.
                let mut labels = vec![label];

                // Look up `tests` in the the target we're testing, and if we find any tests, add them to the test backlog.
                for test in node.tests() {
                    if state.resolve_tests_platform_independently {
                        let label = state
                            .ctx
                            .get_configured_provider_label(
                                &test.unconfigured(),
                                state.global_target_platform.as_ref(),
                            )
                            .await?;
                        labels.push(label);
                    } else {
                        labels.push(test);
                    }
                }

                anyhow::Ok(TestDriverTask::TestTargets { labels })
            }
            .boxed()
        }));
    }

    fn test_targets(&mut self, labels: Vec<ConfiguredProvidersLabel>) {
        self.work.extend(labels.into_iter().filter_map(|label| {
            if !self.labels_seen.insert(label.clone()) {
                return None;
            }

            let state = self.state;

            let fut = async move {
                test_target(
                    state.ctx,
                    label,
                    state.test_executor.dupe(),
                    state.session,
                    state.label_filtering.dupe(),
                    state.cell_resolver,
                    state.working_dir_cell,
                )
                .await?;

                anyhow::Ok(TestDriverTask::Done)
            }
            .boxed();

            Some(fut)
        }));
    }
}

struct SpecTargets {
    labels: Vec<(TargetName, ProvidersPatternExtra)>,
    /// Indicates whether this should be skipped if incompatible.
    skippable: bool,
}

fn spec_to_targets(
    spec: PackageSpec<ProvidersPatternExtra>,
    res: Arc<EvaluationResult>,
    skip_incompatible_targets: bool,
    missing_target_behavior: MissingTargetBehavior,
) -> anyhow::Result<SpecTargets> {
    let skippable = match spec {
        PackageSpec::Targets(..) => skip_incompatible_targets,
        PackageSpec::All => true,
    };

    let (targets, missing) = res.apply_spec(spec);

    if let Some(missing) = missing {
        match missing_target_behavior {
            MissingTargetBehavior::Fail => {
                return Err(missing.into_error());
            }
            MissingTargetBehavior::Warn => {
                console_message(missing.missing_targets_warning());
            }
        }
    }

    Ok(SpecTargets {
        labels: targets.into_keys().collect(),
        skippable,
    })
}

async fn test_target(
    ctx: &DiceComputations,
    target: ConfiguredProvidersLabel,
    test_executor: Arc<dyn TestExecutor + '_>,
    session: &TestSession,
    label_filtering: Arc<TestLabelFiltering>,
    cell_resolver: &CellResolver,
    working_dir_cell: CellName,
) -> anyhow::Result<Option<ConfiguredProvidersLabel>> {
    // NOTE: We fail if we hit an incompatible target here. This can happen if we reach an
    // incompatible target via `tests = [...]`. This should perhaps change, but that's how it works
    // in v1: https://fb.workplace.com/groups/buckeng/posts/8520953297953210
    let frozen_providers = ctx.get_providers(&target).await?.require_compatible()?;
    let providers = frozen_providers.provider_collection();
    build_artifacts(ctx, providers, &label_filtering).await?;

    let fut = match <dyn TestProvider>::from_collection(providers) {
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

async fn build_artifacts(
    ctx: &DiceComputations,
    providers: &FrozenProviderCollection,
    label_filtering: &TestLabelFiltering,
) -> anyhow::Result<()> {
    fn get_artifacts_to_build(
        label_filtering: &TestLabelFiltering,
        providers: &FrozenProviderCollection,
    ) -> anyhow::Result<IndexSet<ArtifactGroup>> {
        Ok(match <dyn TestProvider>::from_collection(providers) {
            Some(provider) => {
                if skip_build_based_on_labels(provider, label_filtering) {
                    return Ok(indexset![]);
                }
                let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                provider.visit_artifacts(&mut artifact_visitor)?;
                artifact_visitor.inputs
            }
            None => {
                // not a test
                indexset![]
            }
        })
    }
    let artifacts_to_build = get_artifacts_to_build(label_filtering, providers)?;
    // build the test target first
    future::try_join_all(
        artifacts_to_build
            .iter()
            .map(|input| ctx.ensure_artifact_group(input)),
    )
    .await?;
    Ok(())
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
        build_configured_target_handle(providers_label.clone(), session, cell_resolver);

    match maybe_handle {
        Ok(handle) => {
            let fut = test_info.dispatch(handle, test_executor, working_dir_cell);

            (async move {
                fut.await
                    .context("Failed to notify test executor of a new test")?;
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
            let exe = std::env::current_exe().context("Cannot get Buck2 executable")?;
            let exe = fs_util::canonicalize(exe)
                .context("Failed to canonicalize path to Buck2 executable")?;
            let exe = exe.as_abs_path();
            let mut exe_dir = exe
                .parent()
                .context("Buck2 executable directory has no parent")?;

            // We allow overriding the dir here. This is used for buck2.sh
            static BINARY_DIR_RELATIVE_OVERRIDE: EnvHelper<PathBuf> =
                EnvHelper::new("BUCK2_BINARY_DIR_RELATIVE_TO");

            let overridden;

            if let Some(v) = BINARY_DIR_RELATIVE_OVERRIDE.get()? {
                overridden = exe_dir.join(v.as_path());
                exe_dir = &overridden;
            }

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
