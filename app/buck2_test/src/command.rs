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
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::test_provider::TestProvider;
use buck2_build_api::nodes::calculation::NodeCalculation;
use buck2_cli_proto::HasClientContext;
use buck2_cli_proto::TestRequest;
use buck2_cli_proto::TestResponse;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::events::HasEvents;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::liveliness_observer::LivelinessGuard;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::cells::name::CellName;
use buck2_core::cells::CellResolver;
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
use buck2_core::provider::label::ProvidersName;
use buck2_core::quiet_soft_error;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_query::query::compatibility::MaybeCompatible;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::NoPartialResult;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::pattern::parse_patterns_from_cli_args;
use buck2_server_ctx::pattern::target_platform_from_client_context;
use buck2_server_ctx::template::run_server_command;
use buck2_server_ctx::template::ServerCommandTemplate;
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
use indexmap::IndexMap;
use indexmap::IndexSet;
use more_futures::cancellable_future::critical_section;
use serde::Serialize;

use crate::downward_api::BuckTestDownwardApi;
use crate::executor_launcher::ExecutorLaunch;
use crate::executor_launcher::ExecutorLauncher;
use crate::executor_launcher::OutOfProcessTestExecutor;
use crate::orchestrator::BuckTestOrchestrator;
use crate::orchestrator::TestResultOrExitCode;
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
}

impl ExecutorReport {
    fn ingest(&mut self, status: &TestResultOrExitCode) {
        match status {
            TestResultOrExitCode::TestResult(res) => {
                self.statuses.ingest(res);
            }
            TestResultOrExitCode::ExitCode(exit_code) => {
                self.exit_code = Some(*exit_code);
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

pub async fn test_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
    req: TestRequest,
) -> anyhow::Result<TestResponse> {
    run_server_command(TestServerCommand { req }, ctx, partial_result_dispatcher).await
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
    ctx: DiceTransaction,
    request: &TestRequest,
) -> anyhow::Result<TestResponse> {
    let cwd = server_ctx.working_dir();
    let cell_resolver = ctx.get_cell_resolver().await?;
    let working_dir_cell = cell_resolver.find(cwd)?;

    let client_ctx = request.client_context()?;
    let global_target_platform =
        target_platform_from_client_context(client_ctx, server_ctx, &ctx).await?;

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

    let parsed_patterns = parse_patterns_from_cli_args(&ctx, &request.target_patterns, cwd).await?;
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

    let test_outcome = test_targets(
        &ctx,
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
    })
}

async fn test_targets(
    ctx: &DiceComputations,
    pattern: ResolvedPattern<ConfiguredProvidersPatternExtra>,
    global_target_platform: Option<TargetLabel>,
    external_runner_args: Vec<String>,
    label_filtering: Arc<TestLabelFiltering>,
    launcher: &dyn ExecutorLauncher,
    session: TestSession,
    cell_resolver: CellResolver,
    working_dir_cell: CellName,
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

    let ExecutorLaunch {
        handle: executor_handle,
        client: test_executor,
        make_server,
    } = launcher
        .launch(tpx_args)
        .await
        .context("Failed to launch executor")
        .map_err(
            |err| match quiet_soft_error!("executor_launch_failed", err) {
                Ok(e) => e,
                Err(e) => e,
            },
        )?;

    let test_executor = Arc::new(test_executor) as Arc<dyn TestExecutor>;

    let (test_status_sender, test_status_receiver) = mpsc::unbounded();

    let test_run =
        ctx.temporary_spawn({
            let test_status_sender = test_status_sender.clone();
            move |ctx| {
                // NOTE: This is made a critical section so that we shut down gracefully. We'll cancel
                // if the liveliness guard indicates we should.
                critical_section(move || async move {
                    // Spawn our server to listen to the test runner's requests for execution.
                    let orchestrator = BuckTestOrchestrator::new(
                        ctx.dupe(),
                        session.dupe(),
                        liveliness_observer.dupe(),
                        test_status_sender,
                    )
                    .await
                    .context("Failed to create a BuckTestOrchestrator")?;

                    let server_handle = make_server(orchestrator, BuckTestDownwardApi);

                    let mut driver = TestDriver::new(TestDriverState {
                        ctx: &ctx,
                        label_filtering: &label_filtering,
                        global_target_platform: &global_target_platform,
                        session: &session,
                        test_executor: &test_executor,
                        cell_resolver: &cell_resolver,
                        working_dir_cell,
                    });

                    driver.push_pattern(pattern.convert_pattern().context(
                        "Test with explicit configuration pattern is not supported yet",
                    )?);

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

                    // And finally return our results;

                    anyhow::Ok((driver.build_errors, test_statuses))
                })
            }
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

    let (build_errors, executor_report) = test_run
        .await
        .context("Failed to collect executor report")?;

    Ok(TestOutcome {
        error_messages: build_errors,
        executor_report,
    })
}

enum TestDriverTask {
    InterpretTarget {
        package: PackageLabel,
        spec: PackageSpec<ProvidersPatternExtra>,
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
    fn push_pattern(&mut self, pattern: ResolvedPattern<ProvidersPatternExtra>) {
        for (package, spec) in pattern.specs.into_iter() {
            let fut = future::ready(anyhow::Ok(TestDriverTask::InterpretTarget {
                package,
                spec,
            }))
            .boxed();

            self.work.push(fut);
        }
    }

    /// Drive the test loop until all work is complete.
    async fn drive_to_completion(&mut self) {
        while let Some(task) = self.work.next().await {
            match task {
                Ok(TestDriverTask::InterpretTarget { package, spec }) => {
                    self.interpret_targets(package, spec);
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
    ) {
        let state = self.state;

        self.work.push(
            async move {
                let res = state.ctx.get_interpreter_results(package.dupe()).await?;
                let SpecTargets { labels, skippable } = spec_to_targets(spec, res)?;

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
                    .get_configured_target(&label, state.global_target_platform.as_ref())
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

                // Test this, it's compatible.
                let mut labels = vec![label];

                // Look up `tests` in the the target we're testing, and if we find any tests them, add them to the
                // test backlog.
                for test in node.tests() {
                    labels.push(test);
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
) -> anyhow::Result<SpecTargets> {
    let available_targets = res.targets();

    match spec {
        PackageSpec::All => {
            let labels = available_targets
                .keys()
                .map(|target| {
                    (
                        target.to_owned(),
                        ProvidersPatternExtra {
                            providers: ProvidersName::Default,
                        },
                    )
                })
                .collect();
            Ok(SpecTargets {
                labels,
                skippable: true,
            })
        }
        PackageSpec::Targets(labels) => {
            for (target_name, _) in &labels {
                res.resolve_target(target_name)?;
            }
            Ok(SpecTargets {
                labels,
                skippable: false,
            })
        }
    }
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
    let _ignored = future::join_all(
        artifacts_to_build
            .into_iter()
            .map(|input| async { ctx.ensure_artifact_group(&input).await.map(|v| (input, v)) }),
    )
    .await
    .into_iter()
    .collect::<Result<IndexMap<_, _>, _>>()?;
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
