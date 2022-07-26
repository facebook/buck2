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
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::interpreter::module_internals::EvaluationResult;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::test_provider::TestProvider;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::*;
use buck2_core::fs::project::*;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_node::compatibility::MaybeCompatible;
use buck2_test::downward_api::BuckTestDownwardApi;
use buck2_test::executor_launcher::ExecutorLaunch;
use buck2_test::executor_launcher::ExecutorLauncher;
use buck2_test::executor_launcher::OutOfProcessTestExecutor;
use buck2_test::orchestrator::BuckTestOrchestrator;
use buck2_test::orchestrator::TestResultOrExitCode;
use buck2_test::session::TestSession;
use buck2_test::session::TestSessionOptions;
use buck2_test::translations::build_configured_target_handle;
use cli_proto::TestRequest;
use cli_proto::TestResponse;
use derive_more::Display;
use dice::DiceComputations;
use futures::channel::mpsc;
use futures::future;
use futures::future::BoxFuture;
use futures::future::FutureExt;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use indexmap::IndexMap;
use indexmap::IndexSet;
use serde::Serialize;
use test_api::data::TestStatus;
use test_api::protocol::TestExecutor;
use thiserror::Error;

use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::resolve_patterns;
use crate::daemon::common::target_platform_from_client_context;
use crate::daemon::server::ServerCommandContext;

#[derive(Debug, Error)]
pub(crate) enum TestError {
    #[error("Unknown target `{0}` from package `{1}`")]
    UnknownTarget(TargetName, Package),
}

#[derive(Debug, Serialize)]
pub(crate) struct TestReport {
    project_root: AbsPathBuf,
    outputs: HashMap<TargetLabel, Vec<ProjectRelativePathBuf>>,
}

struct TestOutcome {
    error_messages: Vec<String>,
    executor_report: ExecutorReport,
}

impl TestOutcome {
    pub(crate) fn exit_code(&self) -> anyhow::Result<i32> {
        if !self.error_messages.is_empty() {
            return Ok(1);
        }
        self.executor_report
            .exit_code
            .context("Test executor did not provide an exit code")
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
                self.statuses.ingest(&res.status);
            }
            TestResultOrExitCode::ExitCode(exit_code) => {
                self.exit_code = Some(*exit_code);
            }
        }
    }
}

#[derive(Display, Default)]
#[display(
    fmt = "{} Pass. {} Fail. {} Fatal. {} Skip.",
    passed,
    failed,
    fatals,
    skipped
)]
struct TestStatuses {
    passed: u64,
    skipped: u64,
    failed: u64,
    fatals: u64,
    listing_success: u64,
    listing_failed: u64,
}

impl TestStatuses {
    fn ingest(&mut self, status: &TestStatus) {
        match status {
            TestStatus::PASS => self.passed += 1,
            TestStatus::FAIL => self.failed += 1,
            TestStatus::SKIP => self.skipped += 1,
            TestStatus::OMITTED => self.skipped += 1,
            TestStatus::FATAL => self.fatals += 1,
            TestStatus::TIMEOUT => self.failed += 1,
            TestStatus::UNKNOWN => {}
            TestStatus::RERUN => {}
            TestStatus::LISTING_SUCCESS => self.listing_success += 1,
            TestStatus::LISTING_FAILED => self.listing_failed += 1,
        }
    }
}

pub(crate) async fn test(
    server_ctx: ServerCommandContext,
    request: TestRequest,
) -> anyhow::Result<TestResponse> {
    let cwd = &server_ctx.working_dir;
    let ctx = server_ctx.dice_ctx().await?;
    let cell_resolver = ctx.get_cell_resolver().await?;

    let global_target_platform =
        target_platform_from_client_context(request.context.as_ref(), &cell_resolver, cwd).await?;

    // Get the test runner from the config. Note that we use a different key from v1 since the API
    // is completely different, so there is not expectation that the same binary works for both.
    let test_executor = ctx
        .get_legacy_config_property(cell_resolver.root_cell(), "test", "v2_test_executor")
        .await?
        .context("test.v2_test_executor must be set in configuration")?
        .as_ref()
        .to_owned();

    let parsed_patterns = parse_patterns_from_cli_args(&request.target_patterns, &ctx, cwd).await?;
    let resolved_pattern =
        resolve_patterns(&parsed_patterns, &cell_resolver, &ctx.file_ops()).await?;

    let launcher: Box<dyn ExecutorLauncher> = box OutOfProcessTestExecutor {
        name: test_executor,
    };

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
        request.test_executor_args,
        Arc::new(TestLabelFiltering::new(
            request.included_labels,
            request.excluded_labels,
            request.always_exclude,
            request.build_filtered_targets,
        )),
        &*launcher,
        session,
        cell_resolver,
    )
    .await?;

    let test_statuses = cli_proto::test_response::TestStatuses {
        passed: test_outcome.executor_report.statuses.passed,
        skipped: test_outcome.executor_report.statuses.skipped,
        failed: test_outcome.executor_report.statuses.failed,
        fatals: test_outcome.executor_report.statuses.fatals,
        listing_success: test_outcome.executor_report.statuses.listing_success,
        listing_failed: test_outcome.executor_report.statuses.listing_failed,
    };

    // TODO(bobyf) remap exit code for buck reserved exit code
    let exit_code = test_outcome.exit_code().context("No exit code available")?;
    Ok(TestResponse {
        exit_code,
        error_messages: test_outcome.error_messages,
        test_statuses: Some(test_statuses),
    })
}

async fn test_targets(
    ctx: &DiceComputations,
    pattern: ResolvedPattern<ProvidersPattern>,
    global_target_platform: Option<TargetLabel>,
    external_runner_args: Vec<String>,
    label_filtering: Arc<TestLabelFiltering>,
    launcher: &dyn ExecutorLauncher,
    session: TestSession,
    cell_resolver: CellResolver,
) -> anyhow::Result<TestOutcome> {
    let session = Arc::new(session);

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
        .context("Failed to launch executor")?;

    let test_executor = Arc::new(test_executor) as Arc<dyn TestExecutor>;

    let (test_status_sender, test_status_receiver) = mpsc::unbounded();

    // this is kinda ugly. but we have a bunch of lifetimes on the orchestrator and tpx engine
    // that makes them not spawnable, and so we have to spawn the test discovery code instead.
    // TODO(bobyf): we can probably clean this up after doing tpx out of proc, and fixing some lifetimes
    let test_run = ctx.temporary_spawn({
        let test_status_sender = test_status_sender.clone();
        move |ctx| async move {
            // Spawn our server to listen to the test runner's requests for execution.
            let orchestrator =
                BuckTestOrchestrator::new(ctx.dupe(), session.dupe(), test_status_sender)
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
            });

            driver.push_pattern(pattern);
            driver.drive_to_completion().await;

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
        }
    });

    eprintln!("waiting for all tests to finish...");
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
        package: Package,
        spec: PackageSpec<ProvidersPattern>,
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
    fn push_pattern(&mut self, pattern: ResolvedPattern<ProvidersPattern>) {
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

    fn interpret_targets(&mut self, package: Package, spec: PackageSpec<ProvidersPattern>) {
        let state = self.state;

        self.work.push(
            async move {
                let res = state.ctx.get_interpreter_results(&package).await?;
                let SpecTargets { labels, skippable } = spec_to_targets(&package, spec, res)?;

                let labels = labels.into_map(|(target, providers_name)| {
                    ProvidersLabel::new(TargetLabel::new(package.dupe(), target), providers_name)
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
                            return Err(reason.to_err().into());
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
    labels: Vec<ProvidersPattern>,
    /// Indicates whether this should be skipped if incompatible.
    skippable: bool,
}

fn spec_to_targets(
    package: &Package,
    spec: PackageSpec<ProvidersPattern>,
    res: Arc<EvaluationResult>,
) -> anyhow::Result<SpecTargets> {
    let available_targets = res.targets();

    match spec {
        PackageSpec::All => {
            let labels = available_targets
                .keys()
                .duped()
                .map(|t| (t, ProvidersName::Default))
                .collect();
            Ok(SpecTargets {
                labels,
                skippable: true,
            })
        }
        PackageSpec::Targets(labels) => {
            if let Some(missing) = labels
                .iter()
                .find(|t| !available_targets.contains_key(&t.0))
            {
                Err(TestError::UnknownTarget(missing.0.dupe(), package.dupe()).into())
            } else {
                Ok(SpecTargets {
                    labels,
                    skippable: false,
                })
            }
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
) -> anyhow::Result<Option<ConfiguredProvidersLabel>> {
    // NOTE: We fail if we hit an incompatible target here. This can happen if we reach an
    // incompatible target via `tests = [...]`. This should perhaps change, but that's how it works
    // in v1: https://fb.workplace.com/groups/buckeng/posts/8520953297953210
    let frozen_providers = ctx.get_providers(&target).await?.require_compatible()?;

    let fut = match build_artifacts(ctx, &frozen_providers, &*label_filtering).await? {
        Some(_built) => {
            let providers = frozen_providers.provider_collection();

            match <dyn TestProvider>::from_collection(providers) {
                Some(test_info) => {
                    if label_filtering.build_filtered_targets {
                        let target_labels = test_info.labels();

                        if label_filtering.is_excluded(target_labels) {
                            return Ok(None);
                        }
                    }

                    run_tests(test_executor, target, test_info, session, cell_resolver)
                        .map(|l| Some(l).transpose())
                        .left_future()
                }
                None => {
                    // not a test
                    future::ready(Ok(None)).right_future()
                }
            }
        }
        None => future::ready(Ok(None)).right_future(),
    };

    // NOTE: We produce the future above, then await it here because Module & TestInfo aren't Sync.
    // That's OK, because we don't need them across an await point, but we do need to stucture the
    // code this way for the compiler to see it.
    fut.await
}

async fn build_artifacts(
    ctx: &DiceComputations,
    frozen_providers: &FrozenProviderCollectionValue,
    label_filtering: &TestLabelFiltering,
) -> anyhow::Result<Option<IndexMap<ArtifactGroup, ArtifactGroupValues>>> {
    fn get_artifacts_to_build(
        label_filtering: &TestLabelFiltering,
        frozen_providers: &FrozenProviderCollectionValue,
    ) -> anyhow::Result<Option<IndexSet<ArtifactGroup>>> {
        let providers = frozen_providers.provider_collection();

        Ok(match <dyn TestProvider>::from_collection(providers) {
            Some(provider) => {
                if !label_filtering.build_filtered_targets {
                    let target_labels = provider.labels();

                    if label_filtering.is_excluded(target_labels) {
                        return Ok(None);
                    }
                }

                let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
                provider.visit_artifacts(&mut artifact_visitor)?;
                Some(artifact_visitor.inputs)
            }
            None => {
                // not a test
                None
            }
        })
    }

    Ok(
        match get_artifacts_to_build(label_filtering, frozen_providers)? {
            Some(to_build) => {
                // build the test target first
                Some(
                    future::join_all(to_build.into_iter().map(|input| async {
                        ctx.ensure_artifact_group(&input).await.map(|v| (input, v))
                    }))
                    .await
                    .into_iter()
                    .collect::<Result<IndexMap<_, _>, _>>()?,
                )
            }
            None => None,
        },
    )
}

fn run_tests<'a, 'b>(
    test_executor: Arc<dyn TestExecutor + 'a>,
    providers_label: ConfiguredProvidersLabel,
    test_info: &'b dyn TestProvider,
    session: &'b TestSession,
    cell_resolver: &'b CellResolver,
) -> BoxFuture<'a, anyhow::Result<ConfiguredProvidersLabel>> {
    let maybe_handle =
        build_configured_target_handle(providers_label.clone(), session, cell_resolver);

    match maybe_handle {
        Ok(handle) => {
            let fut = test_info.dispatch(handle, test_executor);

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

#[cfg(test)]
mod tests {
    use crate::daemon::test::TestLabelFiltering;

    #[test]
    fn only_include_labels_in_includes() {
        let filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!not_me".to_owned()],
            vec!["this_doesnt_affect_anything".to_owned()],
            false,
            false,
        );

        assert_eq!(filter.is_excluded(vec!["include_me"]), false);
        assert_eq!(filter.is_excluded(vec!["not_me"]), true);
        assert_eq!(filter.is_excluded(vec!["blah"]), true);
    }

    #[test]
    fn order_of_precedence() {
        let conflicting_filter = TestLabelFiltering::new(
            vec!["!not_me1".to_owned(), "not_me1".to_owned()],
            vec!["not_me2".to_owned()],
            false,
            false,
        );

        assert_eq!(conflicting_filter.is_excluded(vec!["not_me1"]), true);

        let conflicting_filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!include_me".to_owned()],
            vec!["include_me".to_owned()],
            false,
            false,
        );

        assert_eq!(conflicting_filter.is_excluded(vec!["include_me"]), false);

        let conflicting_filter = TestLabelFiltering::new(
            vec!["include_me".to_owned()],
            vec!["!include_me".to_owned()],
            false,
            false,
        );

        assert_eq!(conflicting_filter.is_excluded(vec!["include_me"]), false);
    }

    #[test]
    fn always_excludes_overrides_precedence() {
        let filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!not_me1".to_owned()],
            vec!["not_me2".to_owned()],
            true,
            false,
        );

        assert_eq!(filter.is_excluded(vec!["include_me", "blah"]), false);
        assert_eq!(filter.is_excluded(vec!["include_me", "not_me1"]), true);
        assert_eq!(filter.is_excluded(vec!["include_me", "not_me2"]), true);

        let conflicting_filter = TestLabelFiltering::new(
            vec!["include_me".to_owned(), "!include_me".to_owned()],
            vec!["include_me".to_owned()],
            true,
            false,
        );

        assert_eq!(conflicting_filter.is_excluded(vec!["include_me"]), true);
    }
}
