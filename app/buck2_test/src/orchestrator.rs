/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// https://github.com/rust-lang/rust-clippy/issues/12806
#![allow(clippy::unnecessary_to_owned)]

//! Implementation of the `TestOrchestrator` from `buck2_test_api`.

use std::borrow::Cow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ffi::OsStr;
use std::fmt::Display;
use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::ControlFlow;
use std::ops::DerefMut;
use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::execute::dice_data::CommandExecutorResponse;
use buck2_build_api::actions::execute::dice_data::DiceHasCommandExecutor;
use buck2_build_api::actions::execute::dice_data::GetReClient;
use buck2_build_api::actions::impls::run_action_knobs::HasRunActionKnobs;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::build_signals::HasBuildSignals;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::ArtifactPathMapperImpl;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::TestCommandMember;
use buck2_build_api::interpreter::rule_defs::provider::builtin::local_resource_info::FrozenLocalResourceInfo;
use buck2_build_api::keep_going::KeepGoing;
use buck2_build_signals::env::NodeDuration;
use buck2_build_signals::env::WaitingData;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::events::HasEvents;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::execution_types::executor_config::CommandGenerationOptions;
use buck2_core::execution_types::executor_config::Executor;
use buck2_core::execution_types::executor_config::LocalExecutorOptions;
use buck2_core::execution_types::executor_config::MetaInternalExtraParams;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::execution_types::executor_config::RemoteExecutorCustomImage;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutTestPath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_data::EndOfTestResults;
use buck2_data::SetupLocalResourcesEnd;
use buck2_data::SetupLocalResourcesStart;
use buck2_data::TestDiscovery;
use buck2_data::TestDiscoveryEnd;
use buck2_data::TestDiscoveryStart;
use buck2_data::TestRunEnd;
use buck2_data::TestRunStart;
use buck2_data::TestSessionInfo;
use buck2_data::TestSuite;
use buck2_data::ToProtoMessage;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::execute::blocking::HasBlockingExecutor;
use buck2_execute::execute::cache_uploader::CacheUploadInfo;
use buck2_execute::execute::cache_uploader::NoOpCacheUploader;
use buck2_execute::execute::claim::MutexClaimManager;
use buck2_execute::execute::command_executor::CommandExecutor;
use buck2_execute::execute::environment_inheritance::EnvironmentInheritance;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::prepared::NoOpCommandOptionalExecutor;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::request::OutputCreationBehavior;
use buck2_execute::execute::request::WorkerId;
use buck2_execute::execute::request::WorkerSpec;
use buck2_execute::execute::result::CommandCancellationReason;
use buck2_execute::execute::result::CommandExecutionMetadata;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute_impl::executors::local::EnvironmentBuilder;
use buck2_execute_impl::executors::local::apply_local_execution_environment;
use buck2_execute_impl::executors::local::create_output_dirs;
use buck2_execute_impl::executors::local::materialize_inputs;
use buck2_execute_impl::executors::local::prep_scratch_path;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_resource_control::HasResourceControl;
use buck2_test_api::data::ArgValue;
use buck2_test_api::data::ArgValueContent;
use buck2_test_api::data::CasDigest;
use buck2_test_api::data::ConfiguredTargetHandle;
use buck2_test_api::data::DeclaredOutput;
use buck2_test_api::data::ExecuteResponse;
use buck2_test_api::data::ExecutionDetails;
use buck2_test_api::data::ExecutionResult2;
use buck2_test_api::data::ExecutionStatus;
use buck2_test_api::data::ExecutionStream;
use buck2_test_api::data::ExecutorConfigOverride;
use buck2_test_api::data::ExternalRunnerSpecValue;
use buck2_test_api::data::LocalExecutionCommand;
use buck2_test_api::data::Output;
use buck2_test_api::data::PrepareForLocalExecutionResult;
use buck2_test_api::data::RequiredLocalResources;
use buck2_test_api::data::TestResult;
use buck2_test_api::data::TestStage;
use buck2_test_api::data::convert::host_sharing_requirements_to_grpc;
use buck2_test_api::protocol::TestOrchestrator;
use derive_more::From;
use dice::DiceComputations;
use dice::DiceTransaction;
use dice::Key;
use dice_futures::cancellation::CancellationContext;
use display_container::fmt_container;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use futures::FutureExt;
use futures::channel::mpsc::UnboundedSender;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use fxhash::FxHashMap;
use host_sharing::HostSharingRequirements;
use indexmap::IndexMap;
use indexmap::IndexSet;
use indexmap::indexset;
use itertools::Itertools;
use sorted_vector_map::SortedVectorMap;
use starlark::values::OwnedFrozenValueTyped;

use crate::local_resource_api::LocalResourcesSetupResult;
use crate::local_resource_registry::HasLocalResourceRegistry;
use crate::local_resource_setup::TestStageSimple;
use crate::local_resource_setup::required_providers;
use crate::remote_storage;
use crate::session::TestSession;
use crate::session::TestSessionOptions;
use crate::translations;

const MAX_SUFFIX_LEN: usize = 1024;

#[derive(Debug, Eq, PartialEq)]
pub enum ExecutorMessage {
    TestResult(TestResult),
    ExitCode(i32),
    InfoMessage(String),
}

pub struct BuckTestOrchestrator<'a: 'static> {
    dice: DiceTransaction,
    session: Arc<TestSession>,
    results_channel: UnboundedSender<buck2_error::Result<ExecutorMessage>>,
    events: EventDispatcher,
    liveliness_observer: Arc<dyn LivelinessObserver>,
    cancellations: &'a CancellationContext,
    re_client: Arc<remote_storage::ReClientWithCache>,
}

impl<'a> BuckTestOrchestrator<'a> {
    pub async fn new(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        results_channel: UnboundedSender<buck2_error::Result<ExecutorMessage>>,
        cancellations: &'a CancellationContext,
    ) -> buck2_error::Result<BuckTestOrchestrator<'a>> {
        let events = dice.per_transaction_data().get_dispatcher().dupe();
        let re_client = Arc::new(remote_storage::ReClientWithCache::new(
            dice.per_transaction_data().get_re_client(),
        ));
        Ok(Self::from_parts(
            dice,
            session,
            liveliness_observer,
            results_channel,
            events,
            cancellations,
            re_client,
        ))
    }

    fn from_parts(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        results_channel: UnboundedSender<buck2_error::Result<ExecutorMessage>>,
        events: EventDispatcher,
        cancellations: &'a CancellationContext,
        re_client: Arc<remote_storage::ReClientWithCache>,
    ) -> BuckTestOrchestrator<'a> {
        Self {
            dice,
            session,
            results_channel,
            events,
            liveliness_observer,
            cancellations,
            re_client,
        }
    }

    async fn require_alive(
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> Result<(), Cancelled> {
        if !liveliness_observer.is_alive().await {
            return Err(Cancelled {
                ..Default::default()
            });
        }

        Ok(())
    }

    async fn execute2(
        &self,
        stage: TestStage,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
        required_local_resources: RequiredLocalResources,
    ) -> Result<ExecutionResult2, ExecuteError> {
        Self::require_alive(self.liveliness_observer.dupe()).await?;

        let test_target = self.session.get(test_target)?;

        let fs = self.dice.clone().get_artifact_fs().await?;
        let pre_create_dirs = Arc::new(pre_create_dirs);

        let ExecuteData {
            stdout,
            stderr,
            status,
            timing,
            execution_kind,
            outputs,
        } = prepare_and_execute(
            self.dice.dupe().deref_mut(),
            self.cancellations,
            TestExecutionKey {
                test_target,
                cmd: Arc::new(cmd),
                env: Arc::new(env),
                executor_override: executor_override.map(Arc::new),
                required_local_resources: Arc::new(required_local_resources),
                pre_create_dirs: pre_create_dirs.dupe(),
                stage: Arc::new(stage),
                options: self.session.options(),
                timeout,
                host_sharing_requirements: host_sharing_requirements.into(),
            },
            self.liveliness_observer.dupe(),
        )
        .await?;

        Self::require_alive(self.liveliness_observer.dupe()).await?;

        let mut output_map = HashMap::new();
        let mut paths_to_materialize = vec![];

        let remote_storage_config_update_futures = FuturesUnordered::new();

        for (test_path, artifact) in outputs {
            let project_relative_path = fs.buck_out_path_resolver().resolve_test(&test_path);
            let output_name = test_path.into_path().into();
            // It's OK to search iteratively here because there will be few entries in `pre_create_dirs`
            let remote_storage_config = pre_create_dirs
                .iter()
                .find(|&x| x.name == output_name)
                .map_or_else(Default::default, |x| x.remote_storage_config.dupe());
            match (
                remote_storage_config.supports_remote,
                execution_kind.as_ref(),
                translations::convert_artifact(output_name.clone().into_string(), &artifact),
            ) {
                // This condition checks that a downstream consumer supports
                // remote outputs AND the output is actually in CAS.
                //
                // TODO(arr): is there a better way to check that the output is
                // in CAS other than checking that the command was executed on
                // RE? Alternatively, when we make buck upload local testing
                // artifacts to CAS, we can remove this condition altogether.
                (true, Some(CommandExecutionKind::Remote { .. }), Some(remote_object)) => {
                    let re_client = self.re_client.clone();
                    let future = async move {
                        let _unused = re_client
                            .apply_config(&artifact, &remote_storage_config)
                            .await;
                        (output_name, remote_object)
                    };
                    remote_storage_config_update_futures.push(future);
                }
                _ => {
                    paths_to_materialize.push(project_relative_path.clone());
                    let abs_path = fs.fs().resolve(&project_relative_path);
                    output_map.insert(output_name, Output::LocalPath(abs_path));
                }
            };
        }
        let results: Vec<_> = remote_storage_config_update_futures.collect().await;
        for result in results {
            output_map.insert(result.0, Output::RemoteObject(result.1));
        }

        // Request materialization in case this ran on RE. Eventually Tpx should be able to
        // understand remote outputs but currently we don't have this.
        self.dice
            .per_transaction_data()
            .get_materializer()
            .ensure_materialized(paths_to_materialize)
            .await
            .buck_error_context("Error materializing test outputs")?;

        Ok(ExecutionResult2 {
            status,
            stdout,
            stderr,
            outputs: output_map,
            start_time: timing.start_time,
            execution_time: timing.execution_time,
            execution_details: ExecutionDetails {
                execution_kind: execution_kind.map(|k| k.to_proto(false)),
            },
            max_memory_used_bytes: timing.execution_stats.and_then(|s| s.memory_peak),
        })
    }

    async fn prepare_and_execute_no_dice(
        dice: &mut DiceComputations<'_>,
        key: TestExecutionKey,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        cancellation: &CancellationContext,
    ) -> Result<ExecuteData, ExecuteError> {
        let TestExecutionKey {
            test_target,
            cmd,
            env,
            executor_override,
            required_local_resources,
            pre_create_dirs,
            stage,
            options,
            timeout,
            host_sharing_requirements,
        } = key;
        let fs = dice.get_artifact_fs().await?;
        let test_info = Self::get_test_info(dice, &test_target).await?;
        let test_executor = Self::get_test_executor(
            dice,
            &test_target,
            &test_info,
            executor_override,
            &fs,
            &stage,
        )
        .await?;
        let test_executable_expanded = Self::expand_test_executable(
            dice,
            &test_target,
            &test_info,
            Cow::Borrowed(&cmd),
            Cow::Borrowed(&env),
            Cow::Borrowed(&pre_create_dirs),
            &test_executor.executor().executor_fs(),
            &stage,
            options,
        )
        .boxed()
        .await?;
        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            ensured_inputs,
            supports_re,
            declared_outputs,
            worker,
        } = test_executable_expanded;

        let input_deps_action_keys: Vec<_> = ensured_inputs
            .iter()
            .flat_map(|(_, agv)| {
                agv.iter()
                    .filter_map(|(artifact, _)| artifact.action_key().map(|k| k.dupe()))
            })
            .collect::<HashSet<_>>() // dedupe
            .into_iter()
            .collect();

        let executor_preference = Self::executor_preference(options, supports_re)?;
        let required_resources = if test_executor
            .executor()
            .is_local_execution_possible(executor_preference)
        {
            let setup_local_resources_executor = Self::get_local_executor(dice, &fs).await?;
            let simple_stage = stage.as_ref().into();

            let required_providers = {
                required_providers(dice, &test_info, &required_local_resources, &simple_stage)
                    .await?
            };
            // If some timeout is neeeded, use the same value as for the test itself which is better than nothing.
            Self::setup_local_resources(
                dice,
                cancellation,
                required_providers,
                setup_local_resources_executor,
                timeout,
                liveliness_observer.dupe(),
            )
            .await?
        } else {
            vec![]
        };
        let execution_request = Self::create_command_execution_request(
            dice,
            cwd,
            expanded_cmd,
            expanded_env,
            ensured_inputs,
            declared_outputs,
            &fs,
            Some(timeout),
            Some(host_sharing_requirements),
            Some(executor_preference),
            required_resources,
            worker,
            test_executor.re_dynamic_image(),
            test_executor.meta_internal_extra_params(),
        )
        .boxed()
        .await?;
        let result = Self::execute_request(
            dice,
            cancellation,
            &test_target,
            &stage,
            test_executor.executor(),
            execution_request,
            liveliness_observer.dupe(),
            test_executor.re_cache_enabled(),
        )
        .boxed()
        .await?;

        if let Some(signals) = dice.per_transaction_data().get_build_signals() {
            let duration = NodeDuration {
                user: result.timing.execution_time,
                total: result.timing.time_span,
                queue: result.timing.queue_duration,
            };

            match stage.as_ref() {
                TestStage::Listing { suite, .. } => {
                    signals.test_listing(
                        test_target.target().dupe(),
                        suite.to_owned(),
                        duration.to_owned(),
                        &input_deps_action_keys,
                    );
                }
                TestStage::Testing {
                    suite,
                    testcases,
                    variant,
                } => {
                    signals.test_execution(
                        test_target.target().dupe(),
                        suite.to_owned(),
                        testcases,
                        variant.to_owned(),
                        duration,
                        &input_deps_action_keys,
                    );
                }
            }
        }

        Ok(result)
    }
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
struct TestExecutionKey {
    test_target: ConfiguredProvidersLabel,
    cmd: Arc<Vec<ArgValue>>,
    env: Arc<SortedVectorMap<String, ArgValue>>,
    executor_override: Option<Arc<ExecutorConfigOverride>>,
    required_local_resources: Arc<RequiredLocalResources>,
    pre_create_dirs: Arc<Vec<DeclaredOutput>>,
    stage: Arc<TestStage>,
    options: TestSessionOptions,
    timeout: Duration,
    host_sharing_requirements: Arc<HostSharingRequirements>,
}

#[async_trait]
impl Key for TestExecutionKey {
    type Value = Result<Arc<ExecuteData>, ExecuteError>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellations: &CancellationContext,
    ) -> Self::Value {
        cancellations
            .with_structured_cancellation(|observer| {
                async move {
                    BuckTestOrchestrator::prepare_and_execute_no_dice(
                        ctx,
                        self.dupe(),
                        Arc::new(observer),
                        cancellations,
                    )
                    .await
                }
                .boxed()
            })
            .await
            .map(Arc::new)
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }

    fn validity(x: &Self::Value) -> bool {
        // We don't want to cache any failed listings
        x.as_ref()
            .is_ok_and(|f| f.status == ExecutionStatus::Finished { exitcode: 0 })
    }
}

async fn prepare_and_execute(
    ctx: &mut DiceComputations<'static>,
    cancellation: &CancellationContext,
    key: TestExecutionKey,
    liveliness_observer: Arc<dyn LivelinessObserver>,
) -> Result<ExecuteData, ExecuteError> {
    let execute_on_dice = match key.stage.as_ref() {
        TestStage::Listing { cacheable, .. } => *cacheable,
        TestStage::Testing { .. } => false,
    };
    if execute_on_dice {
        let result = tokio::select! {
            _ = liveliness_observer.while_alive() => {
                Err(ExecuteError::Cancelled(Cancelled{..Default::default()}))
            }
            result = prepare_and_execute_dice(ctx, &key) => {
                result
            }
        }?;
        Ok((*result).clone())
    } else {
        Ok(BuckTestOrchestrator::prepare_and_execute_no_dice(
            ctx,
            key,
            liveliness_observer,
            cancellation,
        )
        .await?)
    }
}

async fn prepare_and_execute_dice(
    ctx: &mut DiceComputations<'_>,
    key: &TestExecutionKey,
) -> Result<Arc<ExecuteData>, ExecuteError> {
    ctx.compute(key).await.map_err(buck2_error::Error::from)?
}

impl Display for TestExecutionKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "test_target = {}, ", self.test_target)?;
        fmt_container(f, "cmd = [", "], ", self.cmd.as_ref())?;
        fmt_keyed_container(f, "env = {", "}, ", ",", self.env.as_ref())?;
        fmt_container(
            f,
            "executor_override = [",
            "], ",
            self.executor_override.iter(),
        )?;
        write!(
            f,
            "required_local_resources = {}, ",
            self.required_local_resources.as_ref(),
        )?;
        fmt_container(f, "pre_create_dirs = [", "], ", self.pre_create_dirs.iter())?;
        write!(
            f,
            "stage = {}, options = {}, timeout = {}, host_sharing_requirements = {}",
            self.stage,
            self.options,
            self.timeout.as_millis(),
            self.host_sharing_requirements
        )
    }
}

struct PreparedLocalResourceSetupContext {
    pub target: ConfiguredTargetLabel,
    pub execution_request: CommandExecutionRequest,
    pub env_var_mapping: IndexMap<String, String>,
}

#[derive(Clone, Dupe, Allocative)]
enum CancellationReason {
    NotSpecified,
    ReQueueTimeout,
}

#[derive(Default, Clone, Dupe, Allocative)]
struct Cancelled {
    reason: Option<CancellationReason>,
}

// NOTE: This doesn't implement Error so that we can't accidentally lose the Cancelled variant.
#[derive(From, Clone, Dupe, Allocative)]
enum ExecuteError {
    Error(buck2_error::Error),
    Cancelled(Cancelled),
}

#[async_trait]
impl TestOrchestrator for BuckTestOrchestrator<'_> {
    async fn execute2(
        &self,
        stage: TestStage,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        timeout: Duration,
        host_sharing_requirements: HostSharingRequirements,
        pre_create_dirs: Vec<DeclaredOutput>,
        executor_override: Option<ExecutorConfigOverride>,
        required_local_resources: RequiredLocalResources,
    ) -> buck2_error::Result<ExecuteResponse> {
        let res = BuckTestOrchestrator::execute2(
            self,
            stage,
            test_target,
            cmd,
            env,
            timeout,
            host_sharing_requirements,
            pre_create_dirs,
            executor_override,
            required_local_resources,
        )
        .await;

        match res {
            Ok(r) => Ok(ExecuteResponse::Result(r)),
            Err(ExecuteError::Cancelled(cancelled)) => {
                Ok(ExecuteResponse::Cancelled(match cancelled.reason {
                    Some(CancellationReason::NotSpecified) => {
                        Some(buck2_test_api::data::CancellationReason::NotSpecified)
                    }
                    Some(CancellationReason::ReQueueTimeout) => {
                        Some(buck2_test_api::data::CancellationReason::ReQueueTimeout)
                    }
                    None => None,
                }))
            }
            Err(ExecuteError::Error(e)) => Err(e),
        }
    }

    async fn report_test_result(&self, r: TestResult) -> buck2_error::Result<()> {
        let event = buck2_data::instant_event::Data::TestResult(translations::convert_test_result(
            r.clone(),
            &self.session,
        )?);
        self.events.instant_event(event);
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::TestResult(r)))
            .map_err(|_| {
                buck2_error::internal_error!("Test result was received after end-of-tests")
            })?;
        Ok(())
    }

    async fn report_tests_discovered(
        &self,
        test_target: ConfiguredTargetHandle,
        suite: String,
        names: Vec<String>,
    ) -> buck2_error::Result<()> {
        let test_target = self.session.get(test_target)?;

        self.events.instant_event(TestDiscovery {
            data: Some(buck2_data::test_discovery::Data::Tests(TestSuite {
                suite_name: suite,
                test_names: names,
                target_label: Some(test_target.target().as_proto()),
            })),
        });

        Ok(())
    }

    async fn report_test_session(&self, session_info: String) -> buck2_error::Result<()> {
        self.events.instant_event(TestDiscovery {
            data: Some(buck2_data::test_discovery::Data::Session(TestSessionInfo {
                info: session_info,
            })),
        });

        Ok(())
    }

    async fn end_of_test_results(&self, exit_code: i32) -> buck2_error::Result<()> {
        self.events.instant_event(EndOfTestResults { exit_code });
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::ExitCode(exit_code)))
            .map_err(|_| buck2_error::internal_error!("end_of_tests was received twice"))?;
        self.results_channel.close_channel();
        Ok(())
    }

    async fn prepare_for_local_execution(
        &self,
        stage: TestStage,
        test_target: ConfiguredTargetHandle,
        cmd: Vec<ArgValue>,
        env: SortedVectorMap<String, ArgValue>,
        pre_create_dirs: Vec<DeclaredOutput>,
        required_local_resources: RequiredLocalResources,
    ) -> buck2_error::Result<PrepareForLocalExecutionResult> {
        let test_target = self.session.get(test_target)?;

        let fs = self.dice.clone().get_artifact_fs().await?;

        let test_info = Self::get_test_info(self.dice.dupe().deref_mut(), &test_target).await?;

        // In contrast from actual test execution we do not check if local execution is possible.
        // We leave that decision to actual local execution runner that requests local execution preparation.
        let setup_local_resources_executor =
            Self::get_local_executor(self.dice.dupe().deref_mut(), &fs).await?;
        let providers = {
            required_providers(
                self.dice.dupe().deref_mut(),
                &test_info,
                &required_local_resources,
                &TestStageSimple::Testing,
            )
            .await?
        };
        let setup_commands: Vec<PreparedLocalResourceSetupContext> = self
            .dice
            .dupe()
            .deref_mut()
            .try_compute_join(providers, |dice, provider| {
                let fs = fs.clone();
                let executor_fs = setup_local_resources_executor.executor_fs();
                async move {
                    Self::prepare_local_resource(
                        dice,
                        provider,
                        &fs,
                        &executor_fs,
                        Duration::default(),
                    )
                    .await
                }
                .boxed()
            })
            .await?;

        // Tests are not run, so there is no executor override.
        let test_executor = Self::get_test_executor(
            self.dice.dupe().deref_mut(),
            &test_target,
            &test_info,
            None,
            &fs,
            &stage,
        )
        .await?;
        let test_executable_expanded = Self::expand_test_executable(
            self.dice.dupe().deref_mut(),
            &test_target,
            &test_info,
            Cow::Owned(cmd),
            Cow::Owned(env),
            Cow::Owned(pre_create_dirs),
            &test_executor.executor().executor_fs(),
            &stage,
            self.session.options(),
        )
        .await?;

        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            ensured_inputs,
            supports_re: _,
            declared_outputs,
            worker,
        } = test_executable_expanded;

        let execution_request = Self::create_command_execution_request(
            self.dice.dupe().deref_mut(),
            cwd,
            expanded_cmd,
            expanded_env,
            ensured_inputs,
            declared_outputs,
            &fs,
            None,
            None,
            None,
            vec![],
            worker,
            test_executor.re_dynamic_image(),
            test_executor.meta_internal_extra_params(),
        )
        .await?;

        let materializer = self.dice.per_transaction_data().get_materializer();
        let blocking_executor = self.dice.get_blocking_executor();

        let materialized_inputs = materialize_inputs(
            &fs,
            materializer.as_ref(),
            &execution_request,
            self.dice.global_data().get_digest_config(),
        )
        .await?;

        prep_scratch_path(&materialized_inputs.scratch, &fs).await?;

        create_output_dirs(
            &fs,
            &execution_request,
            materializer.dupe(),
            blocking_executor,
            self.cancellations,
        )
        .await?;

        for local_resource_setup_command in setup_commands.iter() {
            let materialized_inputs = materialize_inputs(
                &fs,
                materializer.as_ref(),
                &local_resource_setup_command.execution_request,
                self.dice.global_data().get_digest_config(),
            )
            .await?;
            let blocking_executor = self.dice.get_blocking_executor();

            prep_scratch_path(&materialized_inputs.scratch, &fs).await?;

            create_output_dirs(
                &fs,
                &local_resource_setup_command.execution_request,
                materializer.dupe(),
                blocking_executor,
                self.cancellations,
            )
            .await?;
        }

        Ok(create_prepare_for_local_execution_result(
            &fs,
            execution_request,
            setup_commands,
        ))
    }

    async fn attach_info_message(&self, message: String) -> buck2_error::Result<()> {
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::InfoMessage(message)))
            .map_err(|_| buck2_error::internal_error!("Message received after end-of-tests"))?;
        Ok(())
    }

    async fn upload_to_cas(&self, local_path: String) -> buck2_error::Result<CasDigest> {
        let digest_config = self.dice.global_data().get_digest_config();
        let re_digest = self
            .re_client
            .upload_local_file(&local_path, digest_config)
            .await?;
        Ok(CasDigest {
            hash: re_digest.hash,
            size_bytes: re_digest.size_in_bytes,
        })
    }
}
#[derive(Allocative, Clone)]
struct ExecuteData {
    pub stdout: ExecutionStream,
    pub stderr: ExecutionStream,
    pub status: ExecutionStatus,
    pub timing: CommandExecutionMetadata,
    pub execution_kind: Option<CommandExecutionKind>,
    pub outputs: Vec<(BuckOutTestPath, ArtifactValue)>,
}

impl BuckTestOrchestrator<'_> {
    fn executor_preference(
        opts: TestSessionOptions,
        test_supports_re: bool,
    ) -> buck2_error::Result<ExecutorPreference> {
        let mut executor_preference = ExecutorPreference::Default;

        if !opts.allow_re {
            // We don't ban RE (we only prefer not to use it) if the session doesn't allow it, so
            // that executor overrides or default executor can still route executions to RE.
            executor_preference = executor_preference.and(ExecutorPreference::LocalPreferred)?;
        }

        if !test_supports_re {
            // But if the test doesn't support RE at all, then we ban it.
            executor_preference = executor_preference.and(ExecutorPreference::LocalRequired)?;
        }

        Ok(executor_preference)
    }

    /// Core request execution logic.
    async fn execute_request(
        dice: &mut DiceComputations<'_>,
        cancellation: &CancellationContext,
        test_target_label: &ConfiguredProvidersLabel,
        stage: &TestStage,
        executor: &CommandExecutor,
        request: CommandExecutionRequest,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        re_cache_enabled: bool,
    ) -> Result<ExecuteData, ExecuteError> {
        let events = dice.per_transaction_data().get_dispatcher().dupe();
        let manager = CommandExecutionManager::new(
            Box::new(MutexClaimManager::new()),
            events.dupe(),
            liveliness_observer.dupe(),
            WaitingData::new(),
        );
        let digest_config = dice.global_data().get_digest_config();

        let test_target = TestTarget {
            target: test_target_label.target(),
            action_key_suffix: create_action_key_suffix(stage),
        };

        // For test execution, we currently do not do any cache queries

        let prepared_action = match executor.prepare_action(&request, digest_config, false) {
            Ok(prepared_action) => prepared_action,
            Err(e) => return Err(ExecuteError::Error(e)),
        };
        let prepared_command = PreparedCommand {
            target: &test_target as _,
            request: &request,
            prepared_action: &prepared_action,
            digest_config,
        };

        // instrument execution with a span.
        // TODO(brasselsprouts): migrate this into the executor to get better accuracy.
        let command_exec_result = match stage {
            TestStage::Listing { suite, cacheable } => {
                let start = TestDiscoveryStart {
                    target_label: Some(test_target.target.as_proto()),
                    suite_name: suite.clone(),
                };
                let (result, cached) = events
                    .span_async(start, async move {
                        let (result, cached) = if *cacheable {
                            match executor
                                .action_cache(manager, &prepared_command, cancellation)
                                .await
                            {
                                ControlFlow::Continue(manager) => {
                                    let result = executor
                                        .exec_cmd(manager, &prepared_command, cancellation)
                                        .await;
                                    (result, false)
                                }
                                ControlFlow::Break(result) => (result, true),
                            }
                        } else {
                            let result = executor
                                .exec_cmd(manager, &prepared_command, cancellation)
                                .await;
                            (result, false)
                        };
                        let end = TestDiscoveryEnd {
                            suite_name: suite.clone(),
                            target_label: Some(test_target.target.as_proto()),
                            command_report: Some(
                                result
                                    .report
                                    .to_command_execution_proto(true, true, false)
                                    .await,
                            ),
                            command_host_sharing_requirements: host_sharing_requirements_to_grpc(
                                prepared_command.request.host_sharing_requirements().clone(),
                            )
                            .ok(),
                            re_cache_enabled: *cacheable && re_cache_enabled,
                        };
                        ((result, cached), end)
                    })
                    .await;
                if !cached && *cacheable {
                    let info = CacheUploadInfo {
                        target: &test_target as _,
                        digest_config,
                        mergebase: &None,
                        re_platform: executor.re_platform(),
                    };
                    let _result = match executor
                        .cache_upload(
                            &info,
                            &result,
                            None,
                            None,
                            &prepared_action.action_and_blobs,
                        )
                        .await
                    {
                        Ok(result) => result,
                        Err(e) => return Err(ExecuteError::Error(e)),
                    };
                }
                result
            }
            TestStage::Testing {
                suite, testcases, ..
            } => {
                let command = executor.exec_cmd(manager, &prepared_command, cancellation);
                let test_suite = Some(TestSuite {
                    suite_name: suite.clone(),
                    test_names: testcases.clone(),
                    target_label: Some(test_target.target.as_proto()),
                });
                let start = TestRunStart {
                    suite: test_suite.clone(),
                };
                events
                    .span_async(start, async move {
                        let result = command.await;
                        let end = TestRunEnd {
                            suite: test_suite,
                            command_report: Some(
                                result
                                    .report
                                    .to_command_execution_proto(true, true, false)
                                    .await,
                            ),
                            command_host_sharing_requirements: host_sharing_requirements_to_grpc(
                                prepared_command.request.host_sharing_requirements().clone(),
                            )
                            .ok(),
                        };
                        (result, end)
                    })
                    .await
            }
        };

        let CommandExecutionResult {
            outputs,
            did_cache_upload: _,
            report:
                CommandExecutionReport {
                    std_streams,
                    exit_code,
                    status,
                    timing,
                    ..
                },
            ..
        } = command_exec_result;

        let outputs = outputs
            .into_iter()
            .filter_map(|(output, artifact)| Some((output.into_test_path()?.0, artifact)))
            .collect();

        let std_streams = std_streams
            .into_bytes()
            .await
            .buck_error_context("Error accessing test output")?;
        let stdout = ExecutionStream::Inline(std_streams.stdout);
        let stderr = ExecutionStream::Inline(std_streams.stderr);

        // If we are shutting down, we may have terminated executions and caused
        // the outcomes we are reporting (typically w/ a worker failure).
        Self::require_alive(liveliness_observer.dupe()).await?;

        Ok(match status {
            CommandExecutionStatus::Success { execution_kind } => ExecuteData {
                stdout,
                stderr,
                status: ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(0),
                },
                timing,
                execution_kind: Some(execution_kind),
                outputs,
            },
            CommandExecutionStatus::Failure { execution_kind }
            | CommandExecutionStatus::WorkerFailure { execution_kind } => ExecuteData {
                stdout,
                stderr,
                status: ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                timing,
                execution_kind: Some(execution_kind),
                outputs,
            },
            CommandExecutionStatus::TimedOut {
                duration,
                execution_kind,
            } => ExecuteData {
                stdout,
                stderr,
                status: ExecutionStatus::TimedOut { duration },
                timing,
                execution_kind: Some(execution_kind),
                outputs,
            },
            CommandExecutionStatus::Error {
                error,
                execution_kind,
                ..
            } => ExecuteData {
                stdout: ExecutionStream::Inline(Default::default()),
                stderr: ExecutionStream::Inline(format!("{error:?}").into_bytes()),
                status: ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                timing,
                execution_kind,
                outputs,
            },
            CommandExecutionStatus::Cancelled {
                execution_kind: _,
                reason,
            } => {
                let reason = reason.map(|reason| match reason {
                    CommandCancellationReason::NotSpecified => CancellationReason::NotSpecified,
                    CommandCancellationReason::ReQueueTimeout => CancellationReason::ReQueueTimeout,
                });
                return Err(ExecuteError::Cancelled(Cancelled { reason }));
            }
        })
    }

    fn executor_config_with_remote_cache_override<'a>(
        test_target_node: &'a ConfiguredTargetNode,
        executor_override: Option<&'a CommandExecutorConfig>,
        stage: &TestStage,
    ) -> buck2_error::Result<Cow<'a, CommandExecutorConfig>> {
        let executor_config = match executor_override {
            Some(o) => o,
            None => test_target_node
                .execution_platform_resolution()
                .executor_config()
                .buck_error_context("Error accessing executor config")?,
        };

        if let TestStage::Listing { .. } = &stage {
            return Ok(Cow::Borrowed(executor_config));
        }

        match &executor_config.executor {
            Executor::RemoteEnabled(options) if options.remote_cache_enabled => {
                let mut exec_options = options.clone();
                exec_options.remote_cache_enabled = false;
                let executor_config = CommandExecutorConfig {
                    executor: Executor::RemoteEnabled(exec_options),
                    options: executor_config.options.dupe(),
                };
                Ok(Cow::Owned(executor_config))
            }
            Executor::Local(_) | Executor::RemoteEnabled(_) | Executor::None => {
                Ok(Cow::Borrowed(executor_config))
            }
        }
    }

    async fn get_command_executor(
        dice: &mut DiceComputations<'_>,
        fs: &ArtifactFs,
        executor_config: &CommandExecutorConfig,
        stage: &TestStage,
    ) -> buck2_error::Result<CommandExecutor> {
        let CommandExecutorResponse {
            executor,
            platform,
            action_cache_checker,
            remote_dep_file_cache_checker: _,
            cache_uploader,
            output_trees_download_config: _,
        } = dice.get_command_executor_from_dice(executor_config).await?;

        // Caching is enabled only for listings
        let (cache_uploader, action_cache_checker) = match stage {
            TestStage::Listing { .. } => (cache_uploader, action_cache_checker),
            TestStage::Testing { .. } => (
                Arc::new(NoOpCacheUploader {}) as _,
                Arc::new(NoOpCommandOptionalExecutor {}) as _,
            ),
        };

        let executor = CommandExecutor::new(
            executor,
            action_cache_checker,
            Arc::new(NoOpCommandOptionalExecutor {}),
            cache_uploader,
            fs.clone(),
            executor_config.options,
            platform,
        );
        Ok(executor)
    }

    async fn get_local_executor(
        dice: &mut DiceComputations<'_>,
        fs: &ArtifactFs,
    ) -> buck2_error::Result<CommandExecutor> {
        let executor_config = CommandExecutorConfig {
            executor: Executor::Local(LocalExecutorOptions::default()),
            options: CommandGenerationOptions {
                path_separator: PathSeparatorKind::system_default(),
                output_paths_behavior: Default::default(),
                use_bazel_protocol_remote_persistent_workers: false,
            },
        };
        let CommandExecutorResponse {
            executor,
            platform,
            action_cache_checker: _,
            remote_dep_file_cache_checker: _,
            cache_uploader: _,
            output_trees_download_config: _,
        } = dice
            .get_command_executor_from_dice(&executor_config)
            .await?;
        let executor = CommandExecutor::new(
            executor,
            Arc::new(NoOpCommandOptionalExecutor {}),
            Arc::new(NoOpCommandOptionalExecutor {}),
            Arc::new(NoOpCacheUploader {}),
            fs.clone(),
            executor_config.options,
            platform,
        );
        Ok(executor)
    }

    async fn get_test_info(
        dice: &mut DiceComputations<'_>,
        test_target: &ConfiguredProvidersLabel,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenExternalRunnerTestInfo>> {
        dice.get_providers(test_target)
            .await?
            .require_compatible()?
            .value
            .maybe_map(|c| {
                c.as_ref()
                    .builtin_provider_value::<FrozenExternalRunnerTestInfo>()
            })
            .ok_or_else(|| {
                internal_error!("Test executable only supports ExternalRunnerTestInfo providers")
            })
    }

    async fn get_test_executor(
        dice: &mut DiceComputations<'_>,
        test_target: &ConfiguredProvidersLabel,
        test_info: &FrozenExternalRunnerTestInfo,
        executor_override: Option<Arc<ExecutorConfigOverride>>,
        fs: &ArtifactFs,
        stage: &TestStage,
    ) -> buck2_error::Result<TestExecutor> {
        // NOTE: get_providers() implicitly calls this already but it's not the end of the world
        // since this will get cached in DICE.
        let node = dice
            .get_configured_target_node(test_target.target())
            .await?
            .require_compatible()?;

        let resolved_executor_override = match executor_override {
            Some(executor_override) => Some(
                &test_info
                    .executor_override(&executor_override.name)
                    .ok_or_else(|| {
                        internal_error!("The `executor_override` provided does not exist")
                    })
                    .with_buck_error_context(|| {
                        format!(
                            "Error processing `executor_override`: `{}`",
                            executor_override.name
                        )
                    })?
                    .0,
            ),
            None => match stage {
                TestStage::Listing { .. } if test_info.has_executor_overrides() => test_info
                    .executor_override("listing")
                    .or(test_info.default_executor())
                    .map(|o| &o.0),
                _ => test_info.default_executor().map(|o| &o.0),
            },
        };

        let executor_config = Self::executor_config_with_remote_cache_override(
            &node,
            resolved_executor_override.as_ref().map(|a| &***a),
            stage,
        )?;

        let executor = Self::get_command_executor(dice, fs, &executor_config, stage)
            .await
            .buck_error_context("Error constructing CommandExecutor")?;

        Ok(TestExecutor {
            test_executor: executor,
            executor_config: executor_config.into_owned(),
        })
    }

    async fn expand_test_executable<'a>(
        dice: &mut DiceComputations<'_>,
        test_target: &ConfiguredProvidersLabel,
        test_info: &FrozenExternalRunnerTestInfo,
        cmd: Cow<'a, [ArgValue]>,
        env: Cow<'a, SortedVectorMap<String, ArgValue>>,
        pre_create_dirs: Cow<'a, [DeclaredOutput]>,
        executor_fs: &ExecutorFs<'_>,
        stage: &TestStage,
        opts: TestSessionOptions,
    ) -> buck2_error::Result<ExpandedTestExecutable> {
        let output_root = resolve_output_root(dice, test_target, stage).await?;

        let mut declared_outputs = IndexMap::<BuckOutTestPath, OutputCreationBehavior>::new();

        let mut supports_re = true;

        let cwd;
        let (expanded_cmd, expanded_env, ensured_inputs, expanded_worker) = {
            cwd = if test_info.run_from_project_root() || opts.force_run_from_project_root {
                CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("".to_owned()))
            } else {
                supports_re = false;
                // For compatibility with v1,
                let cell_resolver = dice.get_cell_resolver().await?;
                let cell = cell_resolver.get(test_target.target().pkg().cell_name())?;
                cell.path().to_buf()
            };

            let expander = Execute2RequestExpander {
                test_info,
                output_root: &output_root,
                declared_outputs: &mut declared_outputs,
                fs: executor_fs,
                cmd,
                env,
                digest_config: dice.global_data().get_digest_config(),
            };

            let inputs = expander.get_inputs()?;
            // We already built these before reaching out to tpx, so these should already be ready.
            let ensured_inputs = KeepGoing::try_compute_join_all(dice, inputs, |dice, input| {
                async move {
                    let artifact_group_value = dice.ensure_artifact_group(&input).await?;
                    buck2_error::Ok((input, artifact_group_value))
                }
                .boxed()
            })
            .await?;

            let (expanded_cmd, expanded_env, expanded_worker) = if test_info
                .use_project_relative_paths()
                || opts.force_use_project_relative_paths
            {
                expander.expand::<DefaultCommandLineContext>(&ensured_inputs)
            } else {
                supports_re = false;
                expander.expand::<AbsCommandLineContext>(&ensured_inputs)
            }?;
            (expanded_cmd, expanded_env, ensured_inputs, expanded_worker)
        };

        for output in pre_create_dirs.into_owned() {
            let test_path = BuckOutTestPath::new(output_root.clone(), output.name.into());
            declared_outputs.insert(test_path, OutputCreationBehavior::Create);
        }

        Ok(ExpandedTestExecutable {
            cwd: cwd.as_project_relative_path().to_buf(),
            cmd: expanded_cmd,
            env: expanded_env,
            ensured_inputs,
            declared_outputs,
            supports_re,
            worker: expanded_worker,
        })
    }

    async fn create_command_execution_request(
        dice: &mut DiceComputations<'_>,
        cwd: ProjectRelativePathBuf,
        cmd: Vec<String>,
        env: SortedVectorMap<String, String>,
        ensured_inputs: Vec<(ArtifactGroup, ArtifactGroupValues)>,
        declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
        fs: &ArtifactFs,
        timeout: Option<Duration>,
        host_sharing_requirements: Option<Arc<HostSharingRequirements>>,
        executor_preference: Option<ExecutorPreference>,
        required_local_resources: Vec<LocalResourceState>,
        worker: Option<WorkerSpec>,
        re_dynamic_image: Option<RemoteExecutorCustomImage>,
        meta_internal_extra_params: MetaInternalExtraParams,
    ) -> buck2_error::Result<CommandExecutionRequest> {
        let inputs = ensured_inputs
            .into_iter()
            .map(|(_, v)| CommandExecutionInput::Artifact(Box::new(v)))
            .collect_vec();

        // NOTE: This looks a bit awkward, that's because fbcode's rustfmt and ours slightly
        // disagree about format here...
        let outputs = declared_outputs
            .into_iter()
            .map(|(path, create)| CommandExecutionOutput::TestPath { path, create })
            .collect();
        let digest_config = dice.global_data().get_digest_config();
        let mut request = CommandExecutionRequest::new(
            vec![],
            cmd,
            CommandExecutionPaths::new(
                inputs,
                outputs,
                fs,
                digest_config,
                dice.per_transaction_data()
                    .get_run_action_knobs()
                    .action_paths_interner
                    .as_ref(),
            )?,
            env,
        );
        let has_resource_control = dice
            .per_transaction_data()
            .data
            .get::<HasResourceControl>()
            .unwrap()
            .0;
        request = request
            .with_working_directory(cwd)
            .with_local_environment_inheritance(EnvironmentInheritance::test_allowlist())
            .with_disable_miniperf(!has_resource_control)
            .with_worker(worker)
            .with_remote_execution_custom_image(re_dynamic_image)
            .with_meta_internal_extra_params(meta_internal_extra_params)
            .with_required_local_resources(required_local_resources)?
            .with_is_test();
        if let Some(timeout) = timeout {
            request = request.with_timeout(timeout)
        }
        if let Some(host_sharing_requirements) = host_sharing_requirements {
            request = request.with_host_sharing_requirements(host_sharing_requirements.dupe());
        }
        if let Some(executor_preference) = executor_preference {
            request = request.with_executor_preference(executor_preference);
        }
        Ok(request)
    }

    async fn setup_local_resources(
        dice: &mut DiceComputations<'_>,
        cancellation: &CancellationContext,
        required_providers: Vec<(
            &'_ ConfiguredTargetLabel,
            OwnedFrozenValueTyped<FrozenLocalResourceInfo>,
        )>,
        executor: CommandExecutor,
        default_timeout: Duration,
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> Result<Vec<LocalResourceState>, ExecuteError> {
        if required_providers.is_empty() {
            return Ok(vec![]);
        }
        let setup_commands = dice
            .try_compute_join(required_providers, |dice, provider| {
                let fs = executor.fs();
                let executor_fs = executor.executor_fs();
                async move {
                    Self::prepare_local_resource(dice, provider, fs, &executor_fs, default_timeout)
                        .await
                }
                .boxed()
            })
            .await?;

        Self::require_alive(liveliness_observer.dupe()).await?;
        let events = dice.per_transaction_data().get_dispatcher().dupe();
        let digest_config = dice.global_data().get_digest_config();

        // TODO(romanp): The code below is not optimal. We are locking the entire registry here, but we could have better concurrency.
        // For example, if different suites require different local resources and can execute in parallel, this code runs sequentially but should run in parallel.
        // An easy fix would be to introduce an RwLock instead of a mutex. In this case, suites that have the necessary resources and do not require write access
        // can be executed in parallel.
        let local_resource_state_registry = dice.get_local_resource_registry()?;
        let required_targets = setup_commands
            .iter()
            .map(|ctx| ctx.target.dupe())
            .collect::<Vec<_>>();
        let mut lock = local_resource_state_registry.0.lock().await;

        let resource_futs = setup_commands
            .into_iter()
            .filter(|ctx| !lock.contains_key(&ctx.target))
            .map(|ctx| {
                let missing_target = ctx.target.dupe();
                let setup = Self::start_local_resource(
                    events.dupe(),
                    liveliness_observer.dupe(),
                    digest_config.dupe(),
                    executor.dupe(),
                    ctx,
                    cancellation,
                );
                async move {
                    (
                        missing_target.dupe(),
                        setup.await.with_buck_error_context(|| {
                            format!(
                                "Error setting up local resource declared in `{missing_target}`"
                            )
                        }),
                    )
                }
            });
        for (target, result) in futures::future::join_all(resource_futs).await {
            lock.insert(target, result);
        }

        let result: buck2_error::Result<Vec<_>> = required_targets
            .iter()
            .map(|t| lock.get(t).unwrap().clone())
            .collect();
        Ok(result?)
    }

    async fn prepare_local_resource(
        dice: &mut DiceComputations<'_>,
        provider: (
            &ConfiguredTargetLabel,
            OwnedFrozenValueTyped<FrozenLocalResourceInfo>,
        ),
        fs: &ArtifactFs,
        executor_fs: &ExecutorFs<'_>,
        default_timeout: Duration,
    ) -> buck2_error::Result<PreparedLocalResourceSetupContext> {
        let digest_config = dice.global_data().get_digest_config();

        let (target, provider) = provider;
        let visited_inputs = {
            let setup_command_line = provider.setup_command_line();
            let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
            setup_command_line.visit_artifacts(&mut artifact_visitor)?;
            artifact_visitor.inputs
        };

        let inputs = dice
            .try_compute_join(visited_inputs, |dice, group| {
                async move { dice.ensure_artifact_group(&group).await }.boxed()
            })
            .await?;

        let artifact_path_mapping: FxHashMap<_, _> = inputs
            .iter()
            .flat_map(|v| v.iter())
            .map(|(a, v)| (a, v.content_based_path_hash()))
            .collect();
        let mut cmd: Vec<String> = vec![];
        let mut cmd_line_context = DefaultCommandLineContext::new(executor_fs);
        let setup_command_line = provider.setup_command_line();
        setup_command_line.add_to_command_line(
            &mut cmd,
            &mut cmd_line_context,
            &artifact_path_mapping,
        )?;

        let inputs = inputs
            .into_iter()
            .map(|group_values| CommandExecutionInput::Artifact(Box::new(group_values)))
            .collect();
        let paths = CommandExecutionPaths::new(
            inputs,
            indexset![],
            fs,
            digest_config,
            dice.per_transaction_data()
                .get_run_action_knobs()
                .action_paths_interner
                .as_ref(),
        )?;
        let mut execution_request =
            CommandExecutionRequest::new(vec![], cmd, paths, Default::default());
        execution_request =
            execution_request.with_timeout(provider.setup_timeout().unwrap_or(default_timeout));
        Ok(PreparedLocalResourceSetupContext {
            target: target.dupe(),
            execution_request,
            env_var_mapping: provider.env_var_mapping(),
        })
    }

    async fn start_local_resource(
        events: EventDispatcher,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        digest_config: DigestConfig,
        executor: CommandExecutor,
        context: PreparedLocalResourceSetupContext,
        cancellation: &CancellationContext,
    ) -> buck2_error::Result<LocalResourceState> {
        let manager = CommandExecutionManager::new(
            Box::new(MutexClaimManager::new()),
            events.dupe(),
            liveliness_observer,
            WaitingData::new(),
        );

        let local_resource_target = LocalResourceTarget {
            target: &context.target,
        };
        let prepared_action =
            executor.prepare_action(&context.execution_request, digest_config, false)?;
        let prepared_command = PreparedCommand {
            target: &local_resource_target as _,
            request: &context.execution_request,
            prepared_action: &prepared_action,
            digest_config,
        };
        let command = executor.exec_cmd(manager, &prepared_command, cancellation);

        let start = SetupLocalResourcesStart {
            target_label: Some(context.target.as_proto()),
        };
        let end = SetupLocalResourcesEnd {};
        let execution_result = events
            .span_async(start, async move { (command.await, end) })
            .await;

        let CommandExecutionResult {
            outputs: _,
            report:
                CommandExecutionReport {
                    std_streams,
                    exit_code,
                    status,
                    timing: _,
                    ..
                },
            ..
        } = execution_result;

        let std_streams = std_streams
            .into_bytes()
            .await
            .buck_error_context("Error accessing setup local resource output")?;

        match status {
            CommandExecutionStatus::Success { .. } => {}
            CommandExecutionStatus::Failure { .. }
            | CommandExecutionStatus::WorkerFailure { .. } => {
                return Err(buck2_error::buck2_error!(
                    ErrorTag::LocalResourceSetup,
                    "Local resource setup command failed with `{}` exit code, stdout:\n{}\nstderr:\n{}\n",
                    exit_code.unwrap_or(1),
                    String::from_utf8_lossy(&std_streams.stdout),
                    String::from_utf8_lossy(&std_streams.stderr),
                ));
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                return Err(buck2_error::buck2_error!(
                    ErrorTag::LocalResourceSetup,
                    "Local resource setup command timed out after `{}s`, stdout:\n{}\nstderr:\n{}\n",
                    duration.as_secs(),
                    String::from_utf8_lossy(&std_streams.stdout),
                    String::from_utf8_lossy(&std_streams.stderr),
                ));
            }
            CommandExecutionStatus::Error { error, .. } => {
                return Err(error);
            }
            CommandExecutionStatus::Cancelled { .. } => {
                return Err(buck2_error::buck2_error!(
                    ErrorTag::LocalResourceSetup,
                    "Local resource setup command cancelled"
                ));
            }
        };

        let string_content = String::from_utf8_lossy(&std_streams.stdout);
        let data: LocalResourcesSetupResult = serde_json::from_str(&string_content)
            // .buck_error_context("Error parsing local resource setup command output")
            .map_err(|e| from_any_with_tag(e, ErrorTag::LocalResourceSetup))?;
        let state = data.into_state(context.target.clone(), &context.env_var_mapping)?;

        Ok(state)
    }
}

impl Drop for BuckTestOrchestrator<'_> {
    fn drop(&mut self) {
        // If we didn't close the sender yet, then notify the receiver that our stream is
        // incomplete.
        let _ignored = self
            .results_channel
            .unbounded_send(Err(buck2_error::internal_error!(
                "BuckTestOrchestrator exited before end-of-tests was received",
            )));
    }
}

struct Execute2RequestExpander<'a> {
    test_info: &'a FrozenExternalRunnerTestInfo,
    output_root: &'a ForwardRelativePath,
    declared_outputs: &'a mut IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    fs: &'a ExecutorFs<'a>,
    cmd: Cow<'a, [ArgValue]>,
    env: Cow<'a, SortedVectorMap<String, ArgValue>>,
    digest_config: DigestConfig,
}

fn make_visit_arg_artifacts<'v>(
    cli_args_for_interpolation: Vec<&'v dyn CommandLineArgLike<'v>>,
    env_for_interpolation: HashMap<&'v str, &'v dyn CommandLineArgLike<'v>>,
) -> impl for<'a> Fn(&'a mut dyn CommandLineArtifactVisitor<'v>, &'a ArgValue) -> buck2_error::Result<()>
{
    move |artifact_visitor: &mut dyn CommandLineArtifactVisitor<'_>, value: &ArgValue| {
        match &value.content {
            ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::ArgHandle(h)) => {
                let arg = cli_args_for_interpolation
                    .get(h.0)
                    .ok_or_else(|| internal_error!("Invalid ArgHandle: {h:?}"))?;
                arg.visit_artifacts(artifact_visitor)?;
            }
            ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::EnvHandle(h)) => {
                let arg = env_for_interpolation
                    .get(h.0.as_str())
                    .ok_or_else(|| internal_error!("Invalid EnvHandle: {h:?}"))?;
                arg.visit_artifacts(artifact_visitor)?;
            }
            ArgValueContent::DeclaredOutput(_) | ArgValueContent::ExternalRunnerSpecValue(_) => {}
        };

        buck2_error::Ok(())
    }
}

impl<'a> Execute2RequestExpander<'a> {
    fn get_inputs(&self) -> buck2_error::Result<IndexSet<ArtifactGroup>> {
        let Execute2RequestExpander {
            test_info,
            cmd,
            env,
            ..
        } = self;
        let cli_args_for_interpolation = test_info
            .command()
            .filter_map(|c| match c {
                TestCommandMember::Literal(..) => None,
                TestCommandMember::Arglike(a) => Some(a),
            })
            .collect::<Vec<_>>();
        let env_for_interpolation = test_info.env().collect::<HashMap<_, _>>();

        let visit_arg_artifacts =
            make_visit_arg_artifacts(cli_args_for_interpolation, env_for_interpolation);

        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();
        for var in cmd.iter() {
            visit_arg_artifacts(&mut artifact_visitor, var)?;
        }

        for (_, var) in env.iter() {
            visit_arg_artifacts(&mut artifact_visitor, var)?;
        }
        let worker_exe = test_info.worker().map(|worker| worker.exe_command_line());
        if let Some(worker_exe) = worker_exe {
            worker_exe.visit_artifacts(&mut artifact_visitor)?;
        }

        Ok(artifact_visitor.inputs)
    }

    /// Expand a command and env.
    fn expand<B>(
        self,
        ensured_inputs: &Vec<(ArtifactGroup, ArtifactGroupValues)>,
    ) -> buck2_error::Result<(
        Vec<String>,
        SortedVectorMap<String, String>,
        Option<WorkerSpec>,
    )>
    where
        B: CommandLineContextExt<'a>,
    {
        let Execute2RequestExpander {
            test_info,
            output_root,
            declared_outputs,
            fs,
            cmd,
            env,
            digest_config,
        } = self;
        let cli_args_for_interpolation = test_info
            .command()
            .filter_map(|c| match c {
                TestCommandMember::Literal(..) => None,
                TestCommandMember::Arglike(a) => Some(a),
            })
            .collect::<Vec<_>>();
        let env_for_interpolation = test_info.env().collect::<HashMap<_, _>>();

        let artifact_path_mapping = ArtifactPathMapperImpl::from(ensured_inputs);

        let expand_arg_value = |cli: &mut dyn CommandLineBuilder,
                                ctx: &mut dyn CommandLineContext,
                                declared_outputs: &mut IndexMap<
            BuckOutTestPath,
            OutputCreationBehavior,
        >,
                                value: ArgValue| {
            let ArgValue { content, format } = value;

            let mut cli = CommandLineBuilderFormatWrapper { inner: cli, format };

            match content {
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::Verbatim(v)) => {
                    v.as_str()
                        .add_to_command_line(&mut cli, ctx, &artifact_path_mapping)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::ArgHandle(h)) => {
                    let arg = cli_args_for_interpolation
                        .get(h.0)
                        .ok_or_else(|| internal_error!("Invalid ArgHandle: {h:?}"))?;
                    arg.add_to_command_line(&mut cli, ctx, &artifact_path_mapping)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::EnvHandle(h)) => {
                    let arg = env_for_interpolation
                        .get(h.0.as_str())
                        .ok_or_else(|| internal_error!("Invalid EnvHandle: {h:?}"))?;
                    arg.add_to_command_line(&mut cli, ctx, &artifact_path_mapping)?;
                }
                ArgValueContent::DeclaredOutput(output) => {
                    let test_path =
                        BuckOutTestPath::new(output_root.to_owned(), output.name.clone());
                    let path = fs.fs().buck_out_path_resolver().resolve_test(&test_path);
                    cli.push_location(ctx.resolve_project_path(path)?);
                    declared_outputs.insert(test_path, OutputCreationBehavior::Parent);
                }
            };

            buck2_error::Ok(())
        };

        let mut expanded_cmd = Vec::<String>::new();
        let mut ctx = B::new(self.fs);
        for var in cmd.into_owned() {
            expand_arg_value(&mut expanded_cmd, &mut ctx, declared_outputs, var)?;
        }

        let expanded_env = env
            .into_owned()
            .into_iter()
            .map(|(k, v)| {
                let mut curr_env = String::new();
                let mut ctx = B::new(fs);
                expand_arg_value(
                    &mut SpaceSeparatedCommandLineBuilder::wrap_string(&mut curr_env),
                    &mut ctx,
                    declared_outputs,
                    v,
                )?;
                buck2_error::Ok((k, curr_env))
            })
            .collect::<Result<SortedVectorMap<_, _>, _>>()?;

        let expanded_worker = match test_info.worker() {
            Some(worker) => {
                let mut worker_rendered = Vec::<String>::new();
                let worker_exe = worker.exe_command_line();
                worker_exe.add_to_command_line(
                    &mut worker_rendered,
                    &mut ctx,
                    &artifact_path_mapping,
                )?;
                let worker_env: buck2_error::Result<SortedVectorMap<_, _>> = worker
                    .env()
                    .into_iter()
                    .map(|(k, v)| {
                        let mut env = String::new();
                        let mut ctx = DefaultCommandLineContext::new(fs);
                        v.add_to_command_line(
                            &mut SpaceSeparatedCommandLineBuilder::wrap_string(&mut env),
                            &mut ctx,
                            &artifact_path_mapping,
                        )?;
                        Ok((k.to_owned(), env))
                    })
                    .collect();

                Some(WorkerSpec {
                    exe: worker_rendered,
                    id: WorkerId(worker.id),
                    env: worker_env?,
                    concurrency: worker.concurrency(),
                    streaming: worker.streaming(),
                    remote_key: None,
                    // TODO(ianc): Support input_paths on test workers
                    input_paths: CommandExecutionPaths::new(
                        vec![],
                        indexset![],
                        fs.fs(),
                        digest_config,
                        None,
                    )?,
                })
            }
            _ => None,
        };

        Ok((expanded_cmd, expanded_env, expanded_worker))
    }
}

async fn resolve_output_root(
    dice: &mut DiceComputations<'_>,
    test_target: &ConfiguredProvidersLabel,
    stage: &TestStage,
) -> Result<ForwardRelativePathBuf, buck2_error::Error> {
    let resolver = dice.get_buck_out_path().await?;
    let output_root = match stage {
        TestStage::Listing { .. } => resolver
            .resolve_test_discovery(test_target)?
            .into_forward_relative_path_buf(),
        TestStage::Testing {
            testcases, variant, ..
        } => {
            let extra_info_path = if let Some(variant) = variant {
                format!("{}/{}", variant, testcases.join("/"),)
            } else {
                testcases.join("/")
            };
            let mut hasher = DefaultHasher::new();
            extra_info_path.hash(&mut hasher);
            let extra_info_hashed = format!("{:016x}", hasher.finish());

            resolver
                .resolve_test_execution(
                    test_target,
                    &ForwardRelativePath::unchecked_new(&extra_info_hashed),
                )?
                .into_forward_relative_path_buf()
        }
    };

    Ok(output_root)
}

trait CommandLineContextExt<'a>: CommandLineContext + 'a {
    fn new(fs: &'a ExecutorFs) -> Self;
}

impl<'a> CommandLineContextExt<'a> for DefaultCommandLineContext<'a> {
    fn new(fs: &'a ExecutorFs) -> Self {
        Self::new(fs)
    }
}

impl<'a> CommandLineContextExt<'a> for AbsCommandLineContext<'a> {
    fn new(fs: &'a ExecutorFs) -> Self {
        Self::new(fs)
    }
}

struct CommandLineBuilderFormatWrapper<'a> {
    inner: &'a mut dyn CommandLineBuilder,
    format: Option<String>,
}

impl CommandLineBuilder for CommandLineBuilderFormatWrapper<'_> {
    fn push_arg(&mut self, s: String) {
        let s = if let Some(format) = &self.format {
            format.replace("{}", &s)
        } else {
            s
        };

        self.inner.push_arg(s);
    }
}

struct ExpandedTestExecutable {
    cwd: ProjectRelativePathBuf,
    cmd: Vec<String>,
    env: SortedVectorMap<String, String>,
    ensured_inputs: Vec<(ArtifactGroup, ArtifactGroupValues)>,
    supports_re: bool,
    declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    worker: Option<WorkerSpec>,
}

fn create_prepare_for_local_execution_result(
    fs: &ArtifactFs,
    request: CommandExecutionRequest,
    local_resource_setup_commands: Vec<PreparedLocalResourceSetupContext>,
) -> PrepareForLocalExecutionResult {
    let relative_cwd = request.working_directory();
    let cwd = fs.fs().resolve(relative_cwd);
    let cmd = request.all_args_vec();

    let mut env = LossyEnvironment::new();
    apply_local_execution_environment(
        &mut env,
        &cwd,
        request.env(),
        request.local_environment_inheritance(),
    );

    let local_resource_setup_commands = local_resource_setup_commands
        .into_iter()
        .map(|r| local_resource_setup_command_prepared_for_local_execution(fs, r))
        .collect::<Vec<_>>();

    PrepareForLocalExecutionResult {
        command: LocalExecutionCommand {
            cmd,
            env: env.into_inner(),
            cwd,
        },
        local_resource_setup_commands,
    }
}

fn local_resource_setup_command_prepared_for_local_execution(
    fs: &ArtifactFs,
    resource_setup_command: PreparedLocalResourceSetupContext,
) -> LocalExecutionCommand {
    let relative_cwd = resource_setup_command.execution_request.working_directory();
    let cwd = fs.fs().resolve(relative_cwd);
    let cmd = resource_setup_command.execution_request.all_args_vec();

    let mut env = LossyEnvironment::new();
    apply_local_execution_environment(
        &mut env,
        &cwd,
        resource_setup_command.execution_request.env(),
        resource_setup_command
            .execution_request
            .local_environment_inheritance(),
    );

    LocalExecutionCommand {
        cmd,
        env: env.into_inner(),
        cwd,
    }
}

struct LossyEnvironment {
    inner: SortedVectorMap<String, String>,
}

impl LossyEnvironment {
    fn new() -> Self {
        Self {
            inner: SortedVectorMap::new(),
        }
    }

    fn into_inner(self) -> SortedVectorMap<String, String> {
        self.inner
    }
}

impl EnvironmentBuilder for LossyEnvironment {
    fn clear(&mut self) {
        self.inner.clear();
    }

    fn set<K, V>(&mut self, key: K, val: V)
    where
        K: AsRef<OsStr>,
        V: AsRef<OsStr>,
    {
        self.inner.insert(
            key.as_ref().to_string_lossy().into_owned(),
            val.as_ref().to_string_lossy().into_owned(),
        );
    }

    fn remove<K>(&mut self, key: K)
    where
        K: AsRef<OsStr>,
    {
        self.inner.remove(&*key.as_ref().to_string_lossy());
    }
}

#[derive(Debug)]
struct TestTarget<'a> {
    target: &'a ConfiguredTargetLabel,
    action_key_suffix: String,
}

impl CommandExecutionTarget for TestTarget<'_> {
    fn re_action_key(&self) -> String {
        format!("{} test {}", self.target, self.action_key_suffix)
    }

    fn re_affinity_key(&self) -> String {
        self.target.to_string()
    }

    fn as_proto_action_key(&self) -> buck2_data::ActionKey {
        buck2_data::ActionKey {
            id: Default::default(),
            owner: Some(buck2_data::action_key::Owner::TestTargetLabel(
                self.target.as_proto(),
            )),
            key: Default::default(),
        }
    }

    fn as_proto_action_name(&self) -> buck2_data::ActionName {
        buck2_data::ActionName {
            category: "test".to_owned(),
            identifier: "".to_owned(),
        }
    }
}

fn create_action_key_suffix(stage: &TestStage) -> String {
    let mut action_key_suffix = match &stage {
        TestStage::Listing { .. } => "listing".to_owned(),
        TestStage::Testing {
            testcases, variant, ..
        } => {
            if let Some(variant) = variant {
                format!("{} {}", variant, testcases.join(" "),)
            } else {
                testcases.join(" ")
            }
        }
    };
    if action_key_suffix.len() > MAX_SUFFIX_LEN {
        let truncated = "(truncated)";
        action_key_suffix.truncate(MAX_SUFFIX_LEN - truncated.len());
        action_key_suffix += truncated;
    }
    action_key_suffix
}

#[derive(Debug)]
struct LocalResourceTarget<'a> {
    target: &'a ConfiguredTargetLabel,
}

impl CommandExecutionTarget for LocalResourceTarget<'_> {
    fn re_action_key(&self) -> String {
        String::new()
    }

    fn re_affinity_key(&self) -> String {
        String::new()
    }

    fn as_proto_action_key(&self) -> buck2_data::ActionKey {
        buck2_data::ActionKey {
            id: Default::default(),
            owner: Some(buck2_data::action_key::Owner::LocalResourceSetup(
                self.target.as_proto(),
            )),
            key: Default::default(),
        }
    }

    fn as_proto_action_name(&self) -> buck2_data::ActionName {
        buck2_data::ActionName {
            category: "setup_local_resource".to_owned(),
            identifier: "".to_owned(),
        }
    }
}

struct TestExecutor {
    test_executor: CommandExecutor,
    executor_config: CommandExecutorConfig,
}

impl TestExecutor {
    pub fn re_cache_enabled(&self) -> bool {
        self.executor_config.re_cache_enabled()
    }

    pub fn executor(&self) -> &CommandExecutor {
        &self.test_executor
    }

    pub fn re_dynamic_image(&self) -> Option<RemoteExecutorCustomImage> {
        if let Executor::RemoteEnabled(options) = &self.executor_config.executor {
            options.custom_image.clone().map(|image| *image)
        } else {
            None
        }
    }

    pub fn meta_internal_extra_params(&self) -> MetaInternalExtraParams {
        if let Executor::RemoteEnabled(options) = &self.executor_config.executor {
            options.meta_internal_extra_params.clone()
        } else {
            MetaInternalExtraParams::default()
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_build_api::context::SetBuildContextData;
    use buck2_common::dice::cells::SetCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::liveliness_observer::NoopLivelinessObserver;
    use buck2_core::cells::CellResolver;
    use buck2_core::cells::name::CellName;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_execute::re::manager::UnconfiguredRemoteExecutionClient;
    use buck2_test_api::data::TestStage;
    use buck2_test_api::data::TestStatus;
    use dice::UserComputationData;
    use dice::testing::DiceBuilder;
    use futures::channel::mpsc;
    use futures::channel::mpsc::UnboundedReceiver;
    use futures::future;
    use futures::stream::TryStreamExt;

    use super::*;

    async fn make() -> buck2_error::Result<(
        BuckTestOrchestrator<'static>,
        UnboundedReceiver<buck2_error::Result<ExecutorMessage>>,
    )> {
        let fs = ProjectRootTemp::new().unwrap();

        let cell_resolver = CellResolver::testing_with_name_and_path(
            CellName::testing_new("cell"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
        );
        let buckout_path = ProjectRelativePathBuf::unchecked_new("buck_out/v2".into());
        let mut dice = DiceBuilder::new()
            .set_data(|d| d.set_testing_io_provider(&fs))
            .build(UserComputationData::new())
            .unwrap();
        dice.set_buck_out_path(Some(buckout_path))?;
        dice.set_cell_resolver(cell_resolver)?;

        let dice = dice.commit().await;

        let (sender, receiver) = mpsc::unbounded();

        let re_client = Arc::new(remote_storage::ReClientWithCache::new(
            UnconfiguredRemoteExecutionClient::testing_new_dummy(),
        ));

        Ok((
            BuckTestOrchestrator::from_parts(
                dice,
                Arc::new(TestSession::new(Default::default())),
                NoopLivelinessObserver::create(),
                sender,
                EventDispatcher::null(),
                CancellationContext::testing(),
                re_client,
            ),
            receiver,
        ))
    }

    #[tokio::test]
    async fn orchestrator_results() -> buck2_error::Result<()> {
        let (orchestrator, channel) = make().await?;

        let target =
            ConfiguredTargetLabel::testing_parse("cell//pkg:foo", ConfigurationData::testing_new());

        let target = ConfiguredProvidersLabel::new(target, Default::default());
        let target = orchestrator.session.register(target);

        let jobs = async {
            orchestrator
                .report_test_result(TestResult {
                    target,
                    status: TestStatus::PASS,
                    msg: None,
                    name: "First - test".to_owned(),
                    duration: Some(Duration::from_micros(1)),
                    details: "1".to_owned(),
                    max_memory_used_bytes: None,
                })
                .await?;

            orchestrator
                .report_test_result(TestResult {
                    target,
                    status: TestStatus::FAIL,
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2)),
                    details: "2".to_owned(),
                    max_memory_used_bytes: None,
                })
                .await?;

            orchestrator.end_of_test_results(0).await?;

            buck2_error::Ok(())
        };

        let ((), results) = future::try_join(jobs, channel.try_collect::<Vec<_>>()).await?;

        assert_eq!(
            results,
            vec![
                ExecutorMessage::TestResult(TestResult {
                    target,

                    status: TestStatus::PASS,
                    msg: None,
                    name: "First - test".to_owned(),
                    duration: Some(Duration::from_micros(1)),
                    details: "1".to_owned(),
                    max_memory_used_bytes: None,
                }),
                ExecutorMessage::TestResult(TestResult {
                    target,

                    status: TestStatus::FAIL,
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2)),
                    details: "2".to_owned(),
                    max_memory_used_bytes: None,
                }),
                ExecutorMessage::ExitCode(0),
            ]
        );

        Ok(())
    }

    #[tokio::test]
    async fn orchestrator_attach_info_messages() -> buck2_error::Result<()> {
        let (orchestrator, channel) = make().await?;

        let jobs = async {
            orchestrator.attach_info_message("yolo".to_owned()).await?;

            orchestrator.end_of_test_results(0).await?;

            buck2_error::Ok(())
        };

        let ((), results) = future::try_join(jobs, channel.try_collect::<Vec<_>>()).await?;

        assert_eq!(
            results,
            vec![
                ExecutorMessage::InfoMessage("yolo".to_owned(),),
                ExecutorMessage::ExitCode(0),
            ]
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_orchestrator_channel_drop() -> buck2_error::Result<()> {
        let (orchestrator, channel) = make().await?;
        drop(orchestrator);

        let res = channel.try_collect::<Vec<_>>().await;
        assert!(res.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_orchestrator_closes_channel() -> buck2_error::Result<()> {
        let (orchestrator, channel) = make().await?;
        let sender = orchestrator.results_channel.clone();
        orchestrator.end_of_test_results(1).await?;

        assert!(sender.is_closed());
        drop(channel);

        Ok(())
    }

    #[test]
    fn test_create_action_key_suffix_listing() {
        let stage = TestStage::Listing {
            suite: "test_suite".to_owned(),
            cacheable: true,
        };
        assert_eq!(create_action_key_suffix(&stage), "listing");
    }

    #[test]
    fn test_create_action_key_suffix_testing_no_variant() {
        let stage = TestStage::Testing {
            suite: "test_suite".to_owned(),
            testcases: vec!["test1".to_owned(), "test2".to_owned()],
            variant: None,
        };
        assert_eq!(create_action_key_suffix(&stage), "test1 test2");
    }

    #[test]
    fn test_create_action_key_suffix_testing_with_variant() {
        let stage = TestStage::Testing {
            suite: "test_suite".to_owned(),
            testcases: vec!["test1".to_owned(), "test2".to_owned()],
            variant: Some("variant1".to_owned()),
        };
        assert_eq!(create_action_key_suffix(&stage), "variant1 test1 test2");
    }

    #[test]
    fn test_create_action_key_suffix_truncation() {
        let long_testcase = "a".repeat(MAX_SUFFIX_LEN + 100);
        let stage = TestStage::Testing {
            suite: "test_suite".to_owned(),
            testcases: vec![long_testcase],
            variant: None,
        };
        let result = create_action_key_suffix(&stage);
        assert_eq!(result.len(), MAX_SUFFIX_LEN);
        assert!(result.ends_with("(truncated)"));
    }
}
