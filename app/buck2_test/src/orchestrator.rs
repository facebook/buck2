/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// https://github.com/rust-lang/rust-clippy/issues/12806
#![allow(clippy::unnecessary_to_owned)]

//! Implementation of the `TestOrchestrator` from `buck2_test_api`.

use std::borrow::Cow;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fmt::Display;
use std::ops::ControlFlow;
use std::ops::DerefMut;
use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_build_api::actions::execute::dice_data::CommandExecutorResponse;
use buck2_build_api::actions::execute::dice_data::DiceHasCommandExecutor;
use buck2_build_api::actions::execute::dice_data::GetReClient;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::artifact_groups::calculation::ArtifactGroupCalculation;
use buck2_build_api::artifact_groups::ArtifactGroup;
use buck2_build_api::context::HasBuildContextData;
use buck2_build_api::interpreter::rule_defs::cmd_args::space_separated::SpaceSeparatedCommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::AbsCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use buck2_build_api::interpreter::rule_defs::cmd_args::CommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::FrozenExternalRunnerTestInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::external_runner_test_info::TestCommandMember;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::events::HasEvents;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_common::liveliness_observer::LivelinessObserver;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::execution_types::executor_config::CommandGenerationOptions;
use buck2_core::execution_types::executor_config::Executor;
use buck2_core::execution_types::executor_config::LocalExecutorOptions;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::execution_types::executor_config::RemoteExecutorCustomImage;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutTestPath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::configured_or_unconfigured::ConfiguredOrUnconfiguredTargetLabel;
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
use buck2_execute::execute::result::CommandExecutionMetadata;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute_impl::executors::local::apply_local_execution_environment;
use buck2_execute_impl::executors::local::create_output_dirs;
use buck2_execute_impl::executors::local::materialize_inputs;
use buck2_execute_impl::executors::local::EnvironmentBuilder;
use buck2_futures::cancellation::CancellationContext;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_test_api::data::ArgValue;
use buck2_test_api::data::ArgValueContent;
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
use buck2_test_api::protocol::TestOrchestrator;
use derive_more::From;
use dice::DiceComputations;
use dice::DiceTransaction;
use dice::Key;
use display_container::fmt_container;
use display_container::fmt_keyed_container;
use dupe::Dupe;
use futures::channel::mpsc::UnboundedSender;
use futures::stream::FuturesUnordered;
use futures::stream::StreamExt;
use futures::FutureExt;
use host_sharing::HostSharingRequirements;
use indexmap::indexset;
use indexmap::IndexMap;
use indexmap::IndexSet;
use sorted_vector_map::SortedVectorMap;
use starlark::values::FrozenRef;
use uuid::Uuid;

use crate::local_resource_api::LocalResourcesSetupResult;
use crate::local_resource_registry::HasLocalResourceRegistry;
use crate::local_resource_setup::required_local_resources_setup_contexts;
use crate::local_resource_setup::LocalResourceSetupContext;
use crate::local_resource_setup::TestStageSimple;
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
    results_channel: UnboundedSender<anyhow::Result<ExecutorMessage>>,
    events: EventDispatcher,
    liveliness_observer: Arc<dyn LivelinessObserver>,
    cancellations: &'a CancellationContext<'a>,
}

impl<'a> BuckTestOrchestrator<'a> {
    pub async fn new(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        results_channel: UnboundedSender<anyhow::Result<ExecutorMessage>>,
        cancellations: &'a CancellationContext<'a>,
    ) -> anyhow::Result<BuckTestOrchestrator<'a>> {
        let events = dice.per_transaction_data().get_dispatcher().dupe();
        Ok(Self::from_parts(
            dice,
            session,
            liveliness_observer,
            results_channel,
            events,
            cancellations,
        ))
    }

    fn from_parts(
        dice: DiceTransaction,
        session: Arc<TestSession>,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        results_channel: UnboundedSender<anyhow::Result<ExecutorMessage>>,
        events: EventDispatcher,
        cancellations: &'a CancellationContext,
    ) -> BuckTestOrchestrator<'a> {
        Self {
            dice,
            session,
            results_channel,
            events,
            liveliness_observer,
            cancellations,
        }
    }

    async fn require_alive(
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> Result<(), Cancelled> {
        if !liveliness_observer.is_alive().await {
            return Err(Cancelled);
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

        let fs = self
            .dice
            .clone()
            .get_artifact_fs()
            .await
            .map_err(anyhow::Error::from)?;
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
                prefix: TestExecutionPrefix::new(&stage, &self.session),
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
                    let future = async move {
                        let _unused = remote_storage::apply_config(
                            self.dice.per_transaction_data().get_re_client(),
                            &artifact,
                            &remote_storage_config,
                        )
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
            .buck_error_context_anyhow("Error materializing test outputs")?;

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
        })
    }

    async fn prepare_and_execute_no_dice(
        dice: &mut DiceComputations<'_>,
        key: TestExecutionKey,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        cancellation: &CancellationContext<'_>,
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
            prefix,
            timeout,
            host_sharing_requirements,
        } = key;
        let fs = dice.get_artifact_fs().await.map_err(anyhow::Error::from)?;
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
            prefix,
            options,
        )
        .boxed()
        .await?;
        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            supports_re,
            declared_outputs,
            worker,
        } = test_executable_expanded;
        let executor_preference = Self::executor_preference(options, supports_re)?;
        let required_resources = if test_executor
            .executor()
            .is_local_execution_possible(executor_preference)
        {
            let setup_local_resources_executor = Self::get_local_executor(dice, &fs).await?;
            let simple_stage = stage.as_ref().into();

            let setup_contexts = {
                let executor_fs = setup_local_resources_executor.executor_fs();
                required_local_resources_setup_contexts(
                    dice,
                    &executor_fs,
                    &test_info,
                    &required_local_resources,
                    &simple_stage,
                )
                .await?
            };
            // If some timeout is neeeded, use the same value as for the test itself which is better than nothing.
            Self::setup_local_resources(
                dice,
                cancellation,
                setup_contexts,
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
            inputs,
            declared_outputs,
            &fs,
            Some(timeout),
            Some(host_sharing_requirements),
            Some(executor_preference),
            required_resources,
            worker,
            test_executor.re_dynamic_image(),
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
    prefix: TestExecutionPrefix,
    timeout: Duration,
    host_sharing_requirements: Arc<HostSharingRequirements>,
}

#[derive(Clone, Dupe, Debug, Eq, Hash, PartialEq, Allocative)]
enum TestExecutionPrefix {
    Listing,
    Testing(Arc<ForwardRelativePathBuf>),
}

impl TestExecutionPrefix {
    fn new(stage: &TestStage, session: &TestSession) -> Self {
        match stage {
            TestStage::Listing(_) => TestExecutionPrefix::Listing,
            TestStage::Testing { .. } => TestExecutionPrefix::Testing(session.prefix().dupe()),
        }
    }
}

impl Display for TestExecutionPrefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestExecutionPrefix::Listing => write!(f, "Listing"),
            TestExecutionPrefix::Testing(prefix) => write!(f, "Testing({})", prefix),
        }
    }
}

#[async_trait]
impl Key for TestExecutionKey {
    type Value = buck2_error::Result<Arc<ExecuteData>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        cancellations: &CancellationContext,
    ) -> Self::Value {
        Ok(cancellations
            .with_structured_cancellation(|observer| {
                async move {
                    let result = BuckTestOrchestrator::prepare_and_execute_no_dice(
                        ctx,
                        self.dupe(),
                        Arc::new(observer),
                        cancellations,
                    )
                    .await;
                    let result: anyhow::Result<Arc<ExecuteData>> = match result {
                        Ok(ok) => Ok(Arc::new(ok)),
                        Err(err) => match err {
                            ExecuteError::Error(err) => Err(err)?,
                            ExecuteError::Cancelled(_) => Err(ExecuteDiceErr::Cancelled)?,
                        },
                    };
                    result
                }
                .boxed()
            })
            .await?)
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        false
    }
}

async fn prepare_and_execute(
    ctx: &mut DiceComputations<'static>,
    cancellation: &CancellationContext<'_>,
    key: TestExecutionKey,
    liveliness_observer: Arc<dyn LivelinessObserver>,
) -> Result<ExecuteData, ExecuteError> {
    let execute_on_dice = match key.stage.as_ref() {
        TestStage::Listing(_) => check_cache_listings_experiment(ctx, &key.test_target).await?,
        TestStage::Testing { .. } => false,
    };
    if execute_on_dice {
        let result = tokio::select! {
            _ = liveliness_observer.while_alive() => {
                Err(ExecuteError::Cancelled(Cancelled))
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
    let result = ctx.compute(key).await;
    let result = result.map_err(anyhow::Error::from)?;

    result.map_err(anyhow::Error::from).map_err(|err| {
        if err.downcast_ref::<ExecuteDiceErr>().is_some() {
            ExecuteError::Cancelled(Cancelled)
        } else {
            ExecuteError::Error(err)
        }
    })
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
            "stage = {}, options = {}, prefix = {}, timeout = {}, host_sharing_requirements = {}",
            self.stage,
            self.options,
            self.prefix,
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

// A token used to implement From
struct Cancelled;

// NOTE: This doesn't implement Error so that we can't accidentally lose the Cancelled variant.
#[derive(From)]
enum ExecuteError {
    Error(anyhow::Error),
    Cancelled(Cancelled),
}

#[derive(From, Debug, buck2_error::Error)]
/// Used to support the same ExecuteError's api via dice
enum ExecuteDiceErr {
    #[error("Cancelled")]
    Cancelled,
}

#[async_trait]
impl<'a> TestOrchestrator for BuckTestOrchestrator<'a> {
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
    ) -> anyhow::Result<ExecuteResponse> {
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
            Err(ExecuteError::Cancelled(Cancelled)) => Ok(ExecuteResponse::Cancelled),
            Err(ExecuteError::Error(e)) => Err(e),
        }
    }

    async fn report_test_result(&self, r: TestResult) -> anyhow::Result<()> {
        let event = buck2_data::instant_event::Data::TestResult(translations::convert_test_result(
            r.clone(),
            &self.session,
        )?);
        self.events.instant_event(event);
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::TestResult(r)))
            .map_err(|_| anyhow::Error::msg("Test result was received after end-of-tests"))?;
        Ok(())
    }

    async fn report_tests_discovered(
        &self,
        test_target: ConfiguredTargetHandle,
        suite: String,
        names: Vec<String>,
    ) -> anyhow::Result<()> {
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

    async fn report_test_session(&self, session_info: String) -> anyhow::Result<()> {
        self.events.instant_event(TestDiscovery {
            data: Some(buck2_data::test_discovery::Data::Session(TestSessionInfo {
                info: session_info,
            })),
        });

        Ok(())
    }

    async fn end_of_test_results(&self, exit_code: i32) -> anyhow::Result<()> {
        self.events.instant_event(EndOfTestResults { exit_code });
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::ExitCode(exit_code)))
            .map_err(|_| anyhow::Error::msg("end_of_tests was received twice"))?;
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
    ) -> anyhow::Result<PrepareForLocalExecutionResult> {
        let test_target = self.session.get(test_target)?;

        let fs = self.dice.clone().get_artifact_fs().await?;

        let test_info = Self::get_test_info(self.dice.dupe().deref_mut(), &test_target).await?;

        // In contrast from actual test execution we do not check if local execution is possible.
        // We leave that decision to actual local execution runner that requests local execution preparation.
        let setup_local_resources_executor =
            Self::get_local_executor(self.dice.dupe().deref_mut(), &fs).await?;
        let setup_contexts = {
            let executor_fs = setup_local_resources_executor.executor_fs();
            required_local_resources_setup_contexts(
                self.dice.dupe().deref_mut(),
                &executor_fs,
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
            .try_compute_join(setup_contexts, |dice, context| {
                let fs = fs.clone();
                async move {
                    Self::prepare_local_resource(dice, context, &fs, Duration::default()).await
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
            TestExecutionPrefix::new(&stage, &self.session),
            self.session.options(),
        )
        .await?;

        let ExpandedTestExecutable {
            cwd,
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
            supports_re: _,
            declared_outputs,
            worker,
        } = test_executable_expanded;

        let execution_request = Self::create_command_execution_request(
            self.dice.dupe().deref_mut(),
            cwd,
            expanded_cmd,
            expanded_env,
            inputs,
            declared_outputs,
            &fs,
            None,
            None,
            None,
            vec![],
            worker,
            test_executor.re_dynamic_image(),
        )
        .await?;

        let materializer = self.dice.per_transaction_data().get_materializer();
        let blocking_executor = self.dice.get_blocking_executor();

        materialize_inputs(&fs, materializer.as_ref(), &execution_request).await?;

        create_output_dirs(
            &fs,
            &execution_request,
            materializer.dupe(),
            blocking_executor,
            self.cancellations,
        )
        .await?;

        for local_resource_setup_command in setup_commands.iter() {
            materialize_inputs(
                &fs,
                materializer.as_ref(),
                &local_resource_setup_command.execution_request,
            )
            .await?;
            let blocking_executor = self.dice.get_blocking_executor();

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

    async fn attach_info_message(&self, message: String) -> anyhow::Result<()> {
        self.results_channel
            .unbounded_send(Ok(ExecutorMessage::InfoMessage(message)))
            .map_err(|_| anyhow::Error::msg("Message received after end-of-tests"))?;
        Ok(())
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

impl<'b> BuckTestOrchestrator<'b> {
    fn executor_preference(
        opts: TestSessionOptions,
        test_supports_re: bool,
    ) -> anyhow::Result<ExecutorPreference> {
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
        cancellation: &CancellationContext<'_>,
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
        );
        let digest_config = dice.global_data().get_digest_config();

        let mut action_key_suffix = match &stage {
            TestStage::Listing(_) => "listing".to_owned(),
            TestStage::Testing { testcases, .. } => testcases.join(" "),
        };
        if action_key_suffix.len() > MAX_SUFFIX_LEN {
            let truncated = "(truncated)";
            action_key_suffix.truncate(MAX_SUFFIX_LEN - truncated.len());
            action_key_suffix += truncated;
        }

        let test_target = TestTarget {
            target: test_target_label.target(),
            action_key_suffix,
        };

        // For test execution, we currently do not do any cache queries

        let prepared_action = match executor.prepare_action(&request, digest_config) {
            Ok(prepared_action) => prepared_action,
            Err(e) => return Err(ExecuteError::Error(e.into())),
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
            TestStage::Listing(listing) => {
                let start = TestDiscoveryStart {
                    suite_name: listing.clone(),
                };
                let (result, cached) = events
                    .span_async(start, async move {
                        let (result, cached) = match executor
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
                        };
                        let end = TestDiscoveryEnd {
                            suite_name: listing.clone(),
                            command_report: Some(
                                result
                                    .report
                                    .to_command_execution_proto(true, true, false)
                                    .await,
                            ),
                            re_cache_enabled,
                        };
                        ((result, cached), end)
                    })
                    .await;
                if !cached && check_cache_listings_experiment(dice, &test_target_label).await? {
                    let info = CacheUploadInfo {
                        target: &test_target as _,
                        digest_config,
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
                        Err(e) => return Err(ExecuteError::Error(e.into())),
                    };
                }
                result
            }
            TestStage::Testing { suite, testcases } => {
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
            .buck_error_context_anyhow("Error accessing test output")?;
        let stdout = ExecutionStream::Inline(std_streams.stdout);
        let stderr = ExecutionStream::Inline(std_streams.stderr);

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
            CommandExecutionStatus::Failure { execution_kind } => ExecuteData {
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
                stderr: ExecutionStream::Inline(format!("{:?}", error).into_bytes()),
                status: ExecutionStatus::Finished {
                    exitcode: exit_code.unwrap_or(1),
                },
                timing,
                execution_kind,
                outputs,
            },
            CommandExecutionStatus::Cancelled => {
                return Err(ExecuteError::Cancelled(Cancelled));
            }
        })
    }

    fn executor_config_with_remote_cache_override<'a>(
        test_target_node: &'a ConfiguredTargetNode,
        executor_override: Option<&'a CommandExecutorConfig>,
        stage: &TestStage,
    ) -> anyhow::Result<Cow<'a, CommandExecutorConfig>> {
        let executor_config = match executor_override {
            Some(o) => o,
            None => test_target_node
                .execution_platform_resolution()
                .executor_config()
                .buck_error_context_anyhow("Error accessing executor config")?,
        };

        if let TestStage::Listing(_) = &stage {
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
            Executor::Local(_) | Executor::RemoteEnabled(_) => Ok(Cow::Borrowed(executor_config)),
        }
    }

    async fn get_command_executor(
        dice: &mut DiceComputations<'_>,
        fs: &ArtifactFs,
        executor_config: &CommandExecutorConfig,
        stage: &TestStage,
    ) -> anyhow::Result<CommandExecutor> {
        let CommandExecutorResponse {
            executor,
            platform,
            cache_checker,
            cache_uploader,
        } = dice.get_command_executor_from_dice(executor_config).await?;

        // Caching is enabled only for listings
        let (cache_uploader, cache_checker) = match stage {
            TestStage::Listing(_) => (cache_uploader, cache_checker),
            TestStage::Testing { .. } => (
                Arc::new(NoOpCacheUploader {}) as _,
                Arc::new(NoOpCommandOptionalExecutor {}) as _,
            ),
        };

        let executor = CommandExecutor::new(
            executor,
            cache_checker,
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
    ) -> anyhow::Result<CommandExecutor> {
        let executor_config = CommandExecutorConfig {
            executor: Executor::Local(LocalExecutorOptions::default()),
            options: CommandGenerationOptions {
                path_separator: PathSeparatorKind::system_default(),
                output_paths_behavior: Default::default(),
                use_remote_persistent_workers: false,
            },
        };
        let CommandExecutorResponse {
            executor,
            platform,
            cache_checker: _,
            cache_uploader: _,
        } = dice
            .get_command_executor_from_dice(&executor_config)
            .await?;
        let executor = CommandExecutor::new(
            executor,
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
    ) -> anyhow::Result<FrozenRef<'static, FrozenExternalRunnerTestInfo>> {
        let providers = dice
            .get_providers(test_target)
            .await?
            .require_compatible()?;

        let providers = providers.provider_collection();
        providers
            .builtin_provider::<FrozenExternalRunnerTestInfo>()
            .context("Test executable only supports ExternalRunnerTestInfo providers")
    }

    async fn get_test_executor(
        dice: &mut DiceComputations<'_>,
        test_target: &ConfiguredProvidersLabel,
        test_info: &FrozenExternalRunnerTestInfo,
        executor_override: Option<Arc<ExecutorConfigOverride>>,
        fs: &ArtifactFs,
        stage: &TestStage,
    ) -> anyhow::Result<TestExecutor> {
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
                    .context("The `executor_override` provided does not exist")
                    .with_context(|| {
                        format!(
                            "Error processing `executor_override`: `{}`",
                            executor_override.name
                        )
                    })?
                    .0,
            ),
            None => test_info.default_executor().map(|o| &o.0),
        };

        let executor_config = Self::executor_config_with_remote_cache_override(
            &node,
            resolved_executor_override.as_ref().map(|a| &***a),
            &stage,
        )?;

        let executor = Self::get_command_executor(dice, fs, &executor_config, stage)
            .await
            .context("Error constructing CommandExecutor")?;

        Ok(TestExecutor {
            test_executor: executor,
            executor_config: executor_config.into_owned(),
        })
    }

    async fn expand_test_executable<'a>(
        dice: &mut DiceComputations<'_>,
        test_target: &ConfiguredProvidersLabel,
        test_info: &FrozenExternalRunnerTestInfo,
        cmd: Cow<'a, Vec<ArgValue>>,
        env: Cow<'a, SortedVectorMap<String, ArgValue>>,
        pre_create_dirs: Cow<'a, Vec<DeclaredOutput>>,
        executor_fs: &ExecutorFs<'_>,
        prefix: TestExecutionPrefix,
        opts: TestSessionOptions,
    ) -> anyhow::Result<ExpandedTestExecutable> {
        let output_root = resolve_output_root(dice, test_target, prefix).await?;

        let mut declared_outputs = IndexMap::<BuckOutTestPath, OutputCreationBehavior>::new();

        let mut supports_re = true;

        let cwd;
        let expanded;

        {
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
            };

            expanded = if test_info.use_project_relative_paths()
                || opts.force_use_project_relative_paths
            {
                expander.expand::<DefaultCommandLineContext>()
            } else {
                supports_re = false;
                expander.expand::<AbsCommandLineContext>()
            }?;
        };

        let (expanded_cmd, expanded_env, inputs, expanded_worker) = expanded;

        for output in pre_create_dirs.into_owned() {
            let test_path = BuckOutTestPath::new(output_root.clone(), output.name.into());
            declared_outputs.insert(test_path, OutputCreationBehavior::Create);
        }

        Ok(ExpandedTestExecutable {
            cwd: cwd.as_project_relative_path().to_buf(),
            cmd: expanded_cmd,
            env: expanded_env,
            inputs,
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
        cmd_inputs: IndexSet<ArtifactGroup>,
        declared_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
        fs: &ArtifactFs,
        timeout: Option<Duration>,
        host_sharing_requirements: Option<Arc<HostSharingRequirements>>,
        executor_preference: Option<ExecutorPreference>,
        required_local_resources: Vec<LocalResourceState>,
        worker: Option<WorkerSpec>,
        re_dynamic_image: Option<RemoteExecutorCustomImage>,
    ) -> anyhow::Result<CommandExecutionRequest> {
        let mut inputs = Vec::with_capacity(cmd_inputs.len());
        for input in &cmd_inputs {
            // we already built these before reaching out to tpx, so these should already be ready
            // hence we don't actually need to spawn these in parallel
            // TODO (T102328660): Does CommandExecutionRequest need this artifact?
            inputs.push(CommandExecutionInput::Artifact(Box::new(
                dice.ensure_artifact_group(input).await?,
            )));
        }

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
            CommandExecutionPaths::new(inputs, outputs, fs, digest_config)?,
            env,
        );
        request = request
            .with_working_directory(cwd)
            .with_local_environment_inheritance(EnvironmentInheritance::test_allowlist())
            .with_disable_miniperf(true)
            .with_worker(worker)
            .with_remote_execution_custom_image(re_dynamic_image)
            .with_required_local_resources(required_local_resources)?;
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
        cancellation: &CancellationContext<'_>,
        setup_contexts: Vec<LocalResourceSetupContext>,
        executor: CommandExecutor,
        default_timeout: Duration,
        liveliness_observer: Arc<dyn LivelinessObserver>,
    ) -> Result<Vec<LocalResourceState>, ExecuteError> {
        if setup_contexts.is_empty() {
            return Ok(vec![]);
        }
        let setup_commands = dice
            .try_compute_join(setup_contexts, |dice, context| {
                let fs = executor.fs();
                async move {
                        Self::prepare_local_resource(dice, context, &fs, default_timeout).await
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
        let local_resource_state_registry = dice.get_local_resource_registry();
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
                        setup
                            .await
                            .with_buck_error_context_anyhow(|| {
                                format!(
                                    "Error setting up local resource declared in `{}`",
                                    missing_target
                                )
                            })
                            .map_err(buck2_error::Error::from),
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
        Ok(result.map_err(anyhow::Error::from)?)
    }

    async fn prepare_local_resource(
        dice: &mut DiceComputations<'_>,
        context: LocalResourceSetupContext,
        fs: &ArtifactFs,
        default_timeout: Duration,
    ) -> anyhow::Result<PreparedLocalResourceSetupContext> {
        let digest_config = dice.global_data().get_digest_config();

        let inputs = dice
            .try_compute_join(context.input_artifacts, |dice, group| {
                async move { dice.ensure_artifact_group(&group).await }.boxed()
            })
            .await?;
        let inputs = inputs
            .into_iter()
            .map(|group_values| CommandExecutionInput::Artifact(Box::new(group_values)))
            .collect();
        let paths = CommandExecutionPaths::new(inputs, indexset![], fs, digest_config)?;
        let mut execution_request =
            CommandExecutionRequest::new(vec![], context.cmd, paths, Default::default());
        execution_request =
            execution_request.with_timeout(context.timeout.unwrap_or(default_timeout));
        Ok(PreparedLocalResourceSetupContext {
            target: context.target,
            execution_request,
            env_var_mapping: context.env_var_mapping,
        })
    }

    async fn start_local_resource(
        events: EventDispatcher,
        liveliness_observer: Arc<dyn LivelinessObserver>,
        digest_config: DigestConfig,
        executor: CommandExecutor,
        context: PreparedLocalResourceSetupContext,
        cancellation: &CancellationContext<'_>,
    ) -> buck2_error::Result<LocalResourceState> {
        let manager = CommandExecutionManager::new(
            Box::new(MutexClaimManager::new()),
            events.dupe(),
            liveliness_observer,
        );

        let local_resource_target = LocalResourceTarget {
            target: &context.target,
        };
        let prepared_action = executor.prepare_action(&context.execution_request, digest_config)?;
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
            .buck_error_context_anyhow("Error accessing setup local resource output")?;

        match status {
            CommandExecutionStatus::Success { .. } => {}
            CommandExecutionStatus::Failure { .. } => {
                return Err(anyhow::anyhow!(
                    "Local resource setup command failed with `{}` exit code, stdout:\n{}\nstderr:\n{}\n",
                    exit_code.unwrap_or(1),
                    String::from_utf8_lossy(&std_streams.stdout),
                    String::from_utf8_lossy(&std_streams.stderr),
                ).into());
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                return Err(anyhow::anyhow!(
                    "Local resource setup command timed out after `{}s`, stdout:\n{}\nstderr:\n{}\n",
                    duration.as_secs(),
                    String::from_utf8_lossy(&std_streams.stdout),
                    String::from_utf8_lossy(&std_streams.stderr),
                ).into());
            }
            CommandExecutionStatus::Error { error, .. } => {
                return Err(error.into());
            }
            CommandExecutionStatus::Cancelled => {
                return Err(anyhow::anyhow!("Local resource setup command cancelled").into());
            }
        };

        let string_content = String::from_utf8_lossy(&std_streams.stdout);
        let data: LocalResourcesSetupResult = serde_json::from_str(&string_content)
            .context("Error parsing local resource setup command output")?;
        let state = data.into_state(context.target.clone(), &context.env_var_mapping)?;

        Ok(state)
    }
}

impl<'a> Drop for BuckTestOrchestrator<'a> {
    fn drop(&mut self) {
        // If we didn't close the sender yet, then notify the receiver that our stream is
        // incomplete.
        let _ignored = self.results_channel.unbounded_send(Err(anyhow::Error::msg(
            "BuckTestOrchestrator exited before end-of-tests was received",
        )));
    }
}

struct Execute2RequestExpander<'a> {
    test_info: &'a FrozenExternalRunnerTestInfo,
    output_root: &'a ForwardRelativePath,
    declared_outputs: &'a mut IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    fs: &'a ExecutorFs<'a>,
    cmd: Cow<'a, Vec<ArgValue>>,
    env: Cow<'a, SortedVectorMap<String, ArgValue>>,
}

impl<'a> Execute2RequestExpander<'a> {
    /// Expand a command and env. Return CLI, env, and inputs.
    fn expand<B>(
        self,
    ) -> anyhow::Result<(
        Vec<String>,
        SortedVectorMap<String, String>,
        IndexSet<ArtifactGroup>,
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
        } = self;
        let cli_args_for_interpolation = test_info
            .command()
            .filter_map(|c| match c {
                TestCommandMember::Literal(..) => None,
                TestCommandMember::Arglike(a) => Some(a),
            })
            .collect::<Vec<_>>();

        let env_for_interpolation = test_info.env().collect::<HashMap<_, _>>();

        let expand_arg_value = |cli: &mut dyn CommandLineBuilder,
                                ctx: &mut dyn CommandLineContext,
                                artifact_visitor: &mut dyn CommandLineArtifactVisitor,
                                declared_outputs: &mut IndexMap<
            BuckOutTestPath,
            OutputCreationBehavior,
        >,
                                value: ArgValue| {
            let ArgValue { content, format } = value;

            let mut cli = CommandLineBuilderFormatWrapper { inner: cli, format };

            match content {
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::Verbatim(v)) => {
                    v.as_str().add_to_command_line(&mut cli, ctx)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::ArgHandle(h)) => {
                    let arg = cli_args_for_interpolation
                        .get(h.0)
                        .with_context(|| format!("Invalid ArgHandle: {:?}", h))?;

                    arg.visit_artifacts(artifact_visitor)?;
                    arg.add_to_command_line(&mut cli, ctx)?;
                }
                ArgValueContent::ExternalRunnerSpecValue(ExternalRunnerSpecValue::EnvHandle(h)) => {
                    let arg = env_for_interpolation
                        .get(h.0.as_str())
                        .with_context(|| format!("Invalid EnvHandle: {:?}", h))?;
                    arg.visit_artifacts(artifact_visitor)?;
                    arg.add_to_command_line(&mut cli, ctx)?;
                }
                ArgValueContent::DeclaredOutput(output) => {
                    let test_path =
                        BuckOutTestPath::new(output_root.to_owned(), output.name.into());
                    let path = fs.fs().buck_out_path_resolver().resolve_test(&test_path);
                    let path = ctx.resolve_project_path(path)?.into_string();
                    cli.push_arg(path);
                    declared_outputs.insert(test_path, OutputCreationBehavior::Parent);
                }
            };

            anyhow::Ok(())
        };

        let mut artifact_visitor = SimpleCommandLineArtifactVisitor::new();

        let mut expanded_cmd = Vec::<String>::new();
        let mut ctx = B::new(self.fs);
        for var in cmd.into_owned() {
            expand_arg_value(
                &mut expanded_cmd,
                &mut ctx,
                &mut artifact_visitor,
                declared_outputs,
                var,
            )?;
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
                    &mut artifact_visitor,
                    declared_outputs,
                    v,
                )?;
                anyhow::Ok((k, curr_env))
            })
            .collect::<Result<SortedVectorMap<_, _>, _>>()?;

        let expanded_worker = match test_info.worker() {
            Some(worker) => {
                let mut worker_rendered = Vec::<String>::new();
                let worker_exe = worker.exe_command_line();
                worker_exe.add_to_command_line(&mut worker_rendered, &mut ctx)?;
                worker_exe.visit_artifacts(&mut artifact_visitor)?;
                Some(WorkerSpec {
                    exe: worker_rendered,
                    id: WorkerId(worker.id),
                    concurrency: worker.concurrency(),
                    remote_key: None,
                })
            }
            _ => None,
        };

        let inputs = artifact_visitor.inputs;

        Ok((expanded_cmd, expanded_env, inputs, expanded_worker))
    }
}

async fn resolve_output_root(
    dice: &mut DiceComputations<'_>,
    test_target: &ConfiguredProvidersLabel,
    prefix: TestExecutionPrefix,
) -> Result<ForwardRelativePathBuf, anyhow::Error> {
    let output_root = match prefix {
        TestExecutionPrefix::Listing => {
            let resolver = dice.get_buck_out_path().await?;
            resolver
                .resolve_test_discovery(test_target)
                .into_forward_relative_path_buf()
        }
        TestExecutionPrefix::Testing(prefix) => prefix.join(ForwardRelativePathBuf::unchecked_new(
            Uuid::new_v4().to_string(),
        )),
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

impl<'a> CommandLineBuilder for CommandLineBuilderFormatWrapper<'a> {
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
    inputs: IndexSet<ArtifactGroup>,
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

/// Checks if test listings cache is enabled. Needed only for safe deployment and will be removed
async fn check_cache_listings_experiment(
    dice: &mut DiceComputations<'_>,
    test_target: &ConfiguredProvidersLabel,
) -> anyhow::Result<bool> {
    #[derive(
        Clone,
        Dupe,
        derive_more::Display,
        Debug,
        Eq,
        Hash,
        PartialEq,
        Allocative
    )]
    struct CheckCacheListingsConfigKey;

    #[async_trait]
    impl Key for CheckCacheListingsConfigKey {
        type Value = buck2_error::Result<Arc<Vec<ParsedPattern<TargetPatternExtra>>>>;

        async fn compute(
            &self,
            mut dice: &mut DiceComputations,
            _cancellation: &CancellationContext,
        ) -> Self::Value {
            let cell_resolver = dice.get_cell_resolver().await?;
            let root_cell = cell_resolver.root_cell();
            let alias_resolver = dice.get_cell_alias_resolver(root_cell).await?;
            let root_conf = dice.get_legacy_root_config_on_dice().await?;
            let patterns: Vec<String> = root_conf
                .view(&mut dice)
                .parse_list(BuckconfigKeyRef {
                    section: "buck2",
                    property: "cache_test_listings",
                })?
                .unwrap_or_default();

            let mut result = Vec::new();
            for pattern in patterns {
                result.push(ParsedPattern::parse_precise(
                    pattern.trim(),
                    root_cell,
                    &cell_resolver,
                    &alias_resolver,
                )?);
            }
            Ok(result.into())
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            match (x, y) {
                (Ok(x), Ok(y)) => x == y,
                _ => false,
            }
        }
    }

    let patterns = dice.compute(&CheckCacheListingsConfigKey).await??;
    for pattern in patterns.iter() {
        if pattern.matches(test_target.target().unconfigured_label()) {
            return Ok(true);
        }
    }

    Ok(false)
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
            options.custom_image.clone()
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_build_api::context::SetBuildContextData;
    use buck2_common::dice::cells::SetCellResolver;
    use buck2_common::dice::data::testing::SetTestingIoProvider;
    use buck2_common::liveliness_observer::NoopLivelinessObserver;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_test_api::data::TestStatus;
    use dice::testing::DiceBuilder;
    use dice::UserComputationData;
    use futures::channel::mpsc;
    use futures::channel::mpsc::UnboundedReceiver;
    use futures::future;
    use futures::stream::TryStreamExt;

    use super::*;

    async fn make() -> anyhow::Result<(
        BuckTestOrchestrator<'static>,
        UnboundedReceiver<anyhow::Result<ExecutorMessage>>,
    )> {
        let fs = ProjectRootTemp::new().unwrap();

        let cell_resolver = CellResolver::testing_with_name_and_path(
            CellName::testing_new("cell"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell".to_owned())),
        );
        let buckout_path = ProjectRelativePathBuf::unchecked_new("buck_out/v2".into());
        let mut dice = DiceBuilder::new()
            .set_data(|d| d.set_testing_io_provider(&fs))
            .build(UserComputationData::new())?;
        dice.set_buck_out_path(Some(buckout_path))?;
        dice.set_cell_resolver(cell_resolver)?;

        let dice = dice.commit().await;

        let (sender, receiver) = mpsc::unbounded();

        Ok((
            BuckTestOrchestrator::from_parts(
                dice,
                Arc::new(TestSession::new(Default::default())),
                NoopLivelinessObserver::create(),
                sender,
                EventDispatcher::null(),
                CancellationContext::testing(),
            ),
            receiver,
        ))
    }

    #[tokio::test]
    async fn orchestrator_results() -> anyhow::Result<()> {
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
                })
                .await?;

            orchestrator.end_of_test_results(0).await?;

            anyhow::Ok(())
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
                }),
                ExecutorMessage::TestResult(TestResult {
                    target,

                    status: TestStatus::FAIL,
                    msg: None,
                    name: "Second - test".to_owned(),
                    duration: Some(Duration::from_micros(2)),
                    details: "2".to_owned(),
                }),
                ExecutorMessage::ExitCode(0),
            ]
        );

        Ok(())
    }

    #[tokio::test]
    async fn orchestrator_attach_info_messages() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;

        let jobs = async {
            orchestrator.attach_info_message("yolo".to_owned()).await?;

            orchestrator.end_of_test_results(0).await?;

            anyhow::Ok(())
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
    async fn test_orchestrator_channel_drop() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;
        drop(orchestrator);

        let res = channel.try_collect::<Vec<_>>().await;
        assert!(res.is_err());

        Ok(())
    }

    #[tokio::test]
    async fn test_orchestrator_closes_channel() -> anyhow::Result<()> {
        let (orchestrator, channel) = make().await?;
        let sender = orchestrator.results_channel.clone();
        orchestrator.end_of_test_results(1).await?;

        assert!(sender.is_closed());
        drop(channel);

        Ok(())
    }
}
