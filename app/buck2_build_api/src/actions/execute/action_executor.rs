/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::ops::ControlFlow;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::events::HasEvents;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::http::counting_client::CountingHttpClient;
use buck2_common::http::HasHttpClient;
use buck2_common::io::IoProvider;
use buck2_common::liveliness_observer::NoopLivelinessObserver;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::HasBlockingExecutor;
use buck2_execute::execute::claim::MutexClaimManager;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::execute::command_executor::CommandExecutor;
use buck2_execute::execute::dice_data::CommandExecutorResponse;
use buck2_execute::execute::dice_data::GetReClient;
use buck2_execute::execute::dice_data::HasCommandExecutor;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::prepared::PreparedAction;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::OutputType;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::output_size::OutputCountAndBytes;
use buck2_execute::output_size::OutputSize;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_file_watcher::mergebase::GetMergebase;
use buck2_file_watcher::mergebase::Mergebase;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use indexmap::indexmap;
use indexmap::IndexMap;
use itertools::Itertools;
use more_futures::cancellation::CancellationContext;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::actions::execute::action_execution_target::ActionExecutionTarget;
use crate::actions::execute::error::CommandExecutionErrorMarker;
use crate::actions::execute::error::ExecuteError;
use crate::actions::impls::run_action_knobs::HasRunActionKnobs;
use crate::actions::impls::run_action_knobs::RunActionKnobs;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::RegisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;

/// This is the result of the action as exposed to other things in the dice computation.
#[derive(Clone, Dupe, Debug, PartialEq, Eq, Allocative)]
pub struct ActionOutputs(Arc<ActionOutputsData>);

impl OutputSize for ActionOutputs {
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes {
        let mut total_count = 0;
        let mut total_bytes = 0;
        for v in self.values() {
            let count_and_bytes = v.calc_output_count_and_bytes();
            total_count += count_and_bytes.count;
            total_bytes += count_and_bytes.bytes;
        }
        OutputCountAndBytes {
            count: total_count,
            bytes: total_bytes,
        }
    }
}

#[derive(Derivative, Debug, Allocative)]
#[derivative(PartialEq, Eq)]
struct ActionOutputsData {
    outputs: IndexMap<BuckOutPath, ArtifactValue>,
}

/// Metadata associated with the execution of this action.
#[derive(Debug)]
pub struct ActionExecutionMetadata {
    pub execution_kind: ActionExecutionKind,
    pub timing: ActionExecutionTimingData,
}

/// The *way* that a particular action was executed.
#[derive(Debug, Display, Clone)]
pub enum ActionExecutionKind {
    #[display(fmt = "command({})", kind)]
    Command {
        kind: CommandExecutionKind,
        prefers_local: bool,
        requires_local: bool,
        allows_cache_upload: bool,
        did_cache_upload: bool,
        eligible_for_full_hybrid: bool,
    },
    /// This action is simple and executed inline within buck2 (e.g. write, symlink_dir)
    #[display(fmt = "simple")]
    Simple,
    /// This action was not executed at all.
    #[display(fmt = "skipped")]
    Skipped,
    /// This action logically executed, but didn't do all the work.
    #[display(fmt = "deferred")]
    Deferred,
    /// This action was served by the local dep file cache and not executed.
    #[display(fmt = "local_dep_files")]
    LocalDepFile,
}

pub struct CommandExecutionRef<'a> {
    pub kind: &'a CommandExecutionKind,
    pub prefers_local: bool,
    pub requires_local: bool,
    pub allows_cache_upload: bool,
    pub did_cache_upload: bool,
    pub eligible_for_full_hybrid: bool,
}

impl ActionExecutionKind {
    pub fn as_enum(&self) -> buck2_data::ActionExecutionKind {
        match self {
            ActionExecutionKind::Command { kind, .. } => kind.as_enum(),
            ActionExecutionKind::Simple => buck2_data::ActionExecutionKind::Simple,
            ActionExecutionKind::Skipped => buck2_data::ActionExecutionKind::Skipped,
            ActionExecutionKind::Deferred => buck2_data::ActionExecutionKind::Deferred,
            ActionExecutionKind::LocalDepFile => buck2_data::ActionExecutionKind::LocalDepFile,
        }
    }

    pub fn command(&self) -> Option<CommandExecutionRef<'_>> {
        match self {
            Self::Command {
                kind,
                prefers_local,
                requires_local,
                allows_cache_upload,
                did_cache_upload,
                eligible_for_full_hybrid,
            } => Some(CommandExecutionRef {
                kind,
                prefers_local: *prefers_local,
                requires_local: *requires_local,
                allows_cache_upload: *allows_cache_upload,
                did_cache_upload: *did_cache_upload,
                eligible_for_full_hybrid: *eligible_for_full_hybrid,
            }),
            Self::Simple | Self::Skipped | Self::Deferred | Self::LocalDepFile => None,
        }
    }
}

impl ActionOutputs {
    pub fn new(outputs: IndexMap<BuckOutPath, ArtifactValue>) -> Self {
        Self(Arc::new(ActionOutputsData { outputs }))
    }

    pub fn from_single(artifact: BuckOutPath, value: ArtifactValue) -> Self {
        Self::new(indexmap! {artifact => value})
    }

    pub fn get(&self, artifact: &BuckOutPath) -> Option<&ArtifactValue> {
        self.0.outputs.get(artifact)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&BuckOutPath, &ArtifactValue)> {
        self.0.outputs.iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &ArtifactValue> {
        self.0.outputs.values()
    }
}

/// Executes 'Actions'
#[async_trait]
pub trait ActionExecutor: Send + Sync {
    async fn execute(
        &self,
        inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
        action: &RegisteredAction,
        cancellation: &CancellationContext,
    ) -> (
        Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError>,
        Vec<CommandExecutionReport>,
    );
}

#[async_trait]
pub trait HasActionExecutor {
    async fn get_action_executor(
        &self,
        config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn ActionExecutor>>;
}

#[async_trait]
impl HasActionExecutor for DiceComputations {
    async fn get_action_executor(
        &self,
        executor_config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn ActionExecutor>> {
        let artifact_fs = self.get_artifact_fs().await?;
        let digest_config = self.global_data().get_digest_config();

        let CommandExecutorResponse {
            executor,
            platform,
            cache_checker,
        } = self.get_command_executor(&artifact_fs, executor_config)?;
        let blocking_executor = self.get_blocking_executor();
        let materializer = self.per_transaction_data().get_materializer();
        let events = self.per_transaction_data().get_dispatcher().dupe();
        let re_client = self.per_transaction_data().get_re_client();
        let run_action_knobs = self.per_transaction_data().get_run_action_knobs();
        let io_provider = self.global_data().get_io_provider();
        let http_client = self.per_transaction_data().get_http_client();
        let mergebase = self.per_transaction_data().get_mergebase();

        Ok(Arc::new(BuckActionExecutor::new(
            CommandExecutor::new(
                executor,
                cache_checker,
                artifact_fs,
                executor_config.options,
                platform,
                run_action_knobs.enforce_re_timeouts,
            ),
            blocking_executor,
            materializer,
            events,
            re_client,
            digest_config,
            run_action_knobs,
            io_provider,
            http_client,
            mergebase,
        )))
    }
}

pub struct BuckActionExecutor {
    command_executor: CommandExecutor,
    blocking_executor: Arc<dyn BlockingExecutor>,
    materializer: Arc<dyn Materializer>,
    events: EventDispatcher,
    re_client: ManagedRemoteExecutionClient,
    digest_config: DigestConfig,
    run_action_knobs: RunActionKnobs,
    io_provider: Arc<dyn IoProvider>,
    http_client: CountingHttpClient,
    mergebase: Mergebase,
}

impl BuckActionExecutor {
    pub fn new(
        command_executor: CommandExecutor,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materializer: Arc<dyn Materializer>,
        events: EventDispatcher,
        re_client: ManagedRemoteExecutionClient,
        digest_config: DigestConfig,
        run_action_knobs: RunActionKnobs,
        io_provider: Arc<dyn IoProvider>,
        http_client: CountingHttpClient,
        mergebase: Mergebase,
    ) -> Self {
        Self {
            command_executor,
            blocking_executor,
            materializer,
            events,
            re_client,
            digest_config,
            run_action_knobs,
            io_provider,
            http_client,
            mergebase,
        }
    }
}

struct BuckActionExecutionContext<'a> {
    executor: &'a BuckActionExecutor,
    action: &'a RegisteredAction,
    inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
    outputs: &'a [BuildArtifact],
    command_reports: &'a mut Vec<CommandExecutionReport>,
    cancellations: &'a CancellationContext,
}

impl BuckActionExecutionContext<'_> {
    fn unpack_command_execution_result(
        &mut self,
        request: &CommandExecutionRequest,
        result: CommandExecutionResult,
    ) -> anyhow::Result<(
        IndexMap<BuckOutPath, ArtifactValue>,
        ActionExecutionMetadata,
    )> {
        let CommandExecutionResult {
            outputs,
            report,
            rejected_execution,
            did_cache_upload,
            eligible_for_full_hybrid,
        } = result;
        // TODO (@torozco): The execution kind should be made to come via the command reports too.
        let res = match &report.status {
            CommandExecutionStatus::Success { execution_kind } => {
                let result = (
                    outputs
                        .into_iter()
                        .filter_map(|(output, value)| {
                            Some((output.into_build_artifact()?.0, value))
                        })
                        .collect(),
                    ActionExecutionMetadata {
                        execution_kind: ActionExecutionKind::Command {
                            kind: execution_kind.clone(),
                            prefers_local: request.executor_preference().prefers_local(),
                            requires_local: request.executor_preference().requires_local(),
                            allows_cache_upload: request.allow_cache_upload(),
                            did_cache_upload,
                            eligible_for_full_hybrid,
                        },
                        timing: report.timing.into(),
                    },
                );
                Ok(result)
            }
            _ => Err(CommandExecutionErrorMarker.into()),
        };
        self.command_reports.extend(rejected_execution.into_iter());
        self.command_reports.push(report);
        res
    }
}

#[async_trait]
impl ActionExecutionCtx for BuckActionExecutionContext<'_> {
    fn target(&self) -> ActionExecutionTarget<'_> {
        ActionExecutionTarget::new(self.action)
    }

    fn fs(&self) -> &ArtifactFs {
        self.executor.command_executor.fs()
    }

    fn executor_fs(&self) -> ExecutorFs {
        self.executor.command_executor.executor_fs()
    }

    fn materializer(&self) -> &dyn Materializer {
        self.executor.materializer.as_ref()
    }

    fn events(&self) -> &EventDispatcher {
        &self.executor.events
    }

    fn command_execution_manager(&self) -> CommandExecutionManager {
        CommandExecutionManager::new(
            Box::new(MutexClaimManager::new()),
            self.executor.events.dupe(),
            NoopLivelinessObserver::create(),
        )
    }

    fn artifact_values(&self, artifact: &ArtifactGroup) -> &ArtifactGroupValues {
        self.inputs.get(artifact).unwrap_or_else(|| panic!("Internal error: action {:?} tried to grab the artifact {:?} even though it was not an input.", self.action.owner(), artifact))
    }

    fn blocking_executor(&self) -> &dyn BlockingExecutor {
        self.executor.blocking_executor.as_ref()
    }

    fn re_client(&self) -> ManagedRemoteExecutionClient {
        self.executor.re_client.dupe()
    }

    fn digest_config(&self) -> DigestConfig {
        self.executor.digest_config
    }

    fn run_action_knobs(&self) -> RunActionKnobs {
        self.executor.run_action_knobs
    }

    fn cancellation_context(&self) -> &CancellationContext {
        self.cancellations
    }

    fn mergebase(&self) -> &Mergebase {
        &self.executor.mergebase
    }

    fn prepare_action(
        &mut self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<PreparedAction> {
        self.executor
            .command_executor
            .prepare_action(request, self.digest_config())
    }

    async fn action_cache(
        &mut self,
        manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        prepared_action: &PreparedAction,
    ) -> ControlFlow<
        anyhow::Result<(
            IndexMap<BuckOutPath, ArtifactValue>,
            ActionExecutionMetadata,
        )>,
        CommandExecutionManager,
    > {
        let action = self.target();
        let cache_result = self
            .executor
            .command_executor
            .action_cache(
                manager,
                &PreparedCommand {
                    target: &action as _,
                    request,
                    prepared_action,
                    digest_config: self.digest_config(),
                },
                self.cancellations,
            )
            .await;
        match cache_result {
            ControlFlow::Continue(manager) => ControlFlow::Continue(manager),
            ControlFlow::Break(result) => {
                match self.unpack_command_execution_result(request, result) {
                    Err(e) => ControlFlow::Break(Err(e)),
                    Ok(res) => ControlFlow::Break(Ok(res)),
                }
            }
        }
    }

    async fn exec_cmd(
        &mut self,
        manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        prepared_action: &PreparedAction,
    ) -> anyhow::Result<(
        IndexMap<BuckOutPath, ArtifactValue>,
        ActionExecutionMetadata,
    )> {
        let action = self.target();
        let result = self
            .executor
            .command_executor
            .exec_cmd(
                manager,
                &PreparedCommand {
                    target: &action as _,
                    request,
                    prepared_action,
                    digest_config: self.digest_config(),
                },
                self.cancellations,
            )
            .await;

        self.unpack_command_execution_result(request, result)
    }

    async fn cleanup_outputs(&mut self) -> anyhow::Result<()> {
        // Delete all outputs before we start, so things will be clean.
        let output_paths = self
            .outputs
            .iter()
            .map(|o| self.fs().resolve_build(o.get_path()))
            .collect::<Vec<_>>();

        // Invalidate all the output paths this action might provide. Note that this is a bit
        // approximative: we might have previous instances of this action that declared
        // different outputs with a different materialization method that will become invalid
        // now. However, nothing should reference those stale outputs, so while this does not
        // do a good job of cleaning up garbage, it prevents using invalid artifacts.
        self.executor
            .materializer
            .invalidate_many(output_paths.clone())
            .await
            .context("Failed to invalidate output directory")?;

        // Use Eden's clean up API if possible, it is significantly faster on Eden compared with
        // the native method as the API does not load and materialize files or folders
        if let Some(eden_buck_out) = self.executor.materializer.eden_buck_out() {
            eden_buck_out
                .remove_paths_recursive(self.fs().fs(), output_paths, self.cancellations)
                .await?;
        } else {
            self.executor
                .blocking_executor
                .execute_io(
                    Box::new(CleanOutputPaths {
                        paths: output_paths,
                    }),
                    self.cancellations,
                )
                .await
                .context("Failed to cleanup output directory")?;
        }

        Ok(())
    }

    fn io_provider(&self) -> Arc<dyn IoProvider> {
        self.executor.io_provider.dupe()
    }

    fn http_client(&self) -> CountingHttpClient {
        self.executor.http_client.dupe()
    }
}

#[async_trait]
impl ActionExecutor for BuckActionExecutor {
    async fn execute(
        &self,
        inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
        action: &RegisteredAction,
        cancellations: &CancellationContext,
    ) -> (
        Result<(ActionOutputs, ActionExecutionMetadata), ExecuteError>,
        Vec<CommandExecutionReport>,
    ) {
        let mut command_reports = Vec::new();

        let res = async {
            let outputs = action.outputs()?;

            let mut ctx = BuckActionExecutionContext {
                executor: self,
                action,
                inputs,
                outputs: outputs.as_ref(),
                command_reports: &mut command_reports,
                cancellations,
            };

            let (result, metadata) = match action.as_executable() {
                ActionExecutable::Pristine(exe) => {
                    ctx.cleanup_outputs().await?;
                    exe.execute(&mut ctx).await?
                }
                ActionExecutable::Incremental(exe) => {
                    // Let the action perform clean up in this case.
                    exe.execute(&mut ctx).await?
                }
            };

            // Check that all the outputs are the right output_type
            for x in outputs.iter() {
                let declared = x.output_type();
                // FIXME: One day we should treat FileOrDirectory as a File, and soft_error if it is a directory
                if declared != OutputType::FileOrDirectory {
                    if let Some(t) = result.0.outputs.get(x.get_path()) {
                        let real = if t.is_dir() {
                            OutputType::Directory
                        } else {
                            OutputType::File
                        };
                        if real != declared {
                            return Err(ExecuteError::WrongOutputType {
                                path: self.command_executor.fs().resolve_build(x.get_path()),
                                declared,
                                real,
                            });
                        }
                    }
                }
            }

            // Check all the outputs were returned, and no additional outputs
            // TODO (T122966509): Check projections here as well
            if !outputs
                .iter()
                .map(|b| b.get_path())
                .eq(result.0.outputs.keys())
            {
                let declared = outputs
                    .iter()
                    .filter(|x| !result.0.outputs.contains_key(x.get_path()))
                    .map(|x| self.command_executor.fs().resolve_build(x.get_path()))
                    .collect();
                let real = result
                    .0
                    .outputs
                    .keys()
                    .filter(|x| {
                        // This is error message, linear search is fine.
                        !outputs.iter().map(|b| b.get_path()).contains(x)
                    })
                    .map(|x| self.command_executor.fs().resolve_build(x))
                    .collect::<Vec<_>>();
                if real.is_empty() {
                    Err(ExecuteError::MissingOutputs { declared })
                } else {
                    Err(ExecuteError::MismatchedOutputs { declared, real })
                }
            } else {
                Ok((result, metadata))
            }
        }
        .await;

        (res, command_reports)
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::sync::Mutex;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_artifact::actions::key::ActionKey;
    use buck2_artifact::artifact::artifact_type::testing::BuildArtifactTestingExt;
    use buck2_artifact::artifact::artifact_type::Artifact;
    use buck2_artifact::artifact::build_artifact::BuildArtifact;
    use buck2_artifact::artifact::source_artifact::SourceArtifact;
    use buck2_artifact::deferred::data::DeferredData;
    use buck2_artifact::deferred::id::DeferredId;
    use buck2_artifact::deferred::key::DeferredKey;
    use buck2_common::cas_digest::CasDigestConfig;
    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_common::executor_config::CommandGenerationOptions;
    use buck2_common::executor_config::PathSeparatorKind;
    use buck2_common::http::counting_client::CountingHttpClient;
    use buck2_common::http::ClientForTest;
    use buck2_common::io::fs::FsIoProvider;
    use buck2_core::base_deferred_key::BaseDeferredKey;
    use buck2_core::buck_path::path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::name::CellName;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::data::ConfigurationData;
    use buck2_core::fs::artifact_path_resolver::ArtifactFs;
    use buck2_core::fs::buck_out_path::BuckOutPathResolver;
    use buck2_core::fs::fs_util;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::PackageLabel;
    use buck2_core::target::label::TargetLabel;
    use buck2_core::target::name::TargetNameRef;
    use buck2_events::dispatch::with_dispatcher_async;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_execute::artifact_value::ArtifactValue;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::execute::blocking::testing::DummyBlockingExecutor;
    use buck2_execute::execute::clean_output_paths::cleanup_path;
    use buck2_execute::execute::command_executor::ActionExecutionTimingData;
    use buck2_execute::execute::command_executor::CommandExecutor;
    use buck2_execute::execute::prepared::NoOpCommandExecutor;
    use buck2_execute::execute::request::CommandExecutionInput;
    use buck2_execute::execute::request::CommandExecutionOutput;
    use buck2_execute::execute::request::CommandExecutionPaths;
    use buck2_execute::execute::request::CommandExecutionRequest;
    use buck2_execute::execute::request::OutputType;
    use buck2_execute::execute::testing_dry_run::DryRunExecutor;
    use buck2_execute::materialize::nodisk::NoDiskMaterializer;
    use buck2_execute::re::manager::ManagedRemoteExecutionClient;
    use dupe::Dupe;
    use indexmap::indexset;
    use more_futures::cancellation::CancellationContext;
    use once_cell::sync::Lazy;
    use sorted_vector_map::SortedVectorMap;

    use crate::actions::box_slice_set::BoxSliceSet;
    use crate::actions::execute::action_executor::ActionExecutionKind;
    use crate::actions::execute::action_executor::ActionExecutionMetadata;
    use crate::actions::execute::action_executor::ActionExecutor;
    use crate::actions::execute::action_executor::ActionOutputs;
    use crate::actions::execute::action_executor::BuckActionExecutor;
    use crate::actions::key::ActionKeyExt;
    use crate::actions::Action;
    use crate::actions::ActionExecutable;
    use crate::actions::ActionExecutionCtx;
    use crate::actions::PristineActionExecutable;
    use crate::actions::RegisteredAction;
    use crate::artifact_groups::ArtifactGroup;
    use crate::artifact_groups::ArtifactGroupValues;

    #[tokio::test]
    async fn can_execute_some_action() {
        let cells = CellResolver::testing_with_name_and_path(
            CellName::testing_new("cell"),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
        );

        let temp_fs = ProjectRootTemp::new().unwrap();

        let project_fs = temp_fs.path().dupe();
        let artifact_fs = ArtifactFs::new(
            cells,
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                "cell/buck-out/v2".into(),
            )),
            project_fs.dupe(),
        );

        let tracker = Arc::new(Mutex::new(Vec::new()));

        let executor = BuckActionExecutor::new(
            CommandExecutor::new(
                Arc::new(DryRunExecutor::new(tracker, artifact_fs.clone())),
                Arc::new(NoOpCommandExecutor {}),
                artifact_fs,
                CommandGenerationOptions {
                    path_separator: PathSeparatorKind::Unix,
                    output_paths_behavior: Default::default(),
                },
                Default::default(),
                false,
            ),
            Arc::new(DummyBlockingExecutor {
                fs: project_fs.dupe(),
            }),
            Arc::new(NoDiskMaterializer),
            EventDispatcher::null(),
            ManagedRemoteExecutionClient::testing_new_dummy(),
            DigestConfig::testing_default(),
            Default::default(),
            Arc::new(FsIoProvider::new(
                project_fs,
                CasDigestConfig::testing_default(),
            )),
            CountingHttpClient::new(Arc::new(ClientForTest {})),
            Default::default(),
        );

        #[derive(Debug, Allocative)]
        struct TestingAction {
            inputs: BoxSliceSet<ArtifactGroup>,
            outputs: BoxSliceSet<BuildArtifact>,
            ran: AtomicBool,
        }

        #[async_trait]
        impl Action for TestingAction {
            fn kind(&self) -> buck2_data::ActionKind {
                buck2_data::ActionKind::NotSet
            }

            fn inputs(&self) -> anyhow::Result<Cow<'_, [ArtifactGroup]>> {
                Ok(Cow::Borrowed(self.inputs.as_slice()))
            }

            fn outputs(&self) -> anyhow::Result<Cow<'_, [BuildArtifact]>> {
                Ok(Cow::Borrowed(self.outputs.as_slice()))
            }

            fn as_executable(&self) -> ActionExecutable<'_> {
                ActionExecutable::Pristine(self)
            }

            fn category(&self) -> &Category {
                static TEST_CATEGORY: Lazy<Category> =
                    Lazy::new(|| Category::try_from("testing").unwrap());

                &TEST_CATEGORY
            }

            fn identifier(&self) -> Option<&str> {
                None
            }
        }

        #[async_trait]
        impl PristineActionExecutable for TestingAction {
            async fn execute(
                &self,
                ctx: &mut dyn ActionExecutionCtx,
            ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
                self.ran.store(true, Ordering::SeqCst);

                let req = CommandExecutionRequest::new(
                    vec![],
                    vec!["foo".to_owned(), "bar".to_owned(), "cmd".to_owned()],
                    CommandExecutionPaths::new(
                        self.inputs
                            .iter()
                            .map(|x| {
                                CommandExecutionInput::Artifact(Box::new(
                                    ArtifactGroupValues::from_artifact(
                                        x.unpack_artifact().unwrap().dupe(),
                                        ArtifactValue::file(ctx.digest_config().empty_file()),
                                    ),
                                ))
                            })
                            .collect(),
                        self.outputs
                            .iter()
                            .map(|b| CommandExecutionOutput::BuildArtifact {
                                path: b.get_path().dupe(),
                                output_type: OutputType::FileOrDirectory,
                            })
                            .collect(),
                        ctx.fs(),
                        ctx.digest_config(),
                    )?,
                    SortedVectorMap::new(),
                );

                // on fake executor, this does nothing
                let prepared_action = ctx.prepare_action(&req)?;
                let manager = ctx.command_execution_manager();
                let res = ctx.exec_cmd(manager, &req, &prepared_action).await;

                // Must write out the things we promised to do
                for x in &self.outputs {
                    let dest = x.get_path();
                    let dest_path = ctx.fs().resolve_build(dest);
                    ctx.fs().fs().write_file(&dest_path, "", false)?
                }

                res?;
                let outputs = self
                    .outputs
                    .iter()
                    .map(|o| {
                        (
                            o.get_path().dupe(),
                            ArtifactValue::file(ctx.digest_config().empty_file()),
                        )
                    })
                    .collect();
                Ok((
                    ActionOutputs::new(outputs),
                    ActionExecutionMetadata {
                        execution_kind: ActionExecutionKind::Simple,
                        timing: ActionExecutionTimingData::default(),
                    },
                ))
            }
        }

        let pkg = PackageLabel::new(
            CellName::testing_new("cell"),
            CellRelativePath::unchecked_new("pkg"),
        );

        let inputs = indexset![ArtifactGroup::Artifact(Artifact::from(
            SourceArtifact::new(BuckPath::testing_new(
                pkg.dupe(),
                PackageRelativePathBuf::unchecked_new("source".into()),
            ))
        ))];
        let label = TargetLabel::new(pkg, TargetNameRef::unchecked_new("foo"))
            .configure(ConfigurationData::testing_new());
        let outputs = indexset![BuildArtifact::testing_new(
            label.dupe(),
            ForwardRelativePathBuf::unchecked_new("output".into()),
            DeferredId::testing_new(0),
        )];

        let action = RegisteredAction::new(
            ActionKey::new(DeferredData::unchecked_new(DeferredKey::Base(
                BaseDeferredKey::TargetLabel(label.dupe()),
                DeferredId::testing_new(0),
            ))),
            Box::new(TestingAction {
                inputs: BoxSliceSet::from(inputs),
                outputs: BoxSliceSet::from(outputs.clone()),
                ran: Default::default(),
            }),
            CommandExecutorConfig::testing_local(),
        );
        let res = with_dispatcher_async(
            EventDispatcher::null(),
            executor.execute(Default::default(), &action, CancellationContext::testing()),
        )
        .await
        .0
        .unwrap();
        let outputs = outputs
            .iter()
            .map(|o| {
                (
                    o.get_path().dupe(),
                    ArtifactValue::file(executor.digest_config.empty_file()),
                )
            })
            .collect();
        assert_eq!(res.0, ActionOutputs::new(outputs));
    }

    #[test]
    fn test_cleanup_path_missing() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let fs = fs.path();
        fs_util::create_dir_all(fs.resolve(ProjectRelativePath::unchecked_new("foo/bar/qux")))?;
        cleanup_path(fs, ProjectRelativePath::unchecked_new("foo/bar/qux/xx"))?;
        assert!(
            fs.resolve(ProjectRelativePath::unchecked_new("foo/bar/qux"))
                .exists()
        );
        Ok(())
    }

    #[test]
    fn test_cleanup_path_present() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let fs = fs.path();
        fs_util::create_dir_all(fs.resolve(ProjectRelativePath::unchecked_new("foo/bar/qux")))?;
        cleanup_path(fs, ProjectRelativePath::unchecked_new("foo/bar/qux"))?;
        assert!(
            !fs.resolve(ProjectRelativePath::unchecked_new("foo/bar/qux"))
                .exists()
        );
        assert!(
            fs.resolve(ProjectRelativePath::unchecked_new("foo/bar"))
                .exists()
        );
        Ok(())
    }

    #[test]
    fn test_cleanup_path_overlap() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let fs = fs.path();
        fs.write_file(ProjectRelativePath::unchecked_new("foo/bar"), "xx", false)?;
        cleanup_path(fs, ProjectRelativePath::unchecked_new("foo/bar/qux"))?;
        assert!(
            !fs.resolve(ProjectRelativePath::unchecked_new("foo/bar"))
                .exists()
        );
        assert!(
            fs.resolve(ProjectRelativePath::unchecked_new("foo"))
                .exists()
        );
        Ok(())
    }

    #[test]
    fn test_cleanup_path_overlap_deep() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let fs = fs.path();
        fs.write_file(ProjectRelativePath::unchecked_new("foo/bar"), "xx", false)?;
        cleanup_path(
            fs,
            ProjectRelativePath::unchecked_new("foo/bar/qux/1/2/3/4"),
        )?;
        assert!(
            !fs.resolve(ProjectRelativePath::unchecked_new("foo/bar"))
                .exists()
        );
        assert!(
            fs.resolve(ProjectRelativePath::unchecked_new("foo"))
                .exists()
        );
        Ok(())
    }
}
