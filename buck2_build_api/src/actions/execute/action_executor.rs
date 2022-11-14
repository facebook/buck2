/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::liveliness_manager::NoopLivelinessManager;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::HasBlockingExecutor;
use buck2_execute::execute::claim::MutexClaimManager;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::execute::command_executor::ActionExecutionTimingData;
use buck2_execute::execute::command_executor::CommandExecutor;
use buck2_execute::execute::dice_data::GetReClient;
use buck2_execute::execute::dice_data::HasCommandExecutor;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionReport;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::path::buck_out_path::BuckOutPath;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_interpreter::dice::HasEvents;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use gazebo::prelude::*;
use indexmap::indexmap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use itertools::Itertools;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::execute::error::CommandExecutionErrorMarker;
use crate::actions::execute::error::ExecuteError;
use crate::actions::impls::run::knobs::HasRunActionKnobs;
use crate::actions::impls::run::knobs::RunActionKnobs;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::RegisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::calculation::Calculation;

/// This is the result of the action as exposed to other things in the dice computation.
#[derive(Clone, Dupe, Debug, PartialEq, Eq, Allocative)]
pub struct ActionOutputs(Arc<ActionOutputsData>);

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
            Self::Simple | Self::Skipped | Self::Deferred => None,
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

        let command_executor = self.get_command_executor(&artifact_fs, executor_config)?;
        let blocking_executor = self.get_blocking_executor();
        let materializer = self.per_transaction_data().get_materializer();
        let events = self.per_transaction_data().get_dispatcher().dupe();
        let re_client = self.per_transaction_data().get_re_client();
        let run_action_knobs = self.per_transaction_data().get_run_action_knobs();

        Ok(Arc::new(BuckActionExecutor::new(
            CommandExecutor::new(
                command_executor,
                artifact_fs,
                executor_config.path_separator,
            ),
            blocking_executor,
            materializer,
            events,
            re_client,
            run_action_knobs,
        )))
    }
}

pub struct BuckActionExecutor {
    command_executor: CommandExecutor,
    blocking_executor: Arc<dyn BlockingExecutor>,
    materializer: Arc<dyn Materializer>,
    events: EventDispatcher,
    re_client: ManagedRemoteExecutionClient,
    run_action_knobs: RunActionKnobs,
}

impl BuckActionExecutor {
    pub fn new(
        command_executor: CommandExecutor,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materializer: Arc<dyn Materializer>,
        events: EventDispatcher,
        re_client: ManagedRemoteExecutionClient,
        run_action_knobs: RunActionKnobs,
    ) -> Self {
        Self {
            command_executor,
            blocking_executor,
            materializer,
            events,
            re_client,
            run_action_knobs,
        }
    }
}

struct BuckActionExecutionContext<'a> {
    executor: &'a BuckActionExecutor,
    action: &'a RegisteredAction,
    inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
    outputs: &'a IndexSet<BuildArtifact>,
    command_reports: &'a mut Vec<CommandExecutionReport>,
}

#[async_trait]
impl ActionExecutionCtx for BuckActionExecutionContext<'_> {
    fn target(&self) -> CommandExecutionTarget<'_> {
        CommandExecutionTarget {
            owner: self.action.owner(),
            category: self.action.category(),
            identifier: self.action.identifier(),
            action_key: self.action.key() as _,
        }
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

    fn artifact_values(&self, artifact: &ArtifactGroup) -> &ArtifactGroupValues {
        self.inputs.get(artifact).unwrap_or_else(|| panic!("Internal error: action {:?} tried to grab the artifact {:?} even though it was not an input.", self.action.owner(), artifact))
    }

    fn blocking_executor(&self) -> &dyn BlockingExecutor {
        self.executor.blocking_executor.as_ref()
    }

    fn re_client(&self) -> ManagedRemoteExecutionClient {
        self.executor.re_client.dupe()
    }

    fn run_action_knobs(&self) -> RunActionKnobs {
        self.executor.run_action_knobs
    }

    async fn exec_cmd(
        &mut self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<(
        IndexMap<CommandExecutionOutput, ArtifactValue>,
        ActionExecutionMetadata,
    )> {
        let action = self.target();
        let manager = CommandExecutionManager::new(
            box MutexClaimManager::new(),
            self.executor.events.dupe(),
            NoopLivelinessManager::create(),
        );
        let CommandExecutionResult {
            outputs,
            report,
            rejected_execution,
            did_cache_upload,
            eligible_for_full_hybrid,
        } = self
            .executor
            .command_executor
            .exec_cmd(action, request, manager)
            .await;

        // TODO (@torozco): The execution kind should be made to come via the command reports too.
        let res = match &report.status {
            CommandExecutionStatus::Success { execution_kind } => Ok((
                outputs,
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
            )),

            _ => Err(CommandExecutionErrorMarker.into()),
        };

        self.command_reports.extend(rejected_execution.into_iter());
        self.command_reports.push(report);

        res
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
                .remove_paths_recursive(self.fs().fs(), output_paths)
                .await?;
        } else {
            self.executor
                .blocking_executor
                .execute_io(box CleanOutputPaths {
                    paths: output_paths,
                })
                .await
                .context("Failed to cleanup output directory")?;
        }

        Ok(())
    }
}

#[async_trait]
impl ActionExecutor for BuckActionExecutor {
    async fn execute(
        &self,
        inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
        action: &RegisteredAction,
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

            // Check all the outputs were returned, and no additional outputs
            // TODO (T122966509): Check projections here as well
            if !outputs
                .iter()
                .map(|b| b.get_path())
                .eq(result.0.outputs.keys())
            {
                let wanted = outputs
                    .iter()
                    .filter(|x| !result.0.outputs.contains_key(x.get_path()))
                    .map(|x| self.command_executor.fs().resolve_build(x.get_path()))
                    .collect();
                let got = result
                    .0
                    .outputs
                    .keys()
                    .filter(|x| {
                        // This is error message, linear search is fine.
                        !outputs.iter().map(|b| b.get_path()).contains(x)
                    })
                    .map(|x| self.command_executor.fs().resolve_build(x))
                    .collect::<Vec<_>>();
                if got.is_empty() {
                    Err(ExecuteError::MissingOutputs { wanted })
                } else {
                    Err(ExecuteError::MismatchedOutputs { wanted, got })
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
    use std::collections::HashMap;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::sync::Mutex;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_common::executor_config::CommandExecutorConfig;
    use buck2_common::executor_config::PathSeparatorKind;
    use buck2_core::buck_path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::fs_util;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectRelativePath;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::fs::project::ProjectRootTemp;
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    use buck2_core::package::Package;
    use buck2_core::target::testing::ConfiguredTargetLabelExt;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetName;
    use buck2_events::dispatch::with_dispatcher_async;
    use buck2_events::dispatch::EventDispatcher;
    use buck2_execute::artifact::fs::ArtifactFs;
    use buck2_execute::artifact::source_artifact::SourceArtifact;
    use buck2_execute::artifact_value::ArtifactValue;
    use buck2_execute::base_deferred_key::BaseDeferredKey;
    use buck2_execute::execute::blocking::testing::DummyBlockingExecutor;
    use buck2_execute::execute::clean_output_paths::cleanup_path;
    use buck2_execute::execute::command_executor::ActionExecutionTimingData;
    use buck2_execute::execute::command_executor::CommandExecutor;
    use buck2_execute::execute::request::CommandExecutionInput;
    use buck2_execute::execute::request::CommandExecutionRequest;
    use buck2_execute::execute::testing_dry_run::DryRunExecutor;
    use buck2_execute::materialize::nodisk::NoDiskMaterializer;
    use buck2_execute::path::buck_out_path::BuckOutPathResolver;
    use buck2_execute::path::buck_out_path::BuckPathResolver;
    use buck2_execute::re::manager::ManagedRemoteExecutionClient;
    use gazebo::prelude::*;
    use indexmap::indexset;
    use indexmap::IndexSet;
    use once_cell::sync::Lazy;

    use crate::actions::artifact::build_artifact::BuildArtifact;
    use crate::actions::artifact::testing::BuildArtifactTestingExt;
    use crate::actions::artifact::Artifact;
    use crate::actions::execute::action_executor::ActionExecutionKind;
    use crate::actions::execute::action_executor::ActionExecutionMetadata;
    use crate::actions::execute::action_executor::ActionExecutor;
    use crate::actions::execute::action_executor::ActionOutputs;
    use crate::actions::execute::action_executor::BuckActionExecutor;
    use crate::actions::key::ActionKey;
    use crate::actions::Action;
    use crate::actions::ActionExecutable;
    use crate::actions::ActionExecutionCtx;
    use crate::actions::PristineActionExecutable;
    use crate::actions::RegisteredAction;
    use crate::artifact_groups::ArtifactGroup;
    use crate::artifact_groups::ArtifactGroupValues;
    use crate::deferred::types::testing::DeferredDataExt;
    use crate::deferred::types::testing::DeferredIdExt;
    use crate::deferred::types::DeferredData;
    use crate::deferred::types::DeferredId;
    use crate::deferred::types::DeferredKey;

    #[tokio::test]
    async fn can_execute_some_action() {
        let cells = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("cell".into()),
            CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("cell_path".into())),
        )]);

        let temp_fs = ProjectRootTemp::new().unwrap();

        let project_fs = temp_fs.path().dupe();
        let artifact_fs = ArtifactFs::new(
            BuckPathResolver::new(cells),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                "cell/buck-out/v2".into(),
            )),
            project_fs.dupe(),
        );

        let tracker = Arc::new(Mutex::new(Vec::new()));

        let executor = BuckActionExecutor::new(
            CommandExecutor::new(
                Arc::new(DryRunExecutor::new(tracker, None)),
                artifact_fs,
                PathSeparatorKind::Unix,
            ),
            Arc::new(DummyBlockingExecutor { fs: project_fs }),
            Arc::new(NoDiskMaterializer),
            EventDispatcher::null(),
            ManagedRemoteExecutionClient::testing_new_dummy(),
            Default::default(),
        );

        #[derive(Debug, Allocative)]
        struct TestingAction {
            inputs: IndexSet<ArtifactGroup>,
            outputs: IndexSet<BuildArtifact>,
            ran: AtomicBool,
        }

        #[async_trait]
        impl Action for TestingAction {
            fn kind(&self) -> buck2_data::ActionKind {
                buck2_data::ActionKind::NotSet
            }

            fn inputs(&self) -> anyhow::Result<Cow<'_, IndexSet<ArtifactGroup>>> {
                Ok(Cow::Borrowed(&self.inputs))
            }

            fn outputs(&self) -> anyhow::Result<Cow<'_, IndexSet<BuildArtifact>>> {
                Ok(Cow::Borrowed(&self.outputs))
            }

            fn as_executable(&self) -> ActionExecutable<'_> {
                ActionExecutable::Pristine(self)
            }

            fn category(&self) -> &Category {
                static TEST_CATEGORY: Lazy<Category> =
                    Lazy::new(|| Category::try_from("testing").unwrap());

                &TEST_CATEGORY
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
                    vec!["foo".to_owned(), "bar".to_owned(), "cmd".to_owned()],
                    self.inputs
                        .iter()
                        .map(|x| {
                            CommandExecutionInput::Artifact(box ArtifactGroupValues::from_artifact(
                                x.unpack_artifact().unwrap().dupe(),
                                ArtifactValue::empty_file(),
                            ))
                        })
                        .collect(),
                    self.outputs.iter().map(|b| b.get_path().dupe()).collect(),
                    HashMap::new(),
                );

                // on fake executor, this does nothing
                let res = ctx.exec_cmd(&req).await;

                // Must write out the things we promised to do
                for x in &self.outputs {
                    ctx.fs().write_file(x.get_path(), "", false)?
                }

                res?;
                let outputs = self
                    .outputs
                    .iter()
                    .map(|o| (o.get_path().dupe(), ArtifactValue::empty_file()))
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

        let pkg = Package::new(
            &CellName::unchecked_new("cell".into()),
            CellRelativePath::unchecked_new("pkg"),
        );

        let inputs = indexset![ArtifactGroup::Artifact(Artifact::from(
            SourceArtifact::new(BuckPath::new(
                pkg.dupe(),
                PackageRelativePathBuf::unchecked_new("source".into()),
            ))
        ))];
        let label = ConfiguredTargetLabel::testing_new(
            pkg,
            TargetName::unchecked_new("foo"),
            Configuration::testing_new(),
        );
        let outputs = indexset![BuildArtifact::testing_new(
            label.dupe(),
            ForwardRelativePathBuf::unchecked_new("output".into()),
            DeferredId::testing_new(0),
        )];

        let action = RegisteredAction::new(
            ActionKey::new(DeferredData::testing_new(DeferredKey::Base(
                BaseDeferredKey::TargetLabel(label.dupe()),
                DeferredId::testing_new(0),
            ))),
            box TestingAction {
                inputs,
                outputs: outputs.clone(),
                ran: Default::default(),
            },
            CommandExecutorConfig::testing_local(),
        );
        let res = with_dispatcher_async(
            EventDispatcher::null(),
            executor.execute(Default::default(), &action),
        )
        .await
        .0
        .unwrap();
        let outputs = outputs
            .iter()
            .map(|o| (o.get_path().dupe(), ArtifactValue::empty_file()))
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
