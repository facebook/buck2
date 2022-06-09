/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod blocking;
pub mod commands;
pub mod materializer;

use std::{
    borrow::Cow,
    collections::HashMap,
    fmt::{Debug, Display, Write},
    hash::Hash,
    sync::Arc,
    time::Duration,
};

use anyhow::Context;
use async_trait::async_trait;
use blocking::{BlockingExecutor, HasBlockingExecutor, IoRequest};
use buck2_common::dice::data::HasIoProvider;
use buck2_core::fs::project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf};
use buck2_interpreter::dice::HasEvents;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use events::dispatch::EventDispatcher;
use gazebo::prelude::*;
use indexmap::{indexmap, IndexMap, IndexSet};
use starlark::collections::SmallMap;
use thiserror::Error;

use crate::{
    actions::{
        artifact::{ArtifactFs, ArtifactValue, BuildArtifact},
        run::knobs::{HasRunActionKnobs, RunActionKnobs},
        ActionExecutable, ActionExecutionCtx, RegisteredAction,
    },
    artifact_groups::{ArtifactGroup, ArtifactGroupValues},
    calculation::Calculation,
    execute::{
        commands::{
            dice_data::HasCommandExecutor, output::CommandStdStreams, re::client::ActionDigest,
            ClaimManager, CommandExecutionManager, CommandExecutionOutput, CommandExecutionRequest,
            CommandExecutionResult, CommandExecutionTarget, CommandExecutor,
        },
        materializer::{HasMaterializer, Materializer},
    },
};

fn error_items<T: Display>(xs: &[T]) -> String {
    if xs.is_empty() {
        return "none".to_owned();
    }
    let mut res = String::new();
    for (i, x) in xs.iter().enumerate() {
        if i != 0 {
            res.push_str(", ");
        }
        write!(res, "`{}`", x).unwrap();
    }
    res
}

// NOTE: In practice, this never gets rendered as an error (we unpack it instead).
#[derive(Error, Debug)]
#[error("Action failed")]
pub struct ActionError {
    #[source]
    pub cause: ActionErrorCause,
    pub metadata: ActionExecutionMetadata,
}

#[derive(Error, Debug)]
pub enum ActionErrorCause {
    #[error("Action failed to produce outputs: {}", error_items(.0))]
    MissingOutputs(Vec<ProjectRelativePathBuf>),
    #[error("Action didn't produce the right set of outputs.\nExpected {}`\nGot {}", error_items(.wanted), error_items(.got))]
    MismatchedOutputs {
        wanted: Vec<ProjectRelativePathBuf>,
        got: Vec<ProjectRelativePathBuf>,
    },
    #[error("Command timed out after {:.3}s", .elapsed.as_secs_f64())]
    TimedOut { elapsed: Duration },
    #[error(
        "Command failed with exit code {}",
        .exit_code.map(|v| v.to_string()).unwrap_or_else(|| "no_code".to_owned()),
    )]
    CommandFailed { exit_code: Option<i32> },
}

/// This is the result of the action as exposed to other things in the dice computation.
#[derive(Clone, Dupe, Debug, PartialEq, Eq)]
pub struct ActionOutputs(Arc<ActionOutputsData>);

#[derive(Derivative, Debug)]
#[derivative(PartialEq, Eq)]
struct ActionOutputsData {
    outputs: IndexMap<BuildArtifact, ArtifactValue>,
}

#[derive(Copy, Dupe, Clone, Debug, PartialEq, Eq)]
pub struct ActionExecutionTimingData {
    pub wall_time: Duration,
}

impl Default for ActionExecutionTimingData {
    fn default() -> Self {
        Self {
            wall_time: Duration::ZERO,
        }
    }
}

/// Metadata associated with the execution of this action.
#[derive(Debug)]
pub struct ActionExecutionMetadata {
    pub execution_kind: ActionExecutionKind,
    pub timing: ActionExecutionTimingData,
    pub std_streams: CommandStdStreams,
}

/// The *way* that a particular action was executed.
#[derive(Debug, Display)]
pub enum ActionExecutionKind {
    /// This action was executed locally.
    #[display(fmt = "local")]
    Local {
        command: Vec<String>,
        env: HashMap<String, String>,
    },
    /// This action was executed via a remote executor.
    #[display(fmt = "remote")]
    Remote { digest: ActionDigest },
    /// This action was served by the action cache and not executed.
    #[display(fmt = "action_cache")]
    ActionCache { digest: ActionDigest },
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

impl ActionExecutionKind {
    pub fn as_enum(&self) -> buck2_data::ActionExecutionKind {
        match self {
            ActionExecutionKind::Local { .. } => buck2_data::ActionExecutionKind::Local,
            ActionExecutionKind::Remote { .. } => buck2_data::ActionExecutionKind::Remote,
            ActionExecutionKind::ActionCache { .. } => buck2_data::ActionExecutionKind::ActionCache,
            ActionExecutionKind::Simple => buck2_data::ActionExecutionKind::Simple,
            ActionExecutionKind::Skipped => buck2_data::ActionExecutionKind::Skipped,
            ActionExecutionKind::Deferred => buck2_data::ActionExecutionKind::Deferred,
        }
    }

    pub fn as_local_command(&self) -> Option<buck2_data::LocalCommand> {
        match self {
            ActionExecutionKind::Local { command, env } => Some(buck2_data::LocalCommand {
                argv: command.to_owned(),
                env: env
                    .iter()
                    .map(|(key, value)| buck2_data::local_command::EnvironmentEntry {
                        key: key.clone(),
                        value: value.clone(),
                    })
                    .collect(),
            }),
            _ => None,
        }
    }

    pub fn as_remote_command(&self) -> Option<buck2_data::RemoteCommand> {
        match self {
            ActionExecutionKind::Remote { digest }
            | ActionExecutionKind::ActionCache { digest } => Some(buck2_data::RemoteCommand {
                action_digest: digest.to_string(),
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Eq, Hash, PartialEq, Clone, Dupe)]
pub struct LocalExecutorOptions {}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct RemoteExecutorUseCase(Cow<'static, str>);

impl RemoteExecutorUseCase {
    pub fn new(use_case: String) -> Self {
        Self(Cow::Owned(use_case))
    }
}

impl Default for RemoteExecutorUseCase {
    fn default() -> Self {
        Self(Cow::Borrowed("buck2-default"))
    }
}

impl From<RemoteExecutorUseCase> for String {
    fn from(use_case: RemoteExecutorUseCase) -> String {
        use_case.0.into_owned()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Default, Hash)]
pub struct RemoteExecutorOptions {
    pub re_properties: SmallMap<String, String>,
    pub re_action_key: Option<String>,
    pub re_max_input_files_bytes: Option<u64>,
    pub re_use_case: RemoteExecutorUseCase,
}

#[derive(Debug, Error)]
enum ExecutorConfigError {
    #[error("Action executor config must have at least one of local or remote options")]
    MissingLocalAndRemote,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum CommandExecutorKind {
    Local(LocalExecutorOptions),
    Remote(RemoteExecutorOptions),
    Hybrid {
        local: LocalExecutorOptions,
        remote: RemoteExecutorOptions,
        level: HybridExecutionLevel,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Dupe, Hash)]
pub enum PathSeparatorKind {
    Unix,
    Windows,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct CommandExecutorConfig {
    pub executor_kind: CommandExecutorKind,
    pub path_separator: PathSeparatorKind,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash)]
pub enum HybridExecutionLevel {
    /// Expose both executors but only run it in one preferred executor.
    Limited,
    /// Expose both executors, fallback to the non-preferred executor if execution on the preferred
    /// executor doesn't provide a succesful response. By default, we fallback only on errors (i.e.
    /// the infra failed), but not on failures (i.e. the job exited with 1). If
    /// `fallback_on_failure` is set, then we also fallback on failures.
    Fallback { fallback_on_failure: bool },
    /// Race both executors.
    Full { fallback_on_failure: bool },
}

impl CommandExecutorKind {
    pub fn new(
        local: Option<LocalExecutorOptions>,
        remote: Option<RemoteExecutorOptions>,
        hybrid_level: HybridExecutionLevel,
    ) -> anyhow::Result<Self> {
        match (local, remote) {
            (None, None) => Err(ExecutorConfigError::MissingLocalAndRemote.into()),
            (None, Some(remote)) => Ok(Self::Remote(remote)),
            (Some(local), None) => Ok(Self::Local(local)),
            (Some(local), Some(remote)) => Ok(Self::Hybrid {
                local,
                remote,
                level: hybrid_level,
            }),
        }
    }
}

impl CommandExecutorConfig {
    pub fn new(executor_kind: CommandExecutorKind, path_separator: PathSeparatorKind) -> Self {
        Self {
            executor_kind,
            path_separator,
        }
    }

    pub fn new_with_default_path_separator(executor_kind: CommandExecutorKind) -> Self {
        Self {
            executor_kind,
            path_separator: if cfg!(windows) {
                PathSeparatorKind::Windows
            } else {
                PathSeparatorKind::Unix
            },
        }
    }

    #[cfg(test)]
    pub fn testing_local() -> Self {
        Self::new_with_default_path_separator(CommandExecutorKind::Local(LocalExecutorOptions {}))
    }
}

impl ActionOutputs {
    pub fn new(outputs: IndexMap<BuildArtifact, ArtifactValue>) -> Self {
        Self(Arc::new(ActionOutputsData { outputs }))
    }

    pub fn from_single(artifact: BuildArtifact, value: ArtifactValue) -> Self {
        Self::new(indexmap! {artifact => value})
    }

    pub fn get(&self, artifact: &BuildArtifact) -> Option<&ArtifactValue> {
        self.0.outputs.get(artifact)
    }
}

/// Executes 'Actions'
#[async_trait]
pub trait ActionExecutor: Send + Sync {
    async fn execute(
        &self,
        inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
        action: &RegisteredAction,
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)>;
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
        let artifact_fs = self.get_artifact_fs().await;
        let io_provider = self.global_data().get_io_provider();
        let project_fs = io_provider.fs();

        let command_executor =
            self.get_command_executor(&artifact_fs, project_fs, executor_config)?;
        let blocking_executor = self.get_blocking_executor();
        let materializer = self.per_transaction_data().get_materializer();
        let events = self.per_transaction_data().get_dispatcher().dupe();
        let run_action_knobs = self.per_transaction_data().get_run_action_knobs();

        Ok(Arc::new(BuckActionExecutor::new(
            artifact_fs.clone(),
            CommandExecutor::new(command_executor, artifact_fs),
            blocking_executor,
            materializer,
            events,
            run_action_knobs,
        )))
    }
}

pub struct BuckActionExecutor {
    artifact_fs: ArtifactFs,
    command_executor: CommandExecutor,
    blocking_executor: Arc<dyn BlockingExecutor>,
    materializer: Arc<dyn Materializer>,
    events: EventDispatcher,
    run_action_knobs: RunActionKnobs,
}

impl BuckActionExecutor {
    pub fn new(
        artifact_fs: ArtifactFs,
        command_executor: CommandExecutor,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materializer: Arc<dyn Materializer>,
        events: EventDispatcher,
        run_action_knobs: RunActionKnobs,
    ) -> Self {
        Self {
            artifact_fs,
            command_executor,
            blocking_executor,
            materializer,
            events,
            run_action_knobs,
        }
    }
}
struct BuckActionExecutionContext<'a> {
    executor: &'a BuckActionExecutor,
    action: &'a RegisteredAction,
    inputs: IndexMap<ArtifactGroup, ArtifactGroupValues>,
    outputs: &'a IndexSet<BuildArtifact>,
}

#[async_trait]
impl ActionExecutionCtx for BuckActionExecutionContext<'_> {
    fn target(&self) -> CommandExecutionTarget<'_> {
        CommandExecutionTarget {
            owner: self.action.owner(),
            category: self.action.category(),
            identifier: self.action.identifier(),
        }
    }

    fn fs(&self) -> &ArtifactFs {
        &self.executor.artifact_fs
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
        &*self.executor.blocking_executor
    }

    fn run_action_knobs(&self) -> RunActionKnobs {
        self.executor.run_action_knobs
    }

    async fn exec_cmd(
        &self,
        request: &CommandExecutionRequest,
    ) -> anyhow::Result<(
        IndexMap<CommandExecutionOutput, ArtifactValue>,
        ActionExecutionMetadata,
    )> {
        let action = self.target();
        let manager = CommandExecutionManager::new(
            self.executor.command_executor.name(),
            <dyn ClaimManager>::new_simple(),
            self.executor.events.dupe(),
        );
        let CommandExecutionResult {
            outputs,
            std_streams,
            exit_code,
            metadata,
        } = self
            .executor
            .command_executor
            .exec_cmd(action, request, manager)
            .await;

        match metadata.status {
            commands::ActionResultStatus::Success { execution_kind } => Ok((
                outputs,
                ActionExecutionMetadata {
                    execution_kind,
                    timing: metadata.timing.into(),
                    std_streams,
                },
            )),
            commands::ActionResultStatus::Failure { execution_kind } => Err(ActionError {
                cause: ActionErrorCause::CommandFailed { exit_code },
                metadata: ActionExecutionMetadata {
                    execution_kind,
                    timing: metadata.timing.into(),
                    std_streams,
                },
            }
            .into()),
            commands::ActionResultStatus::TimedOut {
                duration,
                execution_kind,
            } => Err(ActionError {
                cause: ActionErrorCause::TimedOut { elapsed: duration },
                metadata: ActionExecutionMetadata {
                    execution_kind,
                    timing: metadata.timing.into(),
                    std_streams,
                },
            }
            .into()),
            commands::ActionResultStatus::Error(stage, error) => {
                // The string "During execution" is parsed by the ingress tailer, please check it if you change this string!
                Err(error.context(format!("During execution, {}", stage)))
            }
            commands::ActionResultStatus::ClaimRejected => {
                panic!("should be impossible for the executor to finish with a rejected claim")
            }
        }
    }

    async fn cleanup_outputs(&mut self) -> anyhow::Result<()> {
        // Delete all outputs before we start, so things will be clean.
        let output_paths = self
            .outputs
            .iter()
            .map(|o| self.executor.artifact_fs.resolve_build(o))
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
                .remove_paths_recursive(self.executor.artifact_fs.fs(), output_paths)
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
    ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
        let outputs = action.outputs()?;

        let execution_result: anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> = try {
            let mut ctx = BuckActionExecutionContext {
                executor: self,
                action,
                inputs,
                outputs: outputs.as_ref(),
            };

            let (result, metadata) = match action.as_executable() {
                ActionExecutable::Pristine(exe) => {
                    ctx.cleanup_outputs().await?;
                    exe.execute(&ctx).await?
                }
                ActionExecutable::Incremental(exe) => {
                    // Let the action perform clean up in this case.
                    exe.execute(&mut ctx).await?
                }
            };

            // Check all the outputs were returned, and no additional outputs
            // TODO (T122966509): Check projections here as well
            if !outputs.iter().eq(result.0.outputs.keys()) {
                let wanted = outputs
                    .iter()
                    .filter(|x| !result.0.outputs.contains_key(*x))
                    .map(|x| self.artifact_fs.resolve_build(x))
                    .collect();
                let got = result
                    .0
                    .outputs
                    .keys()
                    .filter(|x| !outputs.contains(*x))
                    .map(|x| self.artifact_fs.resolve_build(x))
                    .collect::<Vec<_>>();
                if got.is_empty() {
                    Err(ActionError {
                        cause: ActionErrorCause::MissingOutputs(wanted),
                        metadata,
                    })
                } else {
                    Err(ActionError {
                        cause: ActionErrorCause::MismatchedOutputs { wanted, got },
                        metadata,
                    })
                }
            } else {
                Ok((result, metadata))
            }?
        };

        execution_result.with_context(|| {
            format!(
                "Error when executing action `{}` for rule `{}`",
                action.name(),
                action.owner()
            )
        })
    }
}

/// IoRequest we dispatch to the blocking executor to clear output paths.
pub struct CleanOutputPaths {
    paths: Vec<ProjectRelativePathBuf>,
}

impl CleanOutputPaths {
    pub(crate) fn clean<'a>(
        paths: impl Iterator<Item = &'a ProjectRelativePath>,
        fs: &'a ProjectFilesystem,
    ) -> anyhow::Result<()> {
        for path in paths {
            fs.remove_path_recursive(path)?;

            if let Some(parent) = path.parent() {
                if let Err(err) = fs.create_dir(parent) {
                    // Be aware of T85589819 - the parent directory might already exist, but as a _file_.
                    // It might be even worse, it might be 2 parents up, which will cause create_dir to fail.
                    // We know create_dir fails, so start walking upwards looking for a file to delete.
                    // It's safe to delete this file because we know it doesn't overlap with a current output,
                    // or that check would have failed, so it must be a stale file.
                    let mut x = parent;
                    loop {
                        match fs.symlink_metadata(x) {
                            Ok(m) => {
                                if m.is_dir() {
                                    // Not sure why we couldn't create this, just rethrow the original error
                                    return Err(err);
                                } else {
                                    // Seems like it's a file, so delete it, then try creating again
                                    fs.remove_path_recursive(x)?;
                                    fs.create_dir(parent)?;
                                    break;
                                }
                            }
                            _ => {
                                match x.parent() {
                                    None => {
                                        // We weren't able to find something to remedy,
                                        // and have reached the root, so have to give up
                                        return Err(err);
                                    }
                                    Some(p) => {
                                        // Try again one level up
                                        x = p;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

impl IoRequest for CleanOutputPaths {
    fn execute(self: Box<Self>, project_fs: &ProjectFilesystem) -> anyhow::Result<()> {
        Self::clean(self.paths.iter().map(AsRef::as_ref), project_fs)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        borrow::Cow,
        collections::HashMap,
        convert::TryFrom,
        sync::{
            atomic::{AtomicBool, Ordering},
            Arc, Mutex,
        },
    };

    use async_trait::async_trait;
    use buck2_core::{
        category::Category,
        cells::{paths::CellRelativePath, testing::CellResolverExt, CellName, CellResolver},
        configuration::Configuration,
        fs::{
            paths::ForwardRelativePathBuf,
            project::{ProjectFilesystemTemp, ProjectRelativePathBuf},
        },
        package::{Package, PackageRelativePathBuf},
        target::{testing::ConfiguredTargetLabelExt, ConfiguredTargetLabel, TargetName},
    };
    use events::dispatch::EventDispatcher;
    use gazebo::prelude::*;
    use indexmap::{indexset, IndexSet};
    use once_cell::sync::Lazy;

    use crate::{
        actions::{
            artifact::{
                testing::BuildArtifactTestingExt, Artifact, ArtifactFs, ArtifactValue,
                BuildArtifact, SourceArtifact,
            },
            Action, ActionExecutable, ActionExecutionCtx, PristineActionExecutable,
            RegisteredAction,
        },
        artifact_groups::{ArtifactGroup, ArtifactGroupValues},
        deferred::{
            testing::{DeferredDataExt, DeferredIdExt},
            BaseDeferredKey, DeferredData, DeferredId, DeferredKey,
        },
        execute::{
            blocking::testing::DummyBlockingExecutor,
            commands::{
                dry_run::DryRunExecutor, output::CommandStdStreams, CommandExecutionInput,
                CommandExecutionRequest, CommandExecutor,
            },
            materializer::nodisk::NoDiskMaterializer,
            ActionExecutionKind, ActionExecutionMetadata, ActionExecutionTimingData,
            ActionExecutor, ActionOutputs, BuckActionExecutor, CommandExecutorConfig,
        },
        path::{BuckOutPathResolver, BuckPath, BuckPathResolver},
    };

    #[tokio::test]
    async fn can_execute_some_action() {
        let cells = CellResolver::of_names_and_paths(&[(
            CellName::unchecked_new("cell".into()),
            ProjectRelativePathBuf::unchecked_new("cell_path".into()),
        )]);

        let temp_fs = ProjectFilesystemTemp::new().unwrap();

        let project_fs = temp_fs.path().clone();
        let artifact_fs = ArtifactFs::new(
            BuckPathResolver::new(cells),
            BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
                "cell/buck-out/v2".into(),
            )),
            project_fs.clone(),
        );

        let tracker = Arc::new(Mutex::new(Vec::new()));

        let executor = BuckActionExecutor::new(
            artifact_fs.clone(),
            CommandExecutor::new(
                Arc::new(DryRunExecutor::new(tracker, None)),
                artifact_fs.clone(),
            ),
            Arc::new(DummyBlockingExecutor { fs: project_fs }),
            Arc::new(NoDiskMaterializer),
            EventDispatcher::null(),
            Default::default(),
        );

        #[derive(Debug)]
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
                ctx: &dyn ActionExecutionCtx,
            ) -> anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> {
                self.ran.store(true, Ordering::SeqCst);

                let req = CommandExecutionRequest::new(
                    vec!["foo".to_owned(), "bar".to_owned(), "cmd".to_owned()],
                    self.inputs
                        .iter()
                        .map(|x| {
                            CommandExecutionInput::Artifact(ArtifactGroupValues::from_artifact(
                                x.unpack_artifact().unwrap().dupe(),
                                ArtifactValue::empty_file(),
                            ))
                        })
                        .collect(),
                    self.outputs.clone(),
                    HashMap::new(),
                );

                // on fake executor, this does nothing
                let res = ctx.exec_cmd(&req).await;

                // Must write out the things we promised to do
                for x in &self.outputs {
                    ctx.fs().write_file(x, "", false)?
                }

                res?;
                let outputs = self
                    .outputs
                    .iter()
                    .map(|o| (o.dupe(), ArtifactValue::empty_file()))
                    .collect();
                Ok((
                    ActionOutputs::new(outputs),
                    ActionExecutionMetadata {
                        execution_kind: ActionExecutionKind::Simple,
                        timing: ActionExecutionTimingData::default(),
                        std_streams: CommandStdStreams::Empty,
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
            DeferredData::testing_new(DeferredKey::Base(
                BaseDeferredKey::TargetLabel(label.dupe()),
                DeferredId::testing_new(0),
            )),
            box TestingAction {
                inputs,
                outputs: outputs.clone(),
                ran: Default::default(),
            },
            CommandExecutorConfig::testing_local(),
        );
        let res = executor.execute(Default::default(), &action).await.unwrap();
        let outputs = outputs
            .iter()
            .map(|o| (o.dupe(), ArtifactValue::empty_file()))
            .collect();
        assert_eq!(res.0, ActionOutputs::new(outputs));
    }
}
