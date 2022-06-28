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

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;
use std::hash::Hash;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use blocking::BlockingExecutor;
use blocking::HasBlockingExecutor;
use blocking::IoRequest;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_interpreter::dice::HasEvents;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use events::dispatch::EventDispatcher;
use gazebo::prelude::*;
use indexmap::indexmap;
use indexmap::IndexMap;
use indexmap::IndexSet;
use starlark::collections::SmallMap;
use thiserror::Error;

use crate::actions::artifact::ArtifactFs;
use crate::actions::artifact::ArtifactValue;
use crate::actions::artifact::BuildArtifact;
use crate::actions::artifact::ExecutorFs;
use crate::actions::directory::ActionDirectoryMember;
use crate::actions::run::knobs::HasRunActionKnobs;
use crate::actions::run::knobs::RunActionKnobs;
use crate::actions::ActionExecutable;
use crate::actions::ActionExecutionCtx;
use crate::actions::RegisteredAction;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;
use crate::calculation::Calculation;
use crate::execute::commands::dice_data::HasCommandExecutor;
use crate::execute::commands::output::CommandStdStreams;
use crate::execute::commands::re::client::ActionDigest;
use crate::execute::commands::ClaimManager;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::commands::CommandExecutionOutput;
use crate::execute::commands::CommandExecutionReport;
use crate::execute::commands::CommandExecutionRequest;
use crate::execute::commands::CommandExecutionResult;
use crate::execute::commands::CommandExecutionStatus;
use crate::execute::commands::CommandExecutionTarget;
use crate::execute::commands::CommandExecutor;
use crate::execute::materializer::HasMaterializer;
use crate::execute::materializer::Materializer;

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

#[derive(Debug, Eq, PartialEq, Clone, Copy, Dupe, Hash)]
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

    pub fn calc_output_bytes(&self) -> u64 {
        let mut output_bytes = 0;
        for output in &self.0.outputs {
            let mut walk = unordered_entry_walk(output.1.entry().as_ref());
            while let Some((_path, entry)) = walk.next() {
                match entry {
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                        output_bytes += f.digest.size();
                    }
                    _ => {}
                }
            }
        }

        output_bytes
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
        anyhow::Result<(ActionOutputs, ActionExecutionMetadata)>,
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
        let io_provider = self.global_data().get_io_provider();
        let project_fs = io_provider.fs();

        let command_executor =
            self.get_command_executor(&artifact_fs, project_fs, executor_config)?;
        let blocking_executor = self.get_blocking_executor();
        let materializer = self.per_transaction_data().get_materializer();
        let events = self.per_transaction_data().get_dispatcher().dupe();
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
            run_action_knobs,
        )))
    }
}

pub struct BuckActionExecutor {
    command_executor: CommandExecutor,
    blocking_executor: Arc<dyn BlockingExecutor>,
    materializer: Arc<dyn Materializer>,
    events: EventDispatcher,
    run_action_knobs: RunActionKnobs,
}

impl BuckActionExecutor {
    pub fn new(
        command_executor: CommandExecutor,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materializer: Arc<dyn Materializer>,
        events: EventDispatcher,
        run_action_knobs: RunActionKnobs,
    ) -> Self {
        Self {
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
    #[allow(unused)]
    command_reports: &'a mut Vec<CommandExecutionReport>,
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
        &*self.executor.blocking_executor
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
            self.executor.command_executor.name(),
            <dyn ClaimManager>::new_simple(),
            self.executor.events.dupe(),
        );
        let CommandExecutionResult {
            outputs,
            report:
                CommandExecutionReport {
                    std_streams,
                    exit_code,
                    metadata,
                },
        } = self
            .executor
            .command_executor
            .exec_cmd(action, request, manager)
            .await;

        match metadata.status {
            CommandExecutionStatus::Success { execution_kind } => Ok((
                outputs,
                ActionExecutionMetadata {
                    execution_kind,
                    timing: metadata.timing.into(),
                    std_streams,
                },
            )),
            CommandExecutionStatus::Failure { execution_kind } => Err(ActionError {
                cause: ActionErrorCause::CommandFailed { exit_code },
                metadata: ActionExecutionMetadata {
                    execution_kind,
                    timing: metadata.timing.into(),
                    std_streams,
                },
            }
            .into()),
            CommandExecutionStatus::TimedOut {
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
            CommandExecutionStatus::Error { stage, error } => {
                // The string "During execution" is parsed by the ingress tailer, please check it if you change this string!
                Err(error.context(format!("During execution, {}", stage)))
            }
            CommandExecutionStatus::ClaimRejected => {
                panic!("should be impossible for the executor to finish with a rejected claim")
            }
        }
    }

    async fn cleanup_outputs(&mut self) -> anyhow::Result<()> {
        // Delete all outputs before we start, so things will be clean.
        let output_paths = self
            .outputs
            .iter()
            .map(|o| self.fs().resolve_build(o))
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
        anyhow::Result<(ActionOutputs, ActionExecutionMetadata)>,
        Vec<CommandExecutionReport>,
    ) {
        let mut command_reports = Vec::new();

        let res = async {
            let outputs = action.outputs()?;

            let execution_result: anyhow::Result<(ActionOutputs, ActionExecutionMetadata)> = try {
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
                if !outputs.iter().eq(result.0.outputs.keys()) {
                    let wanted = outputs
                        .iter()
                        .filter(|x| !result.0.outputs.contains_key(*x))
                        .map(|x| self.command_executor.fs().resolve_build(x))
                        .collect();
                    let got = result
                        .0
                        .outputs
                        .keys()
                        .filter(|x| !outputs.contains(*x))
                        .map(|x| self.command_executor.fs().resolve_build(x))
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
        .await;

        (res, command_reports)
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
            cleanup_path(fs, path)
                .with_context(|| format!("Error cleaning up output path `{}`", path))?;
        }
        Ok(())
    }
}

fn cleanup_path(fs: &ProjectFilesystem, mut path: &ProjectRelativePath) -> anyhow::Result<()> {
    fs.remove_path_recursive(path)?;

    // Be aware of T85589819 - the parent directory might already exist, but as a _file_.  It might
    // be even worse, it might be 2 parents up, which will cause create_dir to fail when we try to
    // execute. So, we walk up the tree until we either find a dir we're happy with, or a file we
    // can delete. It's safe to delete this file because we know it doesn't overlap with a current
    // output, or that check would have failed, so it must be a stale file.
    loop {
        path = match path.parent() {
            Some(path) => path,
            None => {
                return Err(anyhow::anyhow!(
                    "Internal Error: reached root before finding a directory that exists!"
                ));
            }
        };

        match fs.symlink_metadata(path) {
            Ok(m) => {
                if m.is_dir() {
                    // It's a dir, no need to go further, and no need to delete.
                } else {
                    // There was a file , so it's safe to delete and then we can exit because we'll
                    // be able to create a dir here.
                    fs.remove_path_recursive(path)?;
                }
                return Ok(());
            }
            Err(e) => {
                #[cfg(unix)]
                {
                    use std::io;

                    // If we get ENOENT that guarantees there is no file on the path. If there was
                    // one, we would get ENOTDIR. TODO (T123279320) This probably works on Windows,
                    // but it wasn't tested there.
                    let is_enoent = e.downcast_ref::<io::Error>().map(|e| e.kind())
                        == Some(io::ErrorKind::NotFound);

                    if is_enoent {
                        return Ok(());
                    }
                }

                #[cfg(not(unix))]
                {
                    // On non-Unix we don't have the optimization above. Recursing all the way up
                    // until we find the first dir (or file to delete) is fine. There will
                    // eventually be *a* directory (at buck-out, then another one at the empty
                    // directory, which is our cwd, and should exist by now).
                    let _e = e;
                }

                // Continue going up. Eventually we should reach the output directory, which should
                // exist.
            }
        }
    }
}

impl IoRequest for CleanOutputPaths {
    fn execute(self: Box<Self>, project_fs: &ProjectFilesystem) -> anyhow::Result<()> {
        Self::clean(self.paths.iter().map(AsRef::as_ref), project_fs)
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;
    use std::collections::HashMap;
    use std::convert::TryFrom;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;
    use std::sync::Mutex;

    use async_trait::async_trait;
    use buck2_core::buck_path::BuckPath;
    use buck2_core::category::Category;
    use buck2_core::cells::paths::CellRelativePath;
    use buck2_core::cells::testing::CellResolverExt;
    use buck2_core::cells::CellName;
    use buck2_core::cells::CellResolver;
    use buck2_core::configuration::Configuration;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use buck2_core::fs::project::ProjectFilesystemTemp;
    use buck2_core::fs::project::ProjectRelativePath;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_core::package::Package;
    use buck2_core::package::PackageRelativePathBuf;
    use buck2_core::target::testing::ConfiguredTargetLabelExt;
    use buck2_core::target::ConfiguredTargetLabel;
    use buck2_core::target::TargetName;
    use events::dispatch::EventDispatcher;
    use gazebo::prelude::*;
    use indexmap::indexset;
    use indexmap::IndexSet;
    use once_cell::sync::Lazy;

    use crate::actions::artifact::testing::BuildArtifactTestingExt;
    use crate::actions::artifact::Artifact;
    use crate::actions::artifact::ArtifactFs;
    use crate::actions::artifact::ArtifactValue;
    use crate::actions::artifact::BuildArtifact;
    use crate::actions::artifact::SourceArtifact;
    use crate::actions::Action;
    use crate::actions::ActionExecutable;
    use crate::actions::ActionExecutionCtx;
    use crate::actions::PristineActionExecutable;
    use crate::actions::RegisteredAction;
    use crate::artifact_groups::ArtifactGroup;
    use crate::artifact_groups::ArtifactGroupValues;
    use crate::deferred::testing::DeferredDataExt;
    use crate::deferred::testing::DeferredIdExt;
    use crate::deferred::BaseDeferredKey;
    use crate::deferred::DeferredData;
    use crate::deferred::DeferredId;
    use crate::deferred::DeferredKey;
    use crate::execute::blocking::testing::DummyBlockingExecutor;
    use crate::execute::cleanup_path;
    use crate::execute::commands::dry_run::DryRunExecutor;
    use crate::execute::commands::output::CommandStdStreams;
    use crate::execute::commands::CommandExecutionInput;
    use crate::execute::commands::CommandExecutionRequest;
    use crate::execute::commands::CommandExecutor;
    use crate::execute::materializer::nodisk::NoDiskMaterializer;
    use crate::execute::ActionExecutionKind;
    use crate::execute::ActionExecutionMetadata;
    use crate::execute::ActionExecutionTimingData;
    use crate::execute::ActionExecutor;
    use crate::execute::ActionOutputs;
    use crate::execute::BuckActionExecutor;
    use crate::execute::CommandExecutorConfig;
    use crate::execute::PathSeparatorKind;
    use crate::path::BuckOutPathResolver;
    use crate::path::BuckPathResolver;

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
            CommandExecutor::new(
                Arc::new(DryRunExecutor::new(tracker, None)),
                artifact_fs,
                PathSeparatorKind::Unix,
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
                ctx: &mut dyn ActionExecutionCtx,
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
        let res = executor
            .execute(Default::default(), &action)
            .await
            .0
            .unwrap();
        let outputs = outputs
            .iter()
            .map(|o| (o.dupe(), ArtifactValue::empty_file()))
            .collect();
        assert_eq!(res.0, ActionOutputs::new(outputs));
    }

    #[test]
    fn test_cleanup_path_missing() -> anyhow::Result<()> {
        let fs = ProjectFilesystemTemp::new()?;
        let fs = fs.path();
        fs.create_dir(ProjectRelativePath::unchecked_new("foo/bar/qux"))?;
        cleanup_path(fs, ProjectRelativePath::unchecked_new("foo/bar/qux/xx"))?;
        assert!(fs.exists(ProjectRelativePath::unchecked_new("foo/bar/qux")));
        Ok(())
    }

    #[test]
    fn test_cleanup_path_present() -> anyhow::Result<()> {
        let fs = ProjectFilesystemTemp::new()?;
        let fs = fs.path();
        fs.create_dir(ProjectRelativePath::unchecked_new("foo/bar/qux"))?;
        cleanup_path(fs, ProjectRelativePath::unchecked_new("foo/bar/qux"))?;
        assert!(!fs.exists(ProjectRelativePath::unchecked_new("foo/bar/qux")));
        assert!(fs.exists(ProjectRelativePath::unchecked_new("foo/bar")));
        Ok(())
    }

    #[test]
    fn test_cleanup_path_overlap() -> anyhow::Result<()> {
        let fs = ProjectFilesystemTemp::new()?;
        let fs = fs.path();
        fs.write_file(ProjectRelativePath::unchecked_new("foo/bar"), "xx", false)?;
        cleanup_path(fs, ProjectRelativePath::unchecked_new("foo/bar/qux"))?;
        assert!(!fs.exists(ProjectRelativePath::unchecked_new("foo/bar")));
        assert!(fs.exists(ProjectRelativePath::unchecked_new("foo")));
        Ok(())
    }

    #[test]
    fn test_cleanup_path_overlap_deep() -> anyhow::Result<()> {
        let fs = ProjectFilesystemTemp::new()?;
        let fs = fs.path();
        fs.write_file(ProjectRelativePath::unchecked_new("foo/bar"), "xx", false)?;
        cleanup_path(
            fs,
            ProjectRelativePath::unchecked_new("foo/bar/qux/1/2/3/4"),
        )?;
        assert!(!fs.exists(ProjectRelativePath::unchecked_new("foo/bar")));
        assert!(fs.exists(ProjectRelativePath::unchecked_new("foo")));
        Ok(())
    }
}
