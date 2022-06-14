/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::HashMap,
    convert::Infallible,
    fmt::Display,
    ops::{ControlFlow, FromResidual},
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, SystemTime},
};

use async_trait::async_trait;
use buck2_common::file_ops::{FileMetadata, TrackedFileDigest};
use buck2_core::{
    category::Category,
    directory::{DirectoryEntry, DirectoryIterator, FingerprintedDirectory},
    fs::{
        paths::ForwardRelativePath,
        project::{ProjectRelativePath, ProjectRelativePathBuf},
    },
};
use derive_more::From;
use events::dispatch::EventDispatcher;
use futures::future::Future;
use gazebo::{prelude::*, variants::UnpackVariants};
use host_sharing::HostSharingRequirements;
use indexmap::{IndexMap, IndexSet};
use remote_execution as RE;

use crate::{
    actions::{
        artifact::{ArtifactFs, ArtifactValue, BuildArtifact, ExecutorFs},
        directory::{insert_entry, ActionDirectoryBuilder, ActionDirectoryMember},
    },
    artifact_groups::ArtifactGroupValues,
    deferred::BaseDeferredKey,
    execute::{
        commands::{
            output::CommandStdStreams,
            re::{
                client::{re_create_action, PreparedAction},
                ActionPaths,
            },
        },
        ActionExecutionKind, ActionExecutionTimingData, PathSeparatorKind,
    },
    path::{BuckOutPath, BuckOutScratchPath, BuckOutTestPath},
};

pub mod dice_data;
#[cfg(test)]
pub mod dry_run;
pub mod hybrid;
pub mod local;
pub mod output;
pub mod re;

/// CommandExecutionResult is the result of an executor executing a command.
#[derive(Debug)]
pub struct CommandExecutionResult {
    /// The outputs produced by this command
    pub outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,

    pub std_streams: CommandStdStreams,
    pub exit_code: Option<i32>,

    /// metadata holds information about **how** the result was produced rather than information about the result itself.
    pub metadata: CommandExecutionMetadata,
}

impl CommandExecutionResult {
    pub fn metadata(&self) -> &CommandExecutionMetadata {
        &self.metadata
    }
}

/// Implement FromResidual so that it's easier to refactor functions returning a CommandExecutionResult
/// (it allows to easily factor out early returns into another function and then propagate them with `?`).
impl FromResidual<ControlFlow<Self, Infallible>> for CommandExecutionResult {
    fn from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(v) => v,
            ControlFlow::Continue(_) => unreachable!(),
        }
    }
}

/// Unlike action where we only really have just 1 time, commands can have slightly richer timing
/// data.
#[derive(Debug)]
pub struct CommandExecutionTimingData {
    /// How long this build actually waited for this action to complete
    pub wall_time: Duration,

    /// How long this command actually took to execute. This can be different from the wall_time if
    /// this was e.g. an action cache hit, in which case this field would reflect how long the
    /// command took to actually execute but not how we had to wait for it.
    pub execution_time: Duration,

    /// When execution started.
    pub start_time: SystemTime,
}

impl Default for CommandExecutionTimingData {
    fn default() -> Self {
        Self {
            wall_time: Duration::default(),
            execution_time: Duration::default(),
            start_time: SystemTime::now(),
        }
    }
}

impl From<CommandExecutionTimingData> for ActionExecutionTimingData {
    fn from(command: CommandExecutionTimingData) -> Self {
        Self {
            wall_time: command.wall_time,
        }
    }
}

#[derive(Debug)]
pub struct CommandExecutionMetadata {
    // This should contain the ClaimedRequest if this execution successfully claimed it.
    pub claim: Option<ClaimedRequest>,
    pub status: ActionResultStatus,
    pub executor: ExecutorName,
    pub timing: CommandExecutionTimingData,
}

pub trait ClaimManager: Send + Sync + 'static {
    fn try_claim(&self) -> bool;
}

impl dyn ClaimManager {
    pub fn new_simple() -> Arc<dyn ClaimManager> {
        Arc::new(AtomicBool::new(false))
    }
}

impl ClaimManager for AtomicBool {
    fn try_claim(&self) -> bool {
        !self.fetch_or(true, Ordering::SeqCst)
    }
}

#[derive(Debug)]
pub struct ClaimedRequest {}

/// This tracker helps track the information that will go into the BuckCommandExecutionMetadata
pub struct CommandExecutionManager {
    executor_name: ExecutorName,
    claimed: bool,

    claim_manager: Arc<dyn ClaimManager>,
    events: EventDispatcher,
}

impl CommandExecutionManager {
    pub fn new(
        executor_name: ExecutorName,
        claim_manager: Arc<dyn ClaimManager>,
        events: EventDispatcher,
    ) -> Self {
        Self {
            executor_name,
            claim_manager,
            claimed: false,
            events,
        }
    }

    pub fn stage<T, F: FnOnce() -> T>(
        &mut self,
        stage: impl Into<buck2_data::executor_stage_start::Stage>,
        f: F,
    ) -> T {
        let event = buck2_data::ExecutorStageStart {
            stage: Some(stage.into()),
        };

        self.events
            .span(event, || (f(), buck2_data::ExecutorStageEnd {}))
    }

    pub async fn stage_async<F: Future>(
        &mut self,
        stage: impl Into<buck2_data::executor_stage_start::Stage>,
        f: F,
    ) -> <F as Future>::Output {
        let event = buck2_data::ExecutorStageStart {
            stage: Some(stage.into()),
        };

        self.events
            .span_async(
                event,
                async move { (f.await, buck2_data::ExecutorStageEnd {}) },
            )
            .await
    }

    /// An exec_cmd request might go to multiple executors. try_claim() indicates that an executor wants to claim the work item.
    ///
    /// An executor must claim the request before making any local changes.
    /// An executor can claim the request at an earlier point (for example, to indicate it has started working on it and others shouldn't bother).
    ///
    /// This is primarily used by the hybrid executor to support sending work to both a local and a remote executor but only allowing one to actually produce a result.
    // TODO(cjhopman): Ideally there'd be some object that all local modifications of a command execution would flow through
    // and we could require executors get that object via making a claim, but that's tough right now.
    pub fn try_claim(&mut self) -> Option<ClaimedRequest> {
        if self.claim_manager.try_claim() {
            self.claimed = true;
            Some(ClaimedRequest {})
        } else {
            None
        }
    }

    pub fn claim_rejected(self) -> CommandExecutionResult {
        self.result(
            ActionResultStatus::ClaimRejected,
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
    }

    /// Explicitly takes a ClaimedRequest here (even though it's unused) to help implementors remember to claim things
    /// since a command can't be successful without making local changes.
    pub fn success(
        self,
        claim: ClaimedRequest,
        execution_kind: ActionExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        // just make the claim look used in the function signature.
        let _ = claim;
        self.result(
            ActionResultStatus::Success { execution_kind },
            outputs,
            std_streams,
            Some(0),
            timing,
        )
    }

    pub fn failure(
        self,
        execution_kind: ActionExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
    ) -> CommandExecutionResult {
        self.result(
            ActionResultStatus::Failure { execution_kind },
            outputs,
            std_streams,
            exit_code,
            CommandExecutionTimingData::default(),
        )
    }

    pub fn timeout(
        self,
        execution_kind: ActionExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        self.result(
            ActionResultStatus::TimedOut {
                duration,
                execution_kind,
            },
            IndexMap::new(),
            std_streams,
            None,
            timing,
        )
    }

    pub fn error(self, failure_type: String, err: anyhow::Error) -> CommandExecutionResult {
        self.result(
            ActionResultStatus::Error(failure_type, err),
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
    }

    fn result(
        self,
        status: ActionResultStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        CommandExecutionResult {
            outputs,
            std_streams,
            exit_code,
            metadata: CommandExecutionMetadata {
                claim: if self.claimed {
                    Some(ClaimedRequest {})
                } else {
                    None
                },
                status,
                executor: self.executor_name,
                timing,
            },
        }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Dupe, Hash)]
pub enum OutputCreationBehavior {
    Create,
    Parent,
}

/// The data contains the information about the command to be executed.
pub struct CommandExecutionRequest {
    args: Vec<String>,
    inputs: Vec<CommandExecutionInput>,
    artifact_outputs: IndexSet<BuildArtifact>,
    test_outputs: Option<IndexMap<BuckOutTestPath, OutputCreationBehavior>>,
    env: HashMap<String, String>,
    timeout: Option<Duration>,
    local_only: bool,
    // Run with a custom $TMPDIR, or just the standard system one
    custom_tmpdir: bool,
    host_sharing_requirements: HostSharingRequirements,
    /// Working directory, relative to the project root.
    working_directory: Option<ProjectRelativePathBuf>,
    /// Whether we should always prefetch stderr when executing. When it's needed, this lets us
    /// overlap stderr download with output donwloads, which might be marginally useful to improve
    /// latency.
    prefetch_lossy_stderr: bool,
}

impl CommandExecutionRequest {
    pub fn new(
        args: Vec<String>,
        inputs: Vec<CommandExecutionInput>,
        artifact_outputs: IndexSet<BuildArtifact>,
        env: HashMap<String, String>,
    ) -> Self {
        Self {
            args,
            inputs,
            artifact_outputs,
            test_outputs: None,
            env,
            timeout: None,
            local_only: false,
            custom_tmpdir: true,
            host_sharing_requirements: HostSharingRequirements::default(),
            working_directory: None,
            prefetch_lossy_stderr: false,
        }
    }

    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    pub fn with_local_only(mut self, local_only: bool) -> Self {
        self.local_only = local_only;
        self
    }

    pub fn with_custom_tmpdir(mut self, custom_tmpdir: bool) -> Self {
        self.custom_tmpdir = custom_tmpdir;
        self
    }

    pub fn with_host_sharing_requirements(
        mut self,
        host_sharing_requirements: HostSharingRequirements,
    ) -> Self {
        self.host_sharing_requirements = host_sharing_requirements;
        self
    }

    pub fn with_working_directory(mut self, working_directory: ProjectRelativePathBuf) -> Self {
        self.working_directory = Some(working_directory);
        self
    }

    pub fn with_test_outputs(
        mut self,
        test_outputs: IndexMap<BuckOutTestPath, OutputCreationBehavior>,
    ) -> Self {
        self.test_outputs = Some(test_outputs);
        self
    }

    pub fn with_prefetch_lossy_stderr(mut self, prefetch_lossy_stderr: bool) -> Self {
        self.prefetch_lossy_stderr = prefetch_lossy_stderr;
        self
    }

    pub fn prefetch_lossy_stderr(&self) -> bool {
        self.prefetch_lossy_stderr
    }

    pub fn args(&self) -> &[String] {
        &self.args
    }

    pub fn inputs(&self) -> &[CommandExecutionInput] {
        &self.inputs
    }

    pub fn outputs<'a>(&'a self) -> impl Iterator<Item = CommandExecutionOutputRef<'a>> + 'a {
        let artifact_outputs = self
            .artifact_outputs
            .iter()
            .map(CommandExecutionOutputRef::BuildArtifact);

        let test_outputs = self.test_outputs.as_ref().into_iter().flat_map(|outputs| {
            outputs
                .iter()
                .map(|(path, create)| CommandExecutionOutputRef::TestPath {
                    path,
                    create: *create,
                })
        });

        artifact_outputs.chain(test_outputs)
    }

    pub fn env(&self) -> &HashMap<String, String> {
        &self.env
    }

    pub fn timeout(&self) -> Option<Duration> {
        self.timeout
    }

    pub fn local_only(&self) -> bool {
        self.local_only
    }

    pub fn host_sharing_requirements(&self) -> &HostSharingRequirements {
        &self.host_sharing_requirements
    }

    pub fn working_directory(&self) -> Option<&ProjectRelativePath> {
        self.working_directory.as_deref()
    }
}

pub(crate) fn inputs_directory(
    inputs: &[CommandExecutionInput],
    fs: &ArtifactFs,
) -> anyhow::Result<ActionDirectoryBuilder> {
    let mut builder = ActionDirectoryBuilder::empty();
    for input in inputs {
        match input {
            CommandExecutionInput::Artifact(group) => {
                group.add_to_directory(&mut builder, fs)?;
            }
            CommandExecutionInput::ActionMetadata(metadata) => {
                let path = fs.buck_out_path_resolver().resolve_gen(&metadata.path);
                builder.insert(
                    &path,
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
                        digest: metadata.digest.dupe(),
                        is_executable: false,
                    })),
                )?;
            }
        };
    }
    Ok(builder)
}

#[derive(Clone)]
pub struct ActionMetadataBlob {
    pub data: Vec<u8>,
    pub digest: TrackedFileDigest,
    pub path: BuckOutPath,
}

#[derive(Clone)]
pub enum CommandExecutionInput {
    Artifact(ArtifactGroupValues),
    ActionMetadata(ActionMetadataBlob),
}

#[derive(From, UnpackVariants, PartialEq, Eq, Hash, Debug)]
pub enum CommandExecutionOutputRef<'a> {
    BuildArtifact(&'a BuildArtifact),
    TestPath {
        path: &'a BuckOutTestPath,
        create: OutputCreationBehavior,
    },
}

impl<'a> CommandExecutionOutputRef<'a> {
    /// Resolve this output to a ResolvedCommandExecutionOutput that allows access to the output
    /// path as well as any dirs to create.
    pub fn resolve(&self, fs: &ArtifactFs) -> ResolvedCommandExecutionOutput {
        match self {
            Self::BuildArtifact(artifact) => ResolvedCommandExecutionOutput {
                path: fs.resolve_build(artifact),
                create: OutputCreationBehavior::Parent,
            },
            Self::TestPath { path, create } => ResolvedCommandExecutionOutput {
                path: fs.buck_out_path_resolver().resolve_test(path),
                create: *create,
            },
        }
    }

    pub fn cloned(&self) -> CommandExecutionOutput {
        match self {
            Self::BuildArtifact(a) => CommandExecutionOutput::BuildArtifact((*a).dupe()),
            Self::TestPath { path, create } => CommandExecutionOutput::TestPath {
                path: (*path).clone(),
                create: *create,
            },
        }
    }
}

#[derive(From, UnpackVariants, PartialEq, Eq, Hash, Debug)]
pub enum CommandExecutionOutput {
    BuildArtifact(BuildArtifact),
    TestPath {
        path: BuckOutTestPath,
        create: OutputCreationBehavior,
    },
}

impl CommandExecutionOutput {
    pub fn as_ref<'a>(&'a self) -> CommandExecutionOutputRef<'a> {
        match self {
            Self::BuildArtifact(ref a) => CommandExecutionOutputRef::BuildArtifact(a),
            Self::TestPath { ref path, create } => CommandExecutionOutputRef::TestPath {
                path,
                create: *create,
            },
        }
    }
}

/// The path for an output, and what should be created before running the action that produces this
/// output.
pub struct ResolvedCommandExecutionOutput {
    path: ProjectRelativePathBuf,
    create: OutputCreationBehavior,
}

impl ResolvedCommandExecutionOutput {
    /// Return this output's path.
    pub fn path(&self) -> &ProjectRelativePath {
        &self.path
    }

    /// Return this output's path, ownedly.
    pub fn into_path(self) -> ProjectRelativePathBuf {
        self.path
    }

    /// Return the path that this output requires creating before running the action.
    pub fn path_to_create(&self) -> Option<&ProjectRelativePath> {
        match self.create {
            OutputCreationBehavior::Create => Some(&self.path),
            OutputCreationBehavior::Parent => self.path.parent(),
        }
    }
}

/// "Status" of an action execution indicating how it finished. E.g. "built_remotely", "local_fallback", "action_cache".
#[derive(Debug)]
pub enum ActionResultStatus {
    Success {
        execution_kind: ActionExecutionKind,
    },
    Failure {
        execution_kind: ActionExecutionKind,
    },
    Error(String, anyhow::Error),
    TimedOut {
        execution_kind: ActionExecutionKind,
        duration: Duration,
    },
    ClaimRejected,
}

impl Display for ActionResultStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ActionResultStatus::Success { execution_kind, .. } => {
                write!(f, "success {}", execution_kind,)
            }
            ActionResultStatus::Failure { execution_kind } => {
                write!(f, "failure {}", execution_kind,)
            }
            ActionResultStatus::Error(kind, err) => write!(f, "error:{}\n{:#}", kind, err),
            ActionResultStatus::TimedOut { duration, .. } => {
                write!(f, "timed out after {:.3}s", duration.as_secs_f64())
            }
            ActionResultStatus::ClaimRejected => write!(f, "claim_rejected!"),
        }
    }
}

/// Name of an executor. E.g. "remote", "local".
#[derive(Debug, Copy, Clone, Dupe)]
pub struct ExecutorName(&'static str);

#[derive(Clone, Dupe)]
pub struct CommandExecutor(Arc<CommandExecutorData>);

struct CommandExecutorData {
    inner: Arc<dyn PreparedCommandExecutor>,
    artifact_fs: ArtifactFs,
    path_separator: PathSeparatorKind,
}

impl CommandExecutor {
    pub fn new(
        inner: Arc<dyn PreparedCommandExecutor>,
        artifact_fs: ArtifactFs,
        path_separator: PathSeparatorKind,
    ) -> Self {
        Self(Arc::new(CommandExecutorData {
            inner,
            artifact_fs,
            path_separator,
        }))
    }

    pub fn name(&self) -> ExecutorName {
        self.0.inner.name()
    }

    pub fn executor_fs(&self) -> ExecutorFs {
        ExecutorFs::new(&self.0.artifact_fs, self.0.path_separator)
    }

    /// Execute a command.
    ///
    /// This intentionally does not return a Result since we want to capture information about the
    /// execution even if there are errors. Any errors can be propagated by converting them
    /// to a result with CommandExecutionManager::error.
    pub async fn exec_cmd(
        &self,
        action: CommandExecutionTarget<'_>,
        request: &CommandExecutionRequest,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let (manager, action_paths, prepared_action) = self.prepare(manager, request).await?;
        self.0
            .inner
            .exec_cmd(
                &PreparedCommand {
                    target: action,
                    request,
                    action_paths,
                    prepared_action,
                },
                manager,
            )
            .await
    }

    async fn prepare(
        &self,
        mut manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
    ) -> ControlFlow<CommandExecutionResult, (CommandExecutionManager, ActionPaths, PreparedAction)>
    {
        let (action_paths, action) = match manager.stage(buck2_data::PrepareAction {}, || {
            let action_paths = self.preamble(request.inputs(), request.outputs())?;
            let input_digest = action_paths.inputs.fingerprint();
            let outputs = action_paths.outputs.map(|x| x.as_str().to_owned());
            let action_metadata_blobs = request.inputs().iter().filter_map(|x| match x {
                CommandExecutionInput::Artifact(_) => None,
                CommandExecutionInput::ActionMetadata(metadata) => {
                    Some((metadata.data.clone(), metadata.digest.dupe()))
                }
            });
            let action = re_create_action(
                request.args().to_vec(),
                outputs.clone(),
                outputs,
                request.working_directory().map(|p| p.as_str().to_owned()),
                request.env(),
                input_digest,
                action_metadata_blobs,
                None,
                self.0.inner.re_platform().cloned(),
                false,
            );

            Ok((action_paths, action))
        }) {
            Ok(v) => v,
            Err(e) => return ControlFlow::Break(manager.error("prepare".into(), e)),
        };

        ControlFlow::Continue((manager, action_paths, action))
    }

    /// Return the inputs (in the form of a ActionImmutableDirectory) and the outputs for this
    /// action.
    fn preamble<'a>(
        &self,
        inputs: &[CommandExecutionInput],
        outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
    ) -> anyhow::Result<ActionPaths> {
        let mut builder = inputs_directory(inputs, &self.0.artifact_fs)?;

        let output_paths = outputs
            .map(|o| {
                let resolved = o.resolve(&self.0.artifact_fs);
                if let Some(dir) = resolved.path_to_create() {
                    builder.mkdir(dir)?;
                }
                anyhow::Ok(resolved.into_path())
            })
            .collect::<Result<Vec<_>, _>>()?;

        insert_entry(
            &mut builder,
            ForwardRelativePath::unchecked_new(".buckconfig"),
            DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata::empty())),
        )?;

        let input_dir = builder.fingerprint();

        let mut input_files_bytes = 0;

        for entry in input_dir.fingerprinted_unordered_walk().without_paths() {
            match entry {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                    input_files_bytes += f.digest.size();
                }
                _ => {}
            };
        }

        Ok(ActionPaths {
            inputs: input_dir,
            outputs: output_paths,
            input_files_bytes,
        })
    }
}

pub struct PreparedCommand<'a, 'b> {
    pub request: &'a CommandExecutionRequest,
    pub target: CommandExecutionTarget<'b>,
    pub action_paths: ActionPaths,
    pub prepared_action: PreparedAction,
}

#[async_trait]
pub trait PreparedCommandExecutor: Send + Sync {
    /// Execute a command.
    ///
    /// This intentionally does not return a Result since we want to capture information about the
    /// execution even if there are errors. Any errors can be propagated by converting them
    /// to a result with CommandExecutionManager::error.
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult;

    fn re_platform(&self) -> Option<&RE::Platform>;

    fn name(&self) -> ExecutorName;
}

/// Indicates why we are executing a given command.
#[derive(Copy, Clone, Dupe, Debug)]
pub struct CommandExecutionTarget<'a> {
    pub owner: &'a BaseDeferredKey,
    pub category: &'a Category,
    pub identifier: Option<&'a str>,
}

impl<'a> CommandExecutionTarget<'a> {
    pub fn re_action_key(&self) -> String {
        self.to_string()
    }

    pub fn re_affinity_key(&self) -> String {
        self.owner.to_string()
    }

    pub fn scratch_dir(&self) -> BuckOutScratchPath {
        BuckOutScratchPath::new(self.owner.dupe(), self.category, self.identifier).unwrap()
    }
}

impl<'a> Display for CommandExecutionTarget<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.owner, self.category)?;
        if let Some(id) = self.identifier {
            write!(f, " {}", id)?;
        }
        Ok(())
    }
}
