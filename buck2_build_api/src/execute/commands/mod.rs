/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::borrow_deref_ref)] // FIXME?

use std::collections::HashMap;
use std::convert::Infallible;
use std::ffi::OsString;
use std::fmt::Display;
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

use async_trait::async_trait;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::category::Category;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_node::execute::config::PathSeparatorKind;
use derive_more::Display;
use derive_more::From;
use events::dispatch::span;
use events::dispatch::span_async;
use events::dispatch::EventDispatcher;
use futures::future::Future;
use gazebo::prelude::*;
use gazebo::variants::UnpackVariants;
use host_sharing::HostSharingRequirements;
use indexmap::IndexMap;
use indexmap::IndexSet;
use once_cell::sync::OnceCell;
use remote_execution as RE;

use crate::actions::artifact::ArtifactFs;
use crate::actions::artifact::ArtifactValue;
use crate::actions::artifact::BuildArtifact;
use crate::actions::artifact::ExecutorFs;
use crate::actions::directory::insert_entry;
use crate::actions::directory::ActionDirectoryBuilder;
use crate::actions::directory::ActionDirectoryMember;
use crate::artifact_groups::ArtifactGroupValues;
use crate::deferred::BaseDeferredKey;
use crate::execute::commands::output::CommandStdStreams;
use crate::execute::commands::re::client::re_create_action;
use crate::execute::commands::re::client::PreparedAction;
use crate::execute::commands::re::ActionPaths;
use crate::execute::ActionDigest;
use crate::execute::ActionExecutionTimingData;
use crate::path::BuckOutPath;
use crate::path::BuckOutScratchPath;
use crate::path::BuckOutTestPath;

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
    /// How it executed.
    pub report: CommandExecutionReport,
    /// A previously rejected execution of this command.
    pub rejected_execution: Option<CommandExecutionReport>,
}

/// Describes how a command executed.
#[derive(Debug)]
pub struct CommandExecutionReport {
    /// This should contain the ClaimedRequest if this execution successfully claimed it.
    pub claim: Option<ClaimedRequest>,
    pub status: CommandExecutionStatus,
    pub executor: ExecutorName,
    pub timing: CommandExecutionTimingData,
    pub std_streams: CommandStdStreams,
    pub exit_code: Option<i32>,
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
#[derive(Debug, Copy, Clone, Dupe)]
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

        span(event, || (f(), buck2_data::ExecutorStageEnd {}))
    }

    pub async fn stage_async<F: Future>(
        &mut self,
        stage: impl Into<buck2_data::executor_stage_start::Stage>,
        f: F,
    ) -> <F as Future>::Output {
        let event = buck2_data::ExecutorStageStart {
            stage: Some(stage.into()),
        };
        span_async(
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
            CommandExecutionStatus::ClaimRejected,
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
        execution_kind: CommandExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        // just make the claim look used in the function signature.
        let _ = claim;
        self.result(
            CommandExecutionStatus::Success { execution_kind },
            outputs,
            std_streams,
            Some(0),
            timing,
        )
    }

    pub fn failure(
        self,
        execution_kind: CommandExecutionKind,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Failure { execution_kind },
            outputs,
            std_streams,
            exit_code,
            CommandExecutionTimingData::default(),
        )
    }

    pub fn timeout(
        self,
        execution_kind: CommandExecutionKind,
        duration: Duration,
        std_streams: CommandStdStreams,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::TimedOut {
                duration,
                execution_kind,
            },
            IndexMap::new(),
            std_streams,
            None,
            timing,
        )
    }

    pub fn error(self, stage: String, error: anyhow::Error) -> CommandExecutionResult {
        self.result(
            CommandExecutionStatus::Error { stage, error },
            IndexMap::new(),
            Default::default(),
            None,
            CommandExecutionTimingData::default(),
        )
    }

    fn result(
        self,
        status: CommandExecutionStatus,
        outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
        std_streams: CommandStdStreams,
        exit_code: Option<i32>,
        timing: CommandExecutionTimingData,
    ) -> CommandExecutionResult {
        CommandExecutionResult {
            outputs,
            report: CommandExecutionReport {
                claim: if self.claimed {
                    Some(ClaimedRequest {})
                } else {
                    None
                },
                status,
                executor: self.executor_name,
                timing,
                std_streams,
                exit_code,
            },
            rejected_execution: None,
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
    executor_preference: ExecutorPreference,
    // Run with a custom $TMPDIR, or just the standard system one
    custom_tmpdir: bool,
    host_sharing_requirements: HostSharingRequirements,
    /// Working directory, relative to the project root.
    working_directory: Option<ProjectRelativePathBuf>,
    /// Whether we should always prefetch stderr when executing. When it's needed, this lets us
    /// overlap stderr download with output donwloads, which might be marginally useful to improve
    /// latency.
    prefetch_lossy_stderr: bool,
    /// Whether to cleanup outputs
    outputs_cleanup: bool,
    /// What environment variables to inherit from the Buck2 daemon.
    local_environment_inheritance: Option<EnvironmentInheritance>,
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
            executor_preference: ExecutorPreference::Default,
            custom_tmpdir: true,
            host_sharing_requirements: HostSharingRequirements::default(),
            working_directory: None,
            prefetch_lossy_stderr: false,
            outputs_cleanup: true,
            local_environment_inheritance: None,
        }
    }

    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    pub fn with_executor_preference(mut self, executor_preference: ExecutorPreference) -> Self {
        self.executor_preference = executor_preference;
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

    pub fn with_outputs_cleanup(mut self, outputs_cleanup: bool) -> Self {
        self.outputs_cleanup = outputs_cleanup;
        self
    }

    pub fn prefetch_lossy_stderr(&self) -> bool {
        self.prefetch_lossy_stderr
    }

    pub fn outputs_cleanup(&self) -> bool {
        self.outputs_cleanup
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

    pub fn executor_preference(&self) -> ExecutorPreference {
        self.executor_preference
    }

    pub fn host_sharing_requirements(&self) -> &HostSharingRequirements {
        &self.host_sharing_requirements
    }

    pub fn working_directory(&self) -> Option<&ProjectRelativePath> {
        self.working_directory.as_deref()
    }

    pub fn with_local_environment_inheritance(
        mut self,
        local_environment_inheritance: EnvironmentInheritance,
    ) -> Self {
        self.local_environment_inheritance = Some(local_environment_inheritance);
        self
    }

    pub fn local_environment_inheritance(&self) -> Option<&EnvironmentInheritance> {
        self.local_environment_inheritance.as_ref()
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

#[derive(Copy, Clone, Dupe, Debug)]
pub struct EnvironmentInheritance {
    values: &'static [(&'static str, OsString)],
}

impl EnvironmentInheritance {
    pub fn test_allowlist() -> Self {
        // This is made to be a list of lists in case we want to include lists from different
        // provenances, like the test_env_allowlist::ENV_LIST_HACKY.
        let allowlists;

        #[cfg(fbcode_build)]
        {
            allowlists = &[test_env_allowlist::LEGACY_TESTPILOT_ALLOW_LIST];
        }

        #[cfg(not(fbcode_build))]
        {
            // NOTE: Not much thought has gone into this, since we don't actually use this
            // codepath. In theory we should probably omit PATH here, but we're likely to want
            // to use this for e.g. genrules and that will *not* be practical...
            allowlists = &[&["PATH", "USER", "LOGNAME", "HOME", "TMPDIR"]];
        }

        // We create this *once* since getenv is actually not cheap (being O(n) of the environment
        // size).
        static TEST_CELL: OnceCell<Vec<(&'static str, OsString)>> = OnceCell::new();

        let values = TEST_CELL.get_or_init(|| {
            let mut ret = Vec::new();
            for list in allowlists.iter() {
                for key in list.iter() {
                    if let Some(value) = std::env::var_os(key) {
                        ret.push((*key, value));
                    }
                }
            }
            ret
        });

        Self { values: &*values }
    }

    pub fn empty() -> Self {
        Self { values: &[] }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&'static str, &'static OsString)> {
        self.values.iter().map(|(k, v)| (*k, v))
    }
}

#[derive(Copy, Clone, Dupe, Display, Debug)]
pub enum ExecutorPreference {
    Default,
    /// Fails when executed by a remote-only executor
    LocalRequired,
    /// Does not fail when executed by a remote-only executor
    LocalPreferred,
}

impl ExecutorPreference {
    pub fn and(self, other: &Self) -> Self {
        if self.requires_local() || other.requires_local() {
            return Self::LocalRequired;
        }

        if self.prefers_local() || other.prefers_local() {
            return Self::LocalPreferred;
        }

        Self::Default
    }

    pub fn requires_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => false,
            Self::Default => false,
        }
    }

    pub fn prefers_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => true,
            Self::Default => false,
        }
    }
}

#[derive(Debug, Display, Clone)]
pub enum CommandExecutionKind {
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
}

impl CommandExecutionKind {
    pub fn as_enum(&self) -> buck2_data::ActionExecutionKind {
        match self {
            Self::Local { .. } => buck2_data::ActionExecutionKind::Local,
            Self::Remote { .. } => buck2_data::ActionExecutionKind::Remote,
            Self::ActionCache { .. } => buck2_data::ActionExecutionKind::ActionCache,
        }
    }
}

/// "Status" of an action execution indicating how it finished. E.g. "built_remotely", "local_fallback", "action_cache".
#[derive(Debug)]
pub enum CommandExecutionStatus {
    Success {
        execution_kind: CommandExecutionKind,
    },
    Failure {
        execution_kind: CommandExecutionKind,
    },
    Error {
        stage: String,
        error: anyhow::Error,
    },
    TimedOut {
        execution_kind: CommandExecutionKind,
        duration: Duration,
    },
    ClaimRejected,
}

impl CommandExecutionStatus {
    pub fn execution_kind(&self) -> Option<&CommandExecutionKind> {
        match self {
            CommandExecutionStatus::Success { execution_kind, .. } => Some(execution_kind),
            CommandExecutionStatus::Failure { execution_kind } => Some(execution_kind),
            CommandExecutionStatus::Error { .. } => None,
            CommandExecutionStatus::TimedOut { execution_kind, .. } => Some(execution_kind),
            CommandExecutionStatus::ClaimRejected => None,
        }
    }
}

impl Display for CommandExecutionStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommandExecutionStatus::Success { execution_kind, .. } => {
                write!(f, "success {}", execution_kind,)
            }
            CommandExecutionStatus::Failure { execution_kind } => {
                write!(f, "failure {}", execution_kind,)
            }
            CommandExecutionStatus::Error { stage, error } => {
                write!(f, "error:{}\n{:#}", stage, error)
            }
            CommandExecutionStatus::TimedOut { duration, .. } => {
                write!(f, "timed out after {:.3}s", duration.as_secs_f64())
            }
            CommandExecutionStatus::ClaimRejected => write!(f, "claim_rejected!"),
        }
    }
}

/// Name of an executor. E.g. "remote", "local".
#[derive(Debug, Copy, Clone, Dupe)]
pub struct ExecutorName(pub &'static str);

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

    pub fn fs(&self) -> &ArtifactFs {
        &self.0.artifact_fs
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
