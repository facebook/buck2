/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_core::execution_types::executor_config::RemoteExecutorCustomImage;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutScratchPath;
use buck2_core::fs::buck_out_path::BuckOutTestPath;
use buck2_core::fs::buck_out_path::BuildArtifactPath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::soft_error;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::buck2_error;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use host_sharing::host_sharing::HostSharingRequirements;
use indexmap::IndexSet;
use itertools::Itertools;
use prost::Message;
use remote_execution as RE;
use sorted_vector_map::SortedVectorMap;
use starlark_map::sorted_set::SortedSet;

use super::dep_file_digest::DepFileDigest;
use crate::artifact::group::artifact_group_values_dyn::ArtifactGroupValuesDyn;
use crate::digest_config::DigestConfig;
use crate::directory::insert_entry;
use crate::directory::ActionDirectoryMember;
use crate::directory::ActionImmutableDirectory;
use crate::execute::environment_inheritance::EnvironmentInheritance;
use crate::execute::inputs_directory::inputs_directory;
use crate::execute::paths_with_digest::PathsWithDigestBlobData;

/// What protobuf messages can be stored in the action metadata blobs.
pub trait ActionMetadataBlobMessage: Message {}

impl ActionMetadataBlobMessage for RE::Action {}
impl ActionMetadataBlobMessage for RE::Command {}
impl ActionMetadataBlobMessage for RE::Tree {}

#[derive(Clone)]
pub struct ActionMetadataBlobData(pub Vec<u8>);

impl ActionMetadataBlobData {
    pub fn from_message(m: &impl ActionMetadataBlobMessage) -> ActionMetadataBlobData {
        let mut blob = Vec::new();
        // Unwrap is safe because it only fails in OOM conditions, which we pretend don't happen
        m.encode(&mut blob).unwrap();
        ActionMetadataBlobData(blob)
    }

    pub fn from_json(json: String) -> ActionMetadataBlobData {
        ActionMetadataBlobData(json.into_bytes())
    }
}

#[derive(Clone)]
pub struct ActionMetadataBlob {
    pub data: PathsWithDigestBlobData,
    pub digest: TrackedFileDigest,
    pub path: BuildArtifactPath,
}

pub enum CommandExecutionInput {
    Artifact(Box<dyn ArtifactGroupValuesDyn>),
    ActionMetadata(ActionMetadataBlob),
    ScratchPath(BuckOutScratchPath),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Dupe, Hash)]
pub enum OutputCreationBehavior {
    Create,
    Parent,
}

#[derive(Debug, buck2_error::Error)]
#[error("Incompatible executor preferences: `{}` & `{}`", a, b)]
#[buck2(input)]
struct IncompatibleExecutorPreferences {
    a: ExecutorPreference,
    b: ExecutorPreference,
}

#[derive(Copy, Clone, Dupe, Display, Debug, Allocative)]
pub enum ExecutorPreference {
    Default,
    /// Fails when executed by a remote-only executor
    LocalRequired,
    /// Does not fail when executed by a remote-only executor
    LocalPreferred,
    /// Fails when executed by a local-only executor
    RemoteRequired,
    /// Does not fail when executed by a local-only executor
    RemotePreferred,
    /// When and-ed with an ExecutorPreference that doesn't *require* local or remote, this erases
    /// any preferences.
    DefaultErasePreferences,
}

impl ExecutorPreference {
    pub fn and(self, other: Self) -> buck2_error::Result<Self> {
        let requires_remote = self.requires_remote() || other.requires_remote();
        let requires_local = self.requires_local() || other.requires_local();

        if requires_remote && requires_local {
            return Err(IncompatibleExecutorPreferences { a: self, b: other }.into());
        }

        if requires_local {
            return Ok(Self::LocalRequired);
        }

        if requires_remote {
            return Ok(Self::RemoteRequired);
        }

        if self.erases_preferences() || other.erases_preferences() {
            return Ok(Self::DefaultErasePreferences);
        }

        for pref in [self, other] {
            if pref.prefers_local() {
                return Ok(Self::LocalPreferred);
            }

            if pref.prefers_remote() {
                return Ok(Self::RemotePreferred);
            }
        }

        Ok(Self::Default)
    }

    pub fn requires_remote(&self) -> bool {
        match self {
            Self::LocalRequired => false,
            Self::LocalPreferred => false,
            Self::RemoteRequired => true,
            Self::RemotePreferred => false,
            Self::Default => false,
            Self::DefaultErasePreferences => false,
        }
    }

    pub fn requires_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => false,
            Self::RemoteRequired => false,
            Self::RemotePreferred => false,
            Self::Default => false,
            Self::DefaultErasePreferences => false,
        }
    }

    pub fn prefers_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => true,
            Self::RemoteRequired => false,
            Self::RemotePreferred => false,
            Self::Default => false,
            Self::DefaultErasePreferences => false,
        }
    }

    pub fn prefers_remote(&self) -> bool {
        match self {
            Self::LocalRequired => false,
            Self::LocalPreferred => false,
            Self::RemoteRequired => true,
            Self::RemotePreferred => true,
            Self::Default => false,
            Self::DefaultErasePreferences => false,
        }
    }

    fn erases_preferences(self) -> bool {
        match self {
            Self::DefaultErasePreferences => true,
            _ => false,
        }
    }
}

pub struct CommandExecutionPaths {
    inputs: Vec<CommandExecutionInput>,
    outputs: IndexSet<CommandExecutionOutput>,

    input_directory: ActionImmutableDirectory,
    output_paths: Vec<(ProjectRelativePathBuf, OutputType)>,

    /// Total size of input files.
    input_files_bytes: u64,
}

impl CommandExecutionPaths {
    pub fn new(
        inputs: Vec<CommandExecutionInput>,
        outputs: IndexSet<CommandExecutionOutput>,
        fs: &ArtifactFs,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Self> {
        let mut builder = inputs_directory(&inputs, fs)?;

        let output_paths = outputs
            .iter()
            .map(|o| {
                let resolved = o.as_ref().resolve(fs);
                if let Some(dir) = resolved.path_to_create() {
                    builder.mkdir(dir)?;
                }
                let output_type = resolved.output_type;
                Ok((resolved.into_path(), output_type))
            })
            .collect::<buck2_error::Result<Vec<_>>>()?;

        insert_entry(
            &mut builder,
            ProjectRelativePath::unchecked_new(".buckconfig"),
            DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata::empty(
                digest_config.cas_digest_config(),
            ))),
        )?;

        let input_directory = builder.fingerprint(digest_config.as_directory_serializer());

        let mut input_files_bytes = 0;

        for entry in input_directory.unordered_walk_leaves().without_paths() {
            match entry {
                ActionDirectoryMember::File(f) => {
                    input_files_bytes += f.digest.size();
                }
                _ => {}
            };
        }

        Ok(Self {
            inputs,
            outputs,
            input_directory,
            output_paths,
            input_files_bytes,
        })
    }

    pub fn input_directory(&self) -> &ActionImmutableDirectory {
        &self.input_directory
    }

    pub fn output_paths(&self) -> &[(ProjectRelativePathBuf, OutputType)] {
        &self.output_paths
    }

    pub fn input_files_bytes(&self) -> u64 {
        self.input_files_bytes
    }
}

#[derive(Copy, Clone, Dupe, Debug, Display, Allocative, Hash, PartialEq, Eq)]
pub struct WorkerId(pub u64);

#[derive(Clone, Debug)]
pub struct WorkerSpec {
    pub id: WorkerId,
    pub exe: Vec<String>,
    pub concurrency: Option<usize>,
    pub remote_key: Option<TrackedFileDigest>,
}

/// The data contains the information about the command to be executed.
pub struct CommandExecutionRequest {
    /// Optional arguments including executable prepended to `args` to get full command line.
    /// This is used by workers to separate worker arguments from executable arguments.
    exe: Vec<String>,
    args: Vec<String>,
    paths: CommandExecutionPaths,
    env: SortedVectorMap<String, String>,
    timeout: Option<Duration>,
    pub executor_preference: ExecutorPreference,
    host_sharing_requirements: Arc<HostSharingRequirements>,
    // Used to disable the low pass filter for concurrent local actions. Enabled by default
    low_pass_filter: bool,
    /// Working directory, relative to the project root.
    working_directory: ProjectRelativePathBuf,
    /// Whether we should always prefetch stderr when executing. When it's needed, this lets us
    /// overlap stderr download with output downloads, which might be marginally useful to improve
    /// latency.
    prefetch_lossy_stderr: bool,
    /// Whether to cleanup outputs
    pub outputs_cleanup: bool,
    /// What environment variables to inherit from the Buck2 daemon.
    local_environment_inheritance: Option<EnvironmentInheritance>,
    /// Whether this command should override the fallback-only behavior on an hybrid executor and
    /// thus always run as if the executor was full-hybrid, assuming it is capable.
    force_full_hybrid_if_capable: bool,
    /// Whether to disable capturing performance counters for this execution.
    disable_miniperf: bool,
    required_local_resources: SortedSet<LocalResourceState>,
    /// Persistent worker to use for execution
    worker: Option<WorkerSpec>,
    /// Whether the executor should guarantee that the inodes for all inputs are unique (i.e. avoid
    /// hardlinking identical input files, for example)
    unique_input_inodes: bool,
    /// Remote dep file key, if the action has a dep file.
    /// If this key is set and remote dep file caching is enabled, it will be used to query the cache.
    pub remote_dep_file_key: Option<DepFileDigest>,
    /// RE dependencies to pass in action metadata.
    remote_execution_dependencies: Vec<RemoteExecutorDependency>,
    /// RE custom tupperware image.
    remote_execution_custom_image: Option<RemoteExecutorCustomImage>,
}

impl CommandExecutionRequest {
    pub fn new(
        exe: Vec<String>,
        args: Vec<String>,
        paths: CommandExecutionPaths,
        env: SortedVectorMap<String, String>,
    ) -> Self {
        Self {
            exe,
            args,
            paths,
            env,
            timeout: None,
            executor_preference: ExecutorPreference::Default,
            host_sharing_requirements: Arc::new(HostSharingRequirements::default()),
            low_pass_filter: true,
            working_directory: ProjectRelativePathBuf::default(),
            prefetch_lossy_stderr: false,
            outputs_cleanup: true,
            local_environment_inheritance: None,
            force_full_hybrid_if_capable: false,
            disable_miniperf: false,
            required_local_resources: SortedSet::new(),
            worker: None,
            unique_input_inodes: false,
            remote_dep_file_key: None,
            remote_execution_dependencies: Vec::new(),
            remote_execution_custom_image: None,
        }
    }

    pub fn paths(&self) -> &CommandExecutionPaths {
        &self.paths
    }

    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    pub fn with_executor_preference(mut self, executor_preference: ExecutorPreference) -> Self {
        self.executor_preference = executor_preference;
        self
    }

    pub fn with_host_sharing_requirements(
        mut self,
        host_sharing_requirements: Arc<HostSharingRequirements>,
    ) -> Self {
        self.host_sharing_requirements = host_sharing_requirements;
        self
    }

    pub fn with_low_pass_filter(mut self, low_pass_filter: bool) -> Self {
        self.low_pass_filter = low_pass_filter;
        self
    }

    pub fn with_working_directory(mut self, working_directory: ProjectRelativePathBuf) -> Self {
        self.working_directory = working_directory;
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

    pub fn with_remote_dep_file_key(mut self, remote_dep_file_key: &DepFileDigest) -> Self {
        self.remote_dep_file_key = Some(remote_dep_file_key.dupe());
        self
    }

    pub fn remote_dep_file_key(&self) -> &Option<DepFileDigest> {
        &self.remote_dep_file_key
    }

    pub fn prefetch_lossy_stderr(&self) -> bool {
        self.prefetch_lossy_stderr
    }

    pub fn outputs_cleanup(&self) -> bool {
        self.outputs_cleanup
    }

    pub fn all_args(&self) -> impl Iterator<Item = &String> {
        self.exe.iter().chain(self.args.iter())
    }

    pub fn all_args_vec(&self) -> Vec<String> {
        self.all_args().cloned().collect()
    }

    pub fn all_args_str(&self) -> String {
        self.all_args().join(" ")
    }

    pub fn exe(&self) -> &[String] {
        &self.exe
    }

    pub fn args(&self) -> &[String] {
        &self.args
    }

    pub fn worker(&self) -> &Option<WorkerSpec> {
        &self.worker
    }

    pub fn with_worker(mut self, worker: Option<WorkerSpec>) -> Self {
        self.worker = worker;
        self
    }

    pub fn inputs(&self) -> &[CommandExecutionInput] {
        &self.paths.inputs
    }

    pub fn outputs<'a>(&'a self) -> impl Iterator<Item = CommandExecutionOutputRef<'a>> + 'a {
        self.paths.outputs.iter().map(|output| output.as_ref())
    }

    pub fn env(&self) -> &SortedVectorMap<String, String> {
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

    pub fn low_pass_filter(&self) -> bool {
        self.low_pass_filter
    }

    pub fn working_directory(&self) -> &ProjectRelativePath {
        &self.working_directory
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

    pub fn with_force_full_hybrid_if_capable(mut self, force_full_hybrid_if_capable: bool) -> Self {
        self.force_full_hybrid_if_capable = force_full_hybrid_if_capable;
        self
    }

    pub fn force_full_hybrid_if_capable(&self) -> bool {
        self.force_full_hybrid_if_capable
    }

    pub fn with_disable_miniperf(mut self, disable_miniperf: bool) -> Self {
        self.disable_miniperf = disable_miniperf;
        self
    }

    pub fn disable_miniperf(&self) -> bool {
        self.disable_miniperf
    }

    pub fn with_required_local_resources(
        mut self,
        required_local_resources: Vec<LocalResourceState>,
    ) -> buck2_error::Result<Self> {
        let original_len = required_local_resources.len();
        self.required_local_resources = required_local_resources.into_iter().collect();
        if self.required_local_resources.len() != original_len {
            return Err(buck2_error!(
                [],
                "Each provided local resource state is supposed to come from a different target."
            ));
        }
        Ok(self)
    }

    pub fn required_local_resources(&self) -> &SortedSet<LocalResourceState> {
        &self.required_local_resources
    }

    pub fn with_unique_input_inodes(mut self, unique_input_inodes: bool) -> Self {
        self.unique_input_inodes = unique_input_inodes;
        self
    }

    pub fn unique_input_inodes(&self) -> bool {
        self.unique_input_inodes
    }

    pub fn with_remote_execution_dependencies(
        mut self,
        remote_execution_dependencies: Vec<RemoteExecutorDependency>,
    ) -> Self {
        self.remote_execution_dependencies = remote_execution_dependencies;
        self
    }

    pub fn remote_execution_dependencies(&self) -> &Vec<RemoteExecutorDependency> {
        &self.remote_execution_dependencies
    }

    pub fn with_remote_execution_custom_image(
        mut self,
        remote_execution_custom_image: Option<RemoteExecutorCustomImage>,
    ) -> Self {
        self.remote_execution_custom_image = remote_execution_custom_image;
        self
    }

    pub fn remote_execution_custom_image(&self) -> &Option<RemoteExecutorCustomImage> {
        &self.remote_execution_custom_image
    }
}

/// Is an output a file or a directory
#[derive(
    PartialEq, Eq, Hash, Debug, Copy, Clone, Dupe, Allocative, Ord, PartialOrd
)]
pub enum OutputType {
    /// We don't know - used to represent legacy code that doesn't yet declare the output type properly.
    /// We aim to mostly remove this alternative over time.
    FileOrDirectory,
    File,
    Directory,
}

#[derive(Debug, buck2_error::Error)]
enum OutputTypeError {
    #[error("Expected {1:?}, but `{0}` is already declared as {2:?}")]
    CheckPath(String, OutputType, OutputType),
}

impl OutputType {
    /// We are hoping for something of output type self, and got a path of an existing declared type.
    /// Raise an error if they are incompatible.
    pub fn check_path(
        self,
        path_for_error_message: impl Display,
        output_type: OutputType,
    ) -> buck2_error::Result<()> {
        if self == OutputType::Directory && output_type == OutputType::FileOrDirectory {
            // If we treat paths whose declared type is FileOrDirectory like files, then that's incompatible with directory
            soft_error!(
                "declare_wrong_type",
                OutputTypeError::CheckPath(
                    path_for_error_message.to_string(),
                    self,
                    OutputType::File
                )
                .into(),
                deprecation: true,
                quiet: true
            )?;
            Ok(())
        } else if self == output_type
            || self == OutputType::FileOrDirectory
            || output_type == OutputType::FileOrDirectory
        {
            Ok(())
        } else {
            Err(
                OutputTypeError::CheckPath(path_for_error_message.to_string(), self, output_type)
                    .into(),
            )
        }
    }
}

#[derive(UnpackVariants, PartialEq, Eq, Hash, Debug)]
pub enum CommandExecutionOutputRef<'a> {
    BuildArtifact {
        path: &'a BuildArtifactPath,
        output_type: OutputType,
    },
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
            Self::BuildArtifact { path, output_type } => ResolvedCommandExecutionOutput {
                path: fs.resolve_build(path),
                create: OutputCreationBehavior::Parent,
                output_type: *output_type,
            },
            Self::TestPath { path, create } => ResolvedCommandExecutionOutput {
                path: fs.buck_out_path_resolver().resolve_test(path),
                create: *create,
                output_type: OutputType::FileOrDirectory,
            },
        }
    }

    pub fn cloned(&self) -> CommandExecutionOutput {
        match self {
            Self::BuildArtifact { path, output_type } => CommandExecutionOutput::BuildArtifact {
                path: (*path).dupe(),
                output_type: *output_type,
            },
            Self::TestPath { path, create } => CommandExecutionOutput::TestPath {
                path: (*path).clone(),
                create: *create,
            },
        }
    }
}

#[derive(UnpackVariants, PartialEq, Eq, Hash, Debug)]
pub enum CommandExecutionOutput {
    BuildArtifact {
        path: BuildArtifactPath,
        output_type: OutputType,
    },
    TestPath {
        path: BuckOutTestPath,
        create: OutputCreationBehavior,
    },
}

impl CommandExecutionOutput {
    pub fn as_ref<'a>(&'a self) -> CommandExecutionOutputRef<'a> {
        match self {
            Self::BuildArtifact {
                ref path,
                output_type,
            } => CommandExecutionOutputRef::BuildArtifact {
                path,
                output_type: *output_type,
            },
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
    pub path: ProjectRelativePathBuf,
    pub output_type: OutputType,
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
