/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::time::Duration;

use allocative::Allocative;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::local_resource_state::LocalResourceState;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPath;
use buck2_core::fs::buck_out_path::BuckOutScratchPath;
use buck2_core::fs::buck_out_path::BuckOutTestPath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::soft_error;
use buck2_util::collections::sorted_set::SortedSet;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use host_sharing::host_sharing::HostSharingRequirements;
use indexmap::IndexSet;
use itertools::Itertools;
use sorted_vector_map::SortedVectorMap;
use thiserror::Error;

use crate::artifact::group::artifact_group_values_dyn::ArtifactGroupValuesDyn;
use crate::digest_config::DigestConfig;
use crate::directory::insert_entry;
use crate::directory::ActionDirectoryMember;
use crate::directory::ActionImmutableDirectory;
use crate::execute::environment_inheritance::EnvironmentInheritance;
use crate::execute::inputs_directory::inputs_directory;

#[derive(Clone)]
pub struct ActionMetadataBlob {
    pub data: Vec<u8>,
    pub digest: TrackedFileDigest,
    pub path: BuckOutPath,
}

pub enum CommandExecutionInput {
    Artifact(Box<dyn ArtifactGroupValuesDyn>),
    ActionMetadata(ActionMetadataBlob),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Dupe, Hash)]
pub enum OutputCreationBehavior {
    Create,
    Parent,
}

#[derive(Debug, Error)]
#[error("Incompatible executor preferences: `{}` & `{}`", a, b)]
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
}

impl ExecutorPreference {
    pub fn and(self, other: Self) -> anyhow::Result<Self> {
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
        }
    }

    pub fn requires_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => false,
            Self::RemoteRequired => false,
            Self::RemotePreferred => false,
            Self::Default => false,
        }
    }

    pub fn prefers_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => true,
            Self::RemoteRequired => false,
            Self::RemotePreferred => false,
            Self::Default => false,
        }
    }

    pub fn prefers_remote(&self) -> bool {
        match self {
            Self::LocalRequired => false,
            Self::LocalPreferred => false,
            Self::RemoteRequired => true,
            Self::RemotePreferred => true,
            Self::Default => false,
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
    ) -> anyhow::Result<Self> {
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
            .collect::<anyhow::Result<Vec<_>>>()?;

        insert_entry(
            &mut builder,
            ProjectRelativePath::unchecked_new(".buckconfig"),
            DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata::empty(
                digest_config.cas_digest_config(),
            ))),
        )?;

        let input_directory = builder.fingerprint(digest_config.as_directory_serializer());

        let mut input_files_bytes = 0;

        for entry in input_directory
            .fingerprinted_unordered_walk()
            .without_paths()
        {
            match entry {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
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

pub struct WorkerSpec {
    pub id: WorkerId,
    pub exe: Vec<String>,
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
    executor_preference: ExecutorPreference,
    // Run with a custom $TMPDIR, or just the standard system one
    custom_tmpdir: Option<BuckOutScratchPath>,
    host_sharing_requirements: HostSharingRequirements,
    /// Working directory, relative to the project root.
    working_directory: Option<ProjectRelativePathBuf>,
    /// Whether we should always prefetch stderr when executing. When it's needed, this lets us
    /// overlap stderr download with output downloads, which might be marginally useful to improve
    /// latency.
    prefetch_lossy_stderr: bool,
    /// Whether to cleanup outputs
    pub outputs_cleanup: bool,
    /// What environment variables to inherit from the Buck2 daemon.
    local_environment_inheritance: Option<EnvironmentInheritance>,
    /// Whether this command should be uploaded to cache when successful.
    allow_cache_upload: bool,
    /// Whether this command should override the fallback-only behavior on an hybrid executor and
    /// thus always run as if the executor was full-hybrid, assuming it is capable.
    force_full_hybrid_if_capable: bool,
    /// Whether to disable capturing performance counters for this execution.
    disable_miniperf: bool,
    required_local_resources: SortedSet<LocalResourceState>,
    /// Persistent worker to use for execution
    worker: Option<WorkerSpec>,
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
            custom_tmpdir: None,
            host_sharing_requirements: HostSharingRequirements::default(),
            working_directory: None,
            prefetch_lossy_stderr: false,
            outputs_cleanup: true,
            local_environment_inheritance: None,
            allow_cache_upload: false,
            force_full_hybrid_if_capable: false,
            disable_miniperf: false,
            required_local_resources: SortedSet::new(),
            worker: None,
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

    pub fn custom_tmpdir(&self) -> &Option<BuckOutScratchPath> {
        &self.custom_tmpdir
    }

    pub fn with_custom_tmpdir(mut self, custom_tmpdir: BuckOutScratchPath) -> Self {
        self.custom_tmpdir = Some(custom_tmpdir);
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

    pub fn with_allow_cache_upload(mut self, allow_cache_upload: bool) -> Self {
        self.allow_cache_upload = allow_cache_upload;
        self
    }

    pub fn allow_cache_upload(&self) -> bool {
        self.allow_cache_upload
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
    ) -> anyhow::Result<Self> {
        let original_len = required_local_resources.len();
        self.required_local_resources = required_local_resources.into_iter().collect();
        if self.required_local_resources.len() != original_len {
            return Err(anyhow::anyhow!(
                "Each provided local resource state is supposed to come from a different target."
            ));
        }
        Ok(self)
    }

    pub fn required_local_resources(&self) -> &SortedSet<LocalResourceState> {
        &self.required_local_resources
    }
}

/// Is an output a file or a directory
#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Dupe, Allocative)]
pub enum OutputType {
    /// We don't know - used to represent legacy code that doesn't yet declare the output type properly.
    /// We aim to mostly remove this alternative over time.
    FileOrDirectory,
    File,
    Directory,
}

#[derive(Debug, Error)]
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
    ) -> anyhow::Result<()> {
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
        path: &'a BuckOutPath,
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
        path: BuckOutPath,
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
