/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Display;
use std::time::Duration;

use allocative::Allocative;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use derive_more::Display;
use gazebo::dupe::Dupe;
use gazebo::variants::UnpackVariants;
use host_sharing::host_sharing::HostSharingRequirements;
use indexmap::IndexMap;
use thiserror::Error;

use crate::artifact::fs::ArtifactFs;
use crate::artifact::group::artifact_group_values_dyn::ArtifactGroupValuesDyn;
use crate::execute::environment_inheritance::EnvironmentInheritance;
use crate::path::buck_out_path::BuckOutPath;
use crate::path::buck_out_path::BuckOutTestPath;

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
}

impl ExecutorPreference {
    pub fn and(self, other: &Self) -> anyhow::Result<Self> {
        let requires_remote = self.requires_remote() || other.requires_remote();
        let requires_local = self.requires_local() || other.requires_local();

        if requires_remote && requires_local {
            return Err(IncompatibleExecutorPreferences { a: self, b: *other }.into());
        }

        if requires_local {
            return Ok(Self::LocalRequired);
        }

        if requires_remote {
            return Ok(Self::RemoteRequired);
        }

        if self.prefers_local() || other.prefers_local() {
            return Ok(Self::LocalPreferred);
        }

        Ok(Self::Default)
    }

    pub fn requires_remote(&self) -> bool {
        match self {
            Self::LocalRequired => false,
            Self::LocalPreferred => false,
            Self::RemoteRequired => true,
            Self::Default => false,
        }
    }

    pub fn requires_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => false,
            Self::RemoteRequired => false,
            Self::Default => false,
        }
    }

    pub fn prefers_local(&self) -> bool {
        match self {
            Self::LocalRequired => true,
            Self::LocalPreferred => true,
            Self::RemoteRequired => false,
            Self::Default => false,
        }
    }
}

/// The data contains the information about the command to be executed.
pub struct CommandExecutionRequest {
    args: Vec<String>,
    inputs: Vec<CommandExecutionInput>,
    artifact_outputs: IndexMap<BuckOutPath, OutputType>,
    test_outputs: Option<IndexMap<BuckOutTestPath, OutputCreationBehavior>>,
    env: HashMap<String, String>,
    timeout: Option<Duration>,
    executor_preference: ExecutorPreference,
    // Run with a custom $TMPDIR, or just the standard system one
    pub custom_tmpdir: bool,
    host_sharing_requirements: HostSharingRequirements,
    /// Working directory, relative to the project root.
    working_directory: Option<ProjectRelativePathBuf>,
    /// Whether we should always prefetch stderr when executing. When it's needed, this lets us
    /// overlap stderr download with output donwloads, which might be marginally useful to improve
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
}

impl CommandExecutionRequest {
    pub fn new(
        args: Vec<String>,
        inputs: Vec<CommandExecutionInput>,
        artifact_outputs: IndexMap<BuckOutPath, OutputType>,
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
            allow_cache_upload: false,
            force_full_hybrid_if_capable: false,
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
        let artifact_outputs = self.artifact_outputs.iter().map(|(path, output_type)| {
            CommandExecutionOutputRef::BuildArtifact {
                path,
                output_type: *output_type,
            }
        });

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
    #[error("Expected file, but `{0}` is a directory because of the trailing `/`")]
    ParsePathExpectedFile(String),
}

impl OutputType {
    /// We are hoping for something of output type self, and got a path of an existing declared type.
    /// Raise an error if they are incompatible.
    pub fn check_path(
        self,
        path_for_error_message: impl Display,
        output_type: OutputType,
    ) -> anyhow::Result<()> {
        if self == output_type
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

    /// We are hoping for something of output type self, and got a string.
    /// If it's incompatible, raise an error, otherwise return the inferred output type.
    pub fn parse_path(self, path: &str) -> anyhow::Result<(&str, OutputType)> {
        // FIXME: One day we should require that anyone requesting Directory has a trailing slash
        match path.strip_suffix('/') {
            None => Ok((path, self)),
            Some(_) if self == OutputType::File => {
                Err(OutputTypeError::ParsePathExpectedFile(path.to_owned()).into())
            }
            Some(path) => Ok((path, OutputType::Directory)),
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
