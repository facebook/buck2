/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::ControlFlow;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::execution_types::executor_config::CommandGenerationOptions;
use buck2_core::execution_types::executor_config::OutputPathsBehavior;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_futures::cancellation::CancellationContext;
use dupe::Dupe;
use remote_execution as RE;
use sorted_vector_map::SortedVectorMap;

use super::cache_uploader::CacheUploadResult;
use crate::artifact::fs::ExecutorFs;
use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use crate::execute::action_digest_and_blobs::ActionDigestAndBlobsBuilder;
use crate::execute::cache_uploader::CacheUploadInfo;
use crate::execute::cache_uploader::DepFileEntry;
use crate::execute::cache_uploader::UploadCache;
use crate::execute::executor_stage;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::paths_with_digest::PathsWithDigestBlobData;
use crate::execute::prepared::PreparedAction;
use crate::execute::prepared::PreparedCommand;
use crate::execute::prepared::PreparedCommandExecutor;
use crate::execute::prepared::PreparedCommandOptionalExecutor;
use crate::execute::request::CommandExecutionInput;
use crate::execute::request::CommandExecutionRequest;
use crate::execute::request::ExecutorPreference;
use crate::execute::request::OutputType;
use crate::execute::result::CommandExecutionMetadata;
use crate::execute::result::CommandExecutionResult;

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

impl From<CommandExecutionMetadata> for ActionExecutionTimingData {
    fn from(command: CommandExecutionMetadata) -> Self {
        Self {
            wall_time: command.wall_time,
        }
    }
}

#[derive(Clone, Dupe)]
pub struct CommandExecutor(Arc<CommandExecutorData>);

struct CommandExecutorData {
    inner: Arc<dyn PreparedCommandExecutor>,
    cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
    artifact_fs: ArtifactFs,
    options: CommandGenerationOptions,
    re_platform: RE::Platform,
    cache_uploader: Arc<dyn UploadCache>,
}

impl CommandExecutor {
    pub fn new(
        inner: Arc<dyn PreparedCommandExecutor>,
        cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
        cache_uploader: Arc<dyn UploadCache>,
        artifact_fs: ArtifactFs,
        options: CommandGenerationOptions,
        re_platform: RE::Platform,
    ) -> Self {
        Self(Arc::new(CommandExecutorData {
            inner,
            cache_checker,
            artifact_fs,
            options,
            re_platform,
            cache_uploader,
        }))
    }

    pub fn fs(&self) -> &ArtifactFs {
        &self.0.artifact_fs
    }

    pub fn executor_fs(&self) -> ExecutorFs {
        ExecutorFs::new(&self.0.artifact_fs, self.0.options.path_separator)
    }

    pub fn re_platform(&self) -> &RE::Platform {
        &self.0.re_platform
    }

    /// Check if the action can be served by the action cache.
    pub async fn action_cache(
        &self,
        manager: CommandExecutionManager,
        prepared_command: &PreparedCommand<'_, '_>,
        cancellations: &CancellationContext<'_>,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        self.0
            .cache_checker
            .maybe_execute(prepared_command, manager, cancellations)
            .await
    }

    pub async fn cache_upload(
        &self,
        info: &CacheUploadInfo<'_>,
        execution_result: &CommandExecutionResult,
        dep_file_entry: Option<DepFileEntry>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
    ) -> anyhow::Result<CacheUploadResult> {
        self.0
            .cache_uploader
            .upload(
                info,
                execution_result,
                dep_file_entry,
                action_digest_and_blobs,
            )
            .await
    }

    /// Execute a command.
    ///
    /// This intentionally does not return a Result since we want to capture information about the
    /// execution even if there are errors. Any errors can be propagated by converting them
    /// to a result with CommandExecutionManager::error.
    pub async fn exec_cmd(
        &self,
        manager: CommandExecutionManager,
        prepared_command: &PreparedCommand<'_, '_>,
        cancellations: &CancellationContext<'_>,
    ) -> CommandExecutionResult {
        self.0
            .inner
            .exec_cmd(prepared_command, manager, cancellations)
            .await
    }

    pub fn is_local_execution_possible(&self, executor_preference: ExecutorPreference) -> bool {
        self.0
            .inner
            .is_local_execution_possible(executor_preference)
    }

    pub fn prepare_action(
        &self,
        request: &CommandExecutionRequest,
        digest_config: DigestConfig,
    ) -> anyhow::Result<PreparedAction> {
        executor_stage(buck2_data::PrepareAction {}, || {
            let input_digest = request.paths().input_directory().fingerprint();

            let action_metadata_blobs = request.inputs().iter().filter_map(|x| match x {
                CommandExecutionInput::Artifact(_) => None,
                CommandExecutionInput::ActionMetadata(metadata) => {
                    Some((metadata.data.clone(), metadata.digest.dupe()))
                }
                CommandExecutionInput::ScratchPath(_) => None,
            });
            let action = re_create_action(
                request.all_args_vec(),
                request.paths().output_paths(),
                request.working_directory().map(|p| p.as_str().to_owned()),
                request.env(),
                input_digest,
                action_metadata_blobs,
                request.timeout(),
                self.0.re_platform.clone(),
                false,
                digest_config,
                self.0.options.output_paths_behavior,
                request.unique_input_inodes(),
                request.remote_execution_dependencies(),
            )?;

            anyhow::Ok(action)
        })
    }
}

fn re_create_action(
    args: Vec<String>,
    outputs: &[(ProjectRelativePathBuf, OutputType)],
    workdir: Option<String>,
    environment: &SortedVectorMap<String, String>,
    input_digest: &TrackedFileDigest,
    blobs: impl IntoIterator<Item = (PathsWithDigestBlobData, TrackedFileDigest)>,
    timeout: Option<Duration>,
    platform: RE::Platform,
    do_not_cache: bool,
    digest_config: DigestConfig,
    output_paths_behavior: OutputPathsBehavior,
    unique_input_inodes: bool,
    remote_execution_dependencies: &Vec<RemoteExecutorDependency>,
) -> anyhow::Result<PreparedAction> {
    let mut command = RE::Command {
        arguments: args,
        platform: Some(platform),
        working_directory: workdir.unwrap_or_default(),
        environment_variables: environment
            .iter()
            .map(|(k, v)| RE::EnvironmentVariable {
                name: (*k).clone(),
                value: (*v).clone(),
            })
            .collect(),
        ..Default::default()
    };

    match output_paths_behavior {
        OutputPathsBehavior::Compatibility => {
            for (output, output_type) in outputs {
                let path = output.as_str().to_owned();

                match output_type {
                    OutputType::FileOrDirectory => {
                        command.output_files.push(path.clone());
                        command.output_directories.push(path);
                    }
                    OutputType::File => command.output_files.push(path),
                    OutputType::Directory => command.output_directories.push(path),
                }
            }
        }
        OutputPathsBehavior::Strict => {
            for (output, output_type) in outputs {
                let path = output.as_str().to_owned();

                match output_type {
                    OutputType::FileOrDirectory => {
                        command.output_files.push(path);
                    }
                    OutputType::File => command.output_files.push(path),
                    OutputType::Directory => command.output_directories.push(path),
                }
            }
        }
        OutputPathsBehavior::OutputPaths => {
            #[cfg(fbcode_build)]
            {
                return Err(anyhow::anyhow!(
                    "output_paths is not supported in fbcode_build"
                ));
            }

            #[cfg(not(fbcode_build))]
            {
                for (output, _output_type) in outputs {
                    command.output_paths.push(output.as_str().to_owned());
                }
            }
        }
    }

    let mut action_and_blobs = ActionDigestAndBlobsBuilder::new(digest_config);

    for (data, digest) in blobs {
        action_and_blobs.add_paths(digest, data);
    }

    // Required by the RE spec to be sorted:
    // https://github.com/bazelbuild/remote-apis/blob/1f36c310b28d762b258ea577ed08e8203274efae/build/bazel/remote/execution/v2/remote_execution.proto#L589
    command.output_directories.sort();
    command.output_files.sort();
    #[cfg(not(fbcode_build))]
    {
        command.output_paths.sort();
        command.output_node_properties.sort();
    }
    command
        .environment_variables
        .sort_by(|e1, e2| e1.name.cmp(&e2.name));

    let mut action = RE::Action {
        input_root_digest: Some(input_digest.to_grpc()),
        command_digest: Some(action_and_blobs.add_command(&command).to_grpc()),
        timeout: timeout
            .map(|t| t.try_into())
            .transpose()
            .context("Cannot convert timeout to GRPC")?,
        do_not_cache,
        ..Default::default()
    };

    if unique_input_inodes {
        #[cfg(fbcode_build)]
        {
            action.copy_policy_resolver = RE::CopyPolicyResolver::SingleHardLinking.into();
        }
    }

    #[cfg(fbcode_build)]
    {
        action.respect_exec_bit = true;
    }

    #[cfg(not(fbcode_build))]
    {
        let _unused = &mut action;
    }

    let action_and_blobs = action_and_blobs.build(&action);

    Ok(PreparedAction {
        action_and_blobs,
        platform: command
            .platform
            .expect("We did put a platform a few lines up"),
        remote_execution_dependencies: remote_execution_dependencies.to_owned(),
    })
}
