/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::ops::ControlFlow;
use std::sync::Arc;
use std::time::Duration;

use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::execution_types::executor_config::CommandGenerationOptions;
use buck2_core::execution_types::executor_config::OutputPathsBehavior;
use buck2_core::execution_types::executor_config::ReGangWorker;
use buck2_core::execution_types::executor_config::RemoteExecutorCafFbpkg;
use buck2_core::execution_types::executor_config::RemoteExecutorCustomImage;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::fingerprinted_directory::FingerprintedDirectory;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use remote_execution as RE;
use remote_execution::TActionResult2;
use sorted_vector_map::SortedVectorMap;

use super::cache_uploader::CacheUploadResult;
use crate::artifact::fs::ExecutorFs;
use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use crate::execute::action_digest_and_blobs::ActionDigestAndBlobsBuilder;
use crate::execute::cache_uploader::CacheUploadInfo;
use crate::execute::cache_uploader::IntoRemoteDepFile;
use crate::execute::cache_uploader::UploadCache;
use crate::execute::executor_stage;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::prepared::PreparedAction;
use crate::execute::prepared::PreparedCommand;
use crate::execute::prepared::PreparedCommandExecutor;
use crate::execute::prepared::PreparedCommandOptionalExecutor;
use crate::execute::request::CommandExecutionRequest;
use crate::execute::request::ExecutorPreference;
use crate::execute::request::OutputType;
use crate::execute::request::RemoteWorkerSpec;
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
            wall_time: command.time_span.duration(),
        }
    }
}

#[derive(Clone, Dupe)]
pub struct CommandExecutor(Arc<CommandExecutorData>);

struct CommandExecutorData {
    inner: Arc<dyn PreparedCommandExecutor>,
    action_cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
    remote_dep_file_cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
    artifact_fs: ArtifactFs,
    options: CommandGenerationOptions,
    re_platform: RE::Platform,
    cache_uploader: Arc<dyn UploadCache>,
}

impl CommandExecutor {
    pub fn new(
        inner: Arc<dyn PreparedCommandExecutor>,
        action_cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
        remote_dep_file_cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
        cache_uploader: Arc<dyn UploadCache>,
        artifact_fs: ArtifactFs,
        options: CommandGenerationOptions,
        re_platform: RE::Platform,
    ) -> Self {
        Self(Arc::new(CommandExecutorData {
            inner,
            action_cache_checker,
            remote_dep_file_cache_checker,
            artifact_fs,
            options,
            re_platform,
            cache_uploader,
        }))
    }

    pub fn fs(&self) -> &ArtifactFs {
        &self.0.artifact_fs
    }

    pub fn executor_fs(&self) -> ExecutorFs<'_> {
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
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        self.0
            .action_cache_checker
            .maybe_execute(prepared_command, manager, cancellations)
            .await
    }

    pub async fn remote_dep_file_cache(
        &self,
        manager: CommandExecutionManager,
        prepared_command: &PreparedCommand<'_, '_>,
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        self.0
            .remote_dep_file_cache_checker
            .maybe_execute(prepared_command, manager, cancellations)
            .await
    }

    pub async fn cache_upload(
        &self,
        info: &CacheUploadInfo<'_>,
        execution_result: &CommandExecutionResult,
        re_result: Option<TActionResult2>,
        dep_file_bundle: Option<&mut dyn IntoRemoteDepFile>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
    ) -> buck2_error::Result<CacheUploadResult> {
        self.0
            .cache_uploader
            .upload(
                info,
                execution_result,
                re_result,
                dep_file_bundle,
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
        cancellations: &CancellationContext,
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
        re_outputs_required: bool,
    ) -> buck2_error::Result<PreparedAction> {
        executor_stage(buck2_data::PrepareAction {}, || {
            let input_digest = request.paths().input_directory().fingerprint();

            let mut platform = self.0.re_platform.clone();
            let all_args = if self.0.options.use_bazel_protocol_remote_persistent_workers
                && let Some(worker) = request.worker()
                && let Some(key) = worker.remote_key.as_ref()
            {
                platform.properties.push(RE::Property {
                    name: "persistentWorkerKey".to_owned(),
                    value: key.to_string(),
                });
                // TODO[AH] Ideally, Buck2 could generate an argfile on the fly.
                for arg in request.args() {
                    if !(arg.starts_with("@")
                        || arg.starts_with("-flagfile")
                        || arg.starts_with("--flagfile"))
                    {
                        return Err(buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "Remote persistent worker arguments must be passed as `@argfile`, `-flagfile=argfile`, or `--flagfile=argfile`."
                        ));
                    }
                }
                worker
                    .exe
                    .iter()
                    .chain(request.args().iter())
                    .cloned()
                    .collect()
            } else {
                request.all_args_vec()
            };
            let action = re_create_action(
                request.args().to_vec(),
                all_args,
                request.paths().output_paths(),
                request.working_directory(),
                request.env(),
                input_digest,
                request.timeout(),
                platform,
                false,
                digest_config,
                self.0.options.output_paths_behavior,
                request.unique_input_inodes(),
                request.remote_execution_dependencies(),
                request.re_gang_workers(),
                request.remote_execution_custom_image(),
                &request
                    .meta_internal_extra_params()
                    .remote_execution_caf_fbpkgs,
                request.remote_worker(),
                re_outputs_required,
            )?;

            buck2_error::Ok(action)
        })
    }
}

fn re_create_action(
    args: Vec<String>,
    all_args: Vec<String>,
    outputs: &[(ProjectRelativePathBuf, OutputType)],
    working_directory: &ProjectRelativePath,
    environment: &SortedVectorMap<String, String>,
    input_digest: &TrackedFileDigest,
    timeout: Option<Duration>,
    platform: RE::Platform,
    do_not_cache: bool,
    digest_config: DigestConfig,
    output_paths_behavior: OutputPathsBehavior,
    unique_input_inodes: bool,
    remote_execution_dependencies: &Vec<RemoteExecutorDependency>,
    re_gang_workers: &Vec<ReGangWorker>,
    remote_execution_custom_image: &Option<RemoteExecutorCustomImage>,
    remote_execution_caf_fbpkgs: &[RemoteExecutorCafFbpkg],
    worker: &Option<RemoteWorkerSpec>,
    re_outputs_required: bool,
) -> buck2_error::Result<PreparedAction> {
    let (worker_tool_init_action, command_args) = if let Some(worker) = worker {
        let mut action_and_blobs = ActionDigestAndBlobsBuilder::new(digest_config);
        let command = RE::Command {
            arguments: worker.init.clone(),
            #[allow(deprecated)]
            platform: Some(platform.clone()),
            working_directory: working_directory.as_str().to_owned(),
            environment_variables: worker
                .env
                .iter()
                .map(|(k, v)| RE::EnvironmentVariable {
                    name: (*k).clone(),
                    value: (*v).clone(),
                })
                .collect(),
            ..Default::default()
        };
        let input_digest = worker.input_paths.input_directory().fingerprint();

        let action = RE::Action {
            input_root_digest: Some(input_digest.to_grpc()),
            command_digest: Some(action_and_blobs.add_command(&command).to_grpc()),
            timeout: timeout
                .map(|t| t.try_into())
                .transpose()
                .buck_error_context("Cannot convert timeout to GRPC")?,
            do_not_cache,
            ..Default::default()
        };
        let action_and_blobs = action_and_blobs.build(&action);
        (Some(action_and_blobs), args)
    } else {
        (None, all_args)
    };

    let mut command = RE::Command {
        arguments: command_args,
        #[allow(deprecated)]
        platform: Some(platform),
        working_directory: working_directory.as_str().to_owned(),
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

                #[allow(deprecated)]
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

                #[allow(deprecated)]
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
                return Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
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

    let mut action = RE::Action {
        input_root_digest: Some(input_digest.to_grpc()),
        command_digest: Some(action_and_blobs.add_command(&command).to_grpc()),
        timeout: timeout
            .map(|t| t.try_into())
            .transpose()
            .buck_error_context("Cannot convert timeout to GRPC")?,
        do_not_cache,
        #[cfg(fbcode_build)]
        worker_tool_action_digest: worker_tool_init_action.clone().map(|a| a.action.to_grpc()),
        ..Default::default()
    };

    #[cfg(fbcode_build)]
    if let Some(custom_image) = remote_execution_custom_image {
        action.caf_image_fbpkg = Some(RE::CafImageFbpkg {
            id: Some(RE::CafFbpkgIdentifier {
                name: custom_image.identifier.name.clone(),
                uuid: custom_image.identifier.uuid.clone(),
                ..Default::default()
            }),
            drop_host_mount_globs: custom_image.drop_host_mount_globs.clone(),
            ..Default::default()
        });
    }

    #[cfg(not(fbcode_build))]
    {
        let _unused = remote_execution_custom_image;
    }

    #[cfg(fbcode_build)]
    {
        action.caf_fbpkgs = remote_execution_caf_fbpkgs
            .iter()
            .map(|caf_fbpkg| RE::CafFbpkg {
                id: Some(RE::CafFbpkgIdentifier {
                    name: caf_fbpkg.name.clone(),
                    uuid: caf_fbpkg.uuid.clone(),
                    ..Default::default()
                }),
                ..Default::default()
            })
            .collect();
    }

    #[cfg(not(fbcode_build))]
    {
        let _unused = remote_execution_caf_fbpkgs;
    }

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

    #[cfg(fbcode_build)]
    {
        action.outputs_required = re_outputs_required;
    }

    #[cfg(not(fbcode_build))]
    {
        let _unused = &mut action;
        let _unused = re_outputs_required;
    }

    let action_and_blobs = action_and_blobs.build(&action);

    Ok(PreparedAction {
        action_and_blobs,
        #[allow(deprecated)]
        platform: command
            .platform
            .expect("We did put a platform a few lines up"),
        remote_execution_dependencies: remote_execution_dependencies.to_owned(),
        re_gang_workers: re_gang_workers.to_owned(),
        worker_tool_init_action,
    })
}
