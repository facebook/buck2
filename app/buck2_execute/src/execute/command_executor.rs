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

use buck2_common::executor_config::CommandGenerationOptions;
use buck2_common::executor_config::OutputPathsBehavior;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dupe::Dupe;
use more_futures::cancellation::CancellationContext;
use remote_execution as RE;
use sorted_vector_map::SortedVectorMap;

use crate::artifact::fs::ExecutorFs;
use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::execute::blobs::ActionBlobs;
use crate::execute::executor_stage_async;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::manager::CommandExecutionManagerExt;
use crate::execute::prepared::PreparedAction;
use crate::execute::prepared::PreparedCommand;
use crate::execute::prepared::PreparedCommandExecutor;
use crate::execute::request::CommandExecutionInput;
use crate::execute::request::CommandExecutionRequest;
use crate::execute::request::ExecutorPreference;
use crate::execute::request::OutputType;
use crate::execute::result::CommandExecutionMetadata;
use crate::execute::result::CommandExecutionResult;
use crate::execute::target::CommandExecutionTarget;

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
    artifact_fs: ArtifactFs,
    options: CommandGenerationOptions,
    re_platform: RE::Platform,
}

impl CommandExecutor {
    pub fn new(
        inner: Arc<dyn PreparedCommandExecutor>,
        artifact_fs: ArtifactFs,
        options: CommandGenerationOptions,
        re_platform: RE::Platform,
    ) -> Self {
        Self(Arc::new(CommandExecutorData {
            inner,
            artifact_fs,
            options,
            re_platform,
        }))
    }

    pub fn fs(&self) -> &ArtifactFs {
        &self.0.artifact_fs
    }

    pub fn executor_fs(&self) -> ExecutorFs {
        ExecutorFs::new(&self.0.artifact_fs, self.0.options.path_separator)
    }

    /// Execute a command.
    ///
    /// This intentionally does not return a Result since we want to capture information about the
    /// execution even if there are errors. Any errors can be propagated by converting them
    /// to a result with CommandExecutionManager::error.
    pub async fn exec_cmd(
        &self,
        action: &dyn CommandExecutionTarget,
        request: &CommandExecutionRequest,
        manager: CommandExecutionManager,
        digest_config: DigestConfig,
        cancellations: &CancellationContext,
    ) -> CommandExecutionResult {
        let (manager, prepared_action) = self.prepare(manager, request, digest_config).await?;
        self.0
            .inner
            .exec_cmd(
                &PreparedCommand {
                    target: action,
                    request,
                    prepared_action,
                    digest_config,
                },
                manager,
                cancellations,
            )
            .await
    }

    pub fn is_local_execution_possible(&self, executor_preference: ExecutorPreference) -> bool {
        self.0
            .inner
            .is_local_execution_possible(executor_preference)
    }

    async fn prepare(
        &self,
        manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        digest_config: DigestConfig,
    ) -> ControlFlow<CommandExecutionResult, (CommandExecutionManager, PreparedAction)> {
        let action = match executor_stage_async(buck2_data::PrepareAction {}, async {
            let input_digest = request.paths().input_directory().fingerprint();

            let action_metadata_blobs = request.inputs().iter().filter_map(|x| match x {
                CommandExecutionInput::Artifact(_) => None,
                CommandExecutionInput::ActionMetadata(metadata) => {
                    Some((metadata.data.clone(), metadata.digest.dupe()))
                }
            });
            let action = re_create_action(
                request.args().to_vec(),
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
            )?;

            anyhow::Ok(action)
        })
        .await
        {
            Ok(v) => v,
            Err(e) => return ControlFlow::Break(manager.error("prepare", e)),
        };

        ControlFlow::Continue((manager, action))
    }
}

fn re_create_action(
    args: Vec<String>,
    outputs: &[(ProjectRelativePathBuf, OutputType)],
    workdir: Option<String>,
    environment: &SortedVectorMap<String, String>,
    input_digest: &TrackedFileDigest,
    blobs: impl Iterator<Item = (Vec<u8>, TrackedFileDigest)>,
    timeout: Option<Duration>,
    platform: RE::Platform,
    do_not_cache: bool,
    digest_config: DigestConfig,
    output_paths_behavior: OutputPathsBehavior,
) -> anyhow::Result<PreparedAction> {
    let _ignored = timeout; // TODO (torozco): Fix me.

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

    let mut prepared_blobs = ActionBlobs::new(digest_config);
    for (data, digest) in blobs {
        prepared_blobs.add_blob(digest, data);
    }
    let action = RE::Action {
        input_root_digest: Some(input_digest.to_grpc()),
        command_digest: Some(
            prepared_blobs
                .add_protobuf_message(&command, digest_config)
                .to_grpc(),
        ),
        timeout: None,
        do_not_cache,
        ..Default::default()
    };

    let action = prepared_blobs.add_protobuf_message(&action, digest_config);
    Ok(PreparedAction {
        action: action.data().dupe().coerce(),
        blobs: prepared_blobs,
        platform: command
            .platform
            .expect("We did put a platform a few lines up"),
    })
}
