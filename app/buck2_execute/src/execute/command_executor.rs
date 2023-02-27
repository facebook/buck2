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
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use dupe::Dupe;
use remote_execution as RE;
use sorted_vector_map::SortedVectorMap;

use crate::artifact::fs::ArtifactFs;
use crate::artifact::fs::ExecutorFs;
use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::directory::insert_entry;
use crate::directory::ActionDirectoryMember;
use crate::execute::blobs::ActionBlobs;
use crate::execute::inputs_directory::inputs_directory;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::manager::CommandExecutionManagerExt;
use crate::execute::prepared::ActionPaths;
use crate::execute::prepared::PreparedAction;
use crate::execute::prepared::PreparedCommand;
use crate::execute::prepared::PreparedCommandExecutor;
use crate::execute::request::CommandExecutionInput;
use crate::execute::request::CommandExecutionOutputRef;
use crate::execute::request::CommandExecutionRequest;
use crate::execute::request::OutputType;
use crate::execute::result::CommandExecutionResult;
use crate::execute::result::CommandExecutionTimingData;
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

impl From<CommandExecutionTimingData> for ActionExecutionTimingData {
    fn from(command: CommandExecutionTimingData) -> Self {
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
        action: CommandExecutionTarget<'_>,
        request: &CommandExecutionRequest,
        manager: CommandExecutionManager,
        digest_config: DigestConfig,
    ) -> CommandExecutionResult {
        let (manager, action_paths, prepared_action) =
            self.prepare(manager, request, digest_config).await?;
        self.0
            .inner
            .exec_cmd(
                &PreparedCommand {
                    target: action,
                    request,
                    action_paths,
                    prepared_action,
                    digest_config,
                },
                manager,
            )
            .await
    }

    async fn prepare(
        &self,
        mut manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        digest_config: DigestConfig,
    ) -> ControlFlow<CommandExecutionResult, (CommandExecutionManager, ActionPaths, PreparedAction)>
    {
        let (action_paths, action) = match manager.stage(buck2_data::PrepareAction {}, || {
            let action_paths = self.preamble(request.inputs(), request.outputs(), digest_config)?;
            let input_digest = action_paths.inputs.fingerprint();

            let action_metadata_blobs = request.inputs().iter().filter_map(|x| match x {
                CommandExecutionInput::Artifact(_) => None,
                CommandExecutionInput::ActionMetadata(metadata) => {
                    Some((metadata.data.clone(), metadata.digest.dupe()))
                }
            });
            let action = re_create_action(
                request.args().to_vec(),
                &action_paths.outputs,
                request.working_directory().map(|p| p.as_str().to_owned()),
                request.env(),
                input_digest,
                action_metadata_blobs,
                None,
                self.0.re_platform.clone(),
                false,
                digest_config,
                self.0.options.output_paths_behavior,
            )?;

            anyhow::Ok((action_paths, action))
        }) {
            Ok(v) => v,
            Err(e) => return ControlFlow::Break(manager.error("prepare", e)),
        };

        ControlFlow::Continue((manager, action_paths, action))
    }

    /// Return the inputs (in the form of a ActionImmutableDirectory) and the outputs for this
    /// action.
    fn preamble<'a>(
        &self,
        inputs: &[CommandExecutionInput],
        outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
        digest_config: DigestConfig,
    ) -> anyhow::Result<ActionPaths> {
        let mut builder = inputs_directory(inputs, &self.0.artifact_fs)?;

        let output_paths = outputs
            .map(|o| {
                let resolved = o.resolve(&self.0.artifact_fs);
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

        let input_dir = builder.fingerprint(digest_config.as_directory_serializer());

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

fn re_create_action(
    args: Vec<String>,
    outputs: &[(ProjectRelativePathBuf, OutputType)],
    workdir: Option<String>,
    environment: &SortedVectorMap<String, String>,
    input_digest: &TrackedFileDigest,
    blobs: impl Iterator<Item = (Vec<u8>, TrackedFileDigest)>,
    timeout: Option<&Duration>,
    platform: RE::Platform,
    do_not_cache: bool,
    digest_config: DigestConfig,
    output_paths_behavior: OutputPathsBehavior,
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

    let timeout = timeout.map(|t| prost_types::Duration {
        seconds: t.as_secs() as i64,
        nanos: t.subsec_nanos() as i32,
    });

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
        timeout,
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
