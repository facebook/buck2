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
use std::ops::ControlFlow;
use std::sync::Arc;
use std::time::Duration;

use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use gazebo::coerce::coerce;
use gazebo::prelude::*;
use remote_execution as RE;

use crate::artifact::fs::ArtifactFs;
use crate::artifact::fs::ExecutorFs;
use crate::digest::CasDigestToReExt;
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

fn re_create_action(
    args: Vec<String>,
    output_files: Vec<String>,
    output_directories: Vec<String>,
    workdir: Option<String>,
    environment: &HashMap<String, String>,
    input_digest: &TrackedFileDigest,
    blobs: impl Iterator<Item = (Vec<u8>, TrackedFileDigest)>,
    timeout: Option<&Duration>,
    platform: Option<RE::Platform>,
    do_not_cache: bool,
) -> PreparedAction {
    // A rust HashMap is in an arbitrary order, so sort first to get a better cache hit rate
    let mut environment = environment.iter().collect::<Vec<_>>();
    environment.sort_by_key(|(k, _)| *k);

    let command = RE::Command {
        arguments: args,
        output_files,
        output_directories,
        platform,
        working_directory: workdir.unwrap_or_default(),
        environment_variables: environment.map(|(k, v)| RE::EnvironmentVariable {
            name: (*k).clone(),
            value: (*v).clone(),
        }),
    };

    let timeout = timeout.map(|t| prost_types::Duration {
        seconds: t.as_secs() as i64,
        nanos: t.subsec_nanos() as i32,
    });

    let mut prepared_blobs = ActionBlobs::new();
    for (data, digest) in blobs {
        prepared_blobs.add_blob(digest, data);
    }
    let action = RE::Action {
        input_root_digest: Some(input_digest.to_grpc()),
        command_digest: Some(prepared_blobs.add_protobuf_message(&command).to_grpc()),
        timeout,
        do_not_cache,
    };

    let action = prepared_blobs.add_protobuf_message(&action);
    PreparedAction {
        action: coerce(action.data().dupe()),
        blobs: prepared_blobs,
    }
}
