/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::borrow_deref_ref)] // FIXME?

use std::ops::ControlFlow;
use std::sync::Arc;

use buck2_common::file_ops::FileMetadata;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_execute::directory::insert_entry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::inputs_directory::inputs_directory;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::name::ExecutorName;
use buck2_execute::execute::prepared::ActionPaths;
use buck2_execute::execute::prepared::PreparedAction;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::CommandExecutionInput;
use buck2_execute::execute::request::CommandExecutionOutputRef;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionTimingData;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::re::client::re_create_action;
use buck2_node::execute::config::PathSeparatorKind;
use gazebo::prelude::*;

use crate::execute::ActionExecutionTimingData;

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
