/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;
use std::sync::Mutex;

use async_trait::async_trait;
use buck2_common::executor_config::RemoteExecutorUseCase;
use indexmap::IndexMap;
use remote_execution as RE;

use crate::artifact::fs::ArtifactFs;
use crate::artifact_value::ArtifactValue;
use crate::execute::action_digest::ActionDigest;
use crate::execute::kind::CommandExecutionKind;
use crate::execute::manager::CommandExecutionManager;
use crate::execute::manager::CommandExecutionManagerExt;
use crate::execute::prepared::PreparedCommand;
use crate::execute::prepared::PreparedCommandExecutor;
use crate::execute::request::CommandExecutionOutput;
use crate::execute::result::CommandExecutionResult;
use crate::execute::result::CommandExecutionTimingData;

#[derive(Debug, PartialEq, Eq)]
pub struct DryRunEntry {
    pub args: Vec<String>,
    pub outputs: Vec<CommandExecutionOutput>,
    pub env: HashMap<String, String>,
}

/// Records executed commands into the provided tracker and returns a successful result for all commands.
/// If the filesystem is supplied, the dry run executor will write the executed command as contents
/// to the output file.
pub struct DryRunExecutor {
    tracker: Arc<Mutex<Vec<DryRunEntry>>>,
    fs: Option<ArtifactFs>,
}

impl DryRunExecutor {
    pub fn new(tracker: Arc<Mutex<Vec<DryRunEntry>>>, fs: Option<ArtifactFs>) -> Self {
        Self { tracker, fs }
    }
}

#[async_trait]
impl PreparedCommandExecutor for DryRunExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let PreparedCommand {
            request,
            target: _target,
            action_paths: _action_paths,
            prepared_action: _prepared_action,
        } = command;

        let manager = manager.claim().await;

        let args = request.args().to_owned();
        let outputs = request.outputs().map(|o| o.cloned()).collect();
        let env = request.env().to_owned();

        self.tracker
            .lock()
            .unwrap()
            .push(DryRunEntry { args, outputs, env });

        let exec_kind = CommandExecutionKind::Local {
            digest: ActionDigest::empty(),
            command: Default::default(),
            env: Default::default(),
        };

        match request
            .outputs()
            .map(|x| {
                if let Some(fs) = &self.fs {
                    let path = x.resolve(fs).into_path();
                    fs.fs().write_file(&path, "", false)?;
                }
                Ok((x.cloned(), ArtifactValue::empty_file()))
            })
            .collect::<anyhow::Result<_>>()
        {
            Ok(outputs) => manager.success(
                exec_kind,
                outputs,
                Default::default(),
                CommandExecutionTimingData::default(),
            ),
            // NOTE: This should probaby be an error() but who cares.
            Err(..) => manager.failure(exec_kind, IndexMap::new(), Default::default(), Some(1)),
        }
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        None
    }

    fn re_use_case(&self) -> RemoteExecutorUseCase {
        RemoteExecutorUseCase::buck2_default()
    }
}
