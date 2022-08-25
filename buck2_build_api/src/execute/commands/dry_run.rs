/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;
use std::sync::Mutex;

use async_trait::async_trait;
use buck2_node::execute::config::RemoteExecutorUseCase;
use gazebo::prelude::*;
use indexmap::IndexMap;
use remote_execution as RE;

use crate::actions::artifact::fs::ArtifactFs;
use crate::actions::artifact::Artifact;
use crate::actions::artifact::ArtifactValue;
use crate::execute::commands::CommandExecutionInput;
use crate::execute::commands::CommandExecutionKind;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::commands::CommandExecutionOutput;
use crate::execute::commands::CommandExecutionResult;
use crate::execute::commands::CommandExecutionTimingData;
use crate::execute::commands::ExecutorName;
use crate::execute::commands::PreparedCommand;
use crate::execute::commands::PreparedCommandExecutor;

#[derive(Debug, PartialEq, Eq)]
pub struct DryRunEntry {
    pub args: Vec<String>,
    pub inputs: HashSet<Artifact>,
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
        mut manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let PreparedCommand {
            request,
            target: _target,
            action_paths: _action_paths,
            prepared_action: _prepared_action,
        } = command;

        let claim = match manager.try_claim() {
            Some(claim) => claim,
            None => return manager.claim_rejected(),
        };

        let args = request.args().to_owned();
        let inputs = request
            .inputs()
            .iter()
            .flat_map(|i| match i {
                CommandExecutionInput::Artifact(group) => {
                    group.iter().map(|(artifact, _value)| artifact.dupe())
                }
                CommandExecutionInput::ActionMetadata(_) => {
                    unimplemented!("action metadata input not supported")
                }
            })
            .collect();
        let outputs = request.outputs().map(|o| o.cloned()).collect();
        let env = request.env().to_owned();

        self.tracker.lock().unwrap().push(DryRunEntry {
            args,
            inputs,
            outputs,
            env,
        });

        let exec_kind = CommandExecutionKind::Local {
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
                claim,
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

    fn name(&self) -> ExecutorName {
        ExecutorName("dry-run")
    }
}
