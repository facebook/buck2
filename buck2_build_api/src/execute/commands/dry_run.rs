/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::{HashMap, HashSet},
    sync::{Arc, Mutex},
};

use async_trait::async_trait;
use gazebo::prelude::*;
use indexmap::IndexMap;
use remote_execution as RE;

use crate::{
    actions::artifact::{Artifact, ArtifactFs, ArtifactValue},
    execute::{
        commands::{
            CommandExecutionInput, CommandExecutionManager, CommandExecutionOutput,
            CommandExecutionResult, CommandExecutionTimingData, ExecutorName, PreparedCommand,
            PreparedCommandExecutor,
        },
        ActionExecutionKind,
    },
};

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
                ActionExecutionKind::Simple,
                outputs,
                "".to_owned(),
                "".to_owned(),
                CommandExecutionTimingData::default(),
            ),
            // NOTE: This should probaby be an error() but who cares.
            Err(..) => manager.failure(
                ActionExecutionKind::Simple,
                IndexMap::new(),
                "".into(),
                "".into(),
                Some(1),
            ),
        }
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        None
    }

    fn name(&self) -> ExecutorName {
        ExecutorName("dry-run")
    }
}
