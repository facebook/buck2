/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Attaching command execution related data to Dice

use std::sync::Arc;

use buck2_common::executor_config::CommandExecutorConfig;
use buck2_core::fs::project::ProjectRoot;
use dice::data::DiceData;
use dice::DiceComputations;
use dice::UserComputationData;
use gazebo::prelude::*;

use crate::artifact::fs::ArtifactFs;
use crate::execute::prepared::PreparedCommandExecutor;
use crate::re::manager::ManagedRemoteExecutionClient;

pub trait SetCommandExecutor {
    fn set_command_executor(&mut self, init: Box<dyn HasCommandExecutor + Send + Sync + 'static>);
}

pub trait HasCommandExecutor {
    fn get_command_executor(
        &self,
        artifact_fs: &ArtifactFs,
        project_fs: &ProjectRoot,
        config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn PreparedCommandExecutor>>;
}

impl SetCommandExecutor for UserComputationData {
    fn set_command_executor(
        &mut self,
        delegate: Box<dyn HasCommandExecutor + Send + Sync + 'static>,
    ) {
        self.data.set(HasCommandExecutorHolder { delegate })
    }
}

impl HasCommandExecutor for DiceComputations {
    fn get_command_executor(
        &self,
        artifact_fs: &ArtifactFs,
        project_fs: &ProjectRoot,
        config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn PreparedCommandExecutor>> {
        let holder = self
            .per_transaction_data()
            .data
            .get::<HasCommandExecutorHolder>()
            .expect("CommandExecutorDelegate should be set");
        holder
            .delegate
            .get_command_executor(artifact_fs, project_fs, config)
    }
}

struct HasCommandExecutorHolder {
    delegate: Box<dyn HasCommandExecutor + Send + Sync + 'static>,
}

pub trait HasFallbackExecutorConfig {
    fn get_fallback_executor_config(&self) -> &CommandExecutorConfig;
}

impl HasFallbackExecutorConfig for DiceComputations {
    fn get_fallback_executor_config(&self) -> &CommandExecutorConfig {
        self.per_transaction_data()
            .data
            .get::<CommandExecutorConfig>()
            .expect("CommandExecutorConfig should be set")
    }
}

pub fn set_fallback_executor_config(data: &mut DiceData, config: CommandExecutorConfig) {
    data.set(config)
}

pub trait SetReClient {
    fn set_re_client(&mut self, re_client: ManagedRemoteExecutionClient);
}

pub trait GetReClient {
    fn get_re_client(&self) -> ManagedRemoteExecutionClient;
}

impl SetReClient for UserComputationData {
    fn set_re_client(&mut self, re_client: ManagedRemoteExecutionClient) {
        self.data.set(re_client);
    }
}

impl GetReClient for UserComputationData {
    fn get_re_client(&self) -> ManagedRemoteExecutionClient {
        self.data
            .get::<ManagedRemoteExecutionClient>()
            .expect("Materializer should be set")
            .dupe()
    }
}
