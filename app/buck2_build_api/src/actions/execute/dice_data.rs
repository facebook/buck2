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

use async_trait::async_trait;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
use buck2_execute::execute::cache_uploader::UploadCache;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::prepared::PreparedCommandOptionalExecutor;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use dice::DiceComputations;
use dice::DiceData;
use dice::DiceDataBuilder;
use dice::UserComputationData;
use dupe::Dupe;
use remote_execution as RE;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;

pub struct CommandExecutorResponse {
    pub executor: Arc<dyn PreparedCommandExecutor>,
    pub platform: RE::Platform,
    pub cache_checker: Arc<dyn PreparedCommandOptionalExecutor>,
    pub cache_uploader: Arc<dyn UploadCache>,
}

pub trait SetCommandExecutor {
    fn set_command_executor(&mut self, init: Box<dyn HasCommandExecutor + Send + Sync + 'static>);
}

pub trait HasCommandExecutor {
    fn get_command_executor(
        &self,
        artifact_fs: &ArtifactFs,
        config: &CommandExecutorConfig,
    ) -> buck2_error::Result<CommandExecutorResponse>;
}

impl SetCommandExecutor for UserComputationData {
    fn set_command_executor(
        &mut self,
        delegate: Box<dyn HasCommandExecutor + Send + Sync + 'static>,
    ) {
        self.data.set(HasCommandExecutorHolder { delegate })
    }
}

#[async_trait]
pub trait DiceHasCommandExecutor {
    async fn get_command_executor_from_dice(
        &mut self,
        config: &CommandExecutorConfig,
    ) -> buck2_error::Result<CommandExecutorResponse>;
}

#[async_trait]
impl DiceHasCommandExecutor for DiceComputations<'_> {
    async fn get_command_executor_from_dice(
        &mut self,
        config: &CommandExecutorConfig,
    ) -> buck2_error::Result<CommandExecutorResponse> {
        let artifact_fs = self.get_artifact_fs().await?;
        let holder = self
            .per_transaction_data()
            .data
            .get::<HasCommandExecutorHolder>()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
            .buck_error_context("CommandExecutorDelegate should be set")?;
        holder.delegate.get_command_executor(&artifact_fs, config)
    }
}

struct HasCommandExecutorHolder {
    delegate: Box<dyn HasCommandExecutor + Send + Sync + 'static>,
}

pub trait HasFallbackExecutorConfig {
    fn get_fallback_executor_config(&self) -> &Arc<CommandExecutorConfig>;
}

impl HasFallbackExecutorConfig for DiceComputations<'_> {
    fn get_fallback_executor_config(&self) -> &Arc<CommandExecutorConfig> {
        self.per_transaction_data()
            .data
            .get::<Arc<CommandExecutorConfig>>()
            .expect("CommandExecutorConfig should be set")
    }
}

pub fn set_fallback_executor_config(data: &mut DiceData, config: Arc<CommandExecutorConfig>) {
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

#[derive(Debug, Clone, Copy, Dupe)]
pub struct InvalidationTrackingConfig {
    pub enabled: bool,
}

pub trait SetInvalidationTrackingConfig {
    fn set_invalidation_tracking_config(&mut self, enabled: bool);
}

pub trait GetInvalidationTrackingConfig {
    fn get_invalidation_tracking_config(&self) -> InvalidationTrackingConfig;
}

impl SetInvalidationTrackingConfig for DiceDataBuilder {
    fn set_invalidation_tracking_config(&mut self, enabled: bool) {
        self.set(InvalidationTrackingConfig { enabled });
    }
}

impl GetInvalidationTrackingConfig for DiceComputations<'_> {
    fn get_invalidation_tracking_config(&self) -> InvalidationTrackingConfig {
        *self
            .global_data()
            .get::<InvalidationTrackingConfig>()
            .expect("InvalidationTrackingConfig should be set")
    }
}
