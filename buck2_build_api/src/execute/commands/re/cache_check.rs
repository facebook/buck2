/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{ops::ControlFlow, sync::Arc};

use async_trait::async_trait;
use gazebo::prelude::*;
use remote_execution as RE;
use tracing::info;

use crate::execute::{
    commands::{
        re::{
            client::ActionDigest, download::download_action_results,
            manager::ManagedRemoteExecutionClient, ActionPaths,
        },
        CommandExecutionManager, CommandExecutionRequest, CommandExecutionResult, ExecutorName,
        PreparedCommand, PreparedCommandExecutor,
    },
    materializer::Materializer,
};

/// A PreparedCommandExecutor that will check the action cache before executing any actions using the underlying executor.
pub struct CacheCheckingExecutor {
    pub inner: Arc<dyn PreparedCommandExecutor>,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
}

impl CacheCheckingExecutor {
    pub fn new(
        inner: Arc<dyn PreparedCommandExecutor>,
        materializer: Arc<dyn Materializer>,
        re_client: ManagedRemoteExecutionClient,
    ) -> Self {
        Self {
            inner,
            materializer,
            re_client,
        }
    }

    async fn try_action_cache_fetch(
        &self,
        mut manager: CommandExecutionManager,
        request: &CommandExecutionRequest,
        action_paths: &ActionPaths,
        action_digest: &ActionDigest,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let re_client = &self.re_client;
        let action_cache_response = manager
            .stage_async(
                buck2_data::CacheQuery {
                    action_digest: action_digest.to_string(),
                },
                re_client.action_cache(action_digest.dupe()),
            )
            .await;

        let response = match action_cache_response {
            Err(e) => return ControlFlow::Break(manager.error("remote_action_cache".into(), e)),
            Ok(Some(response)) => {
                // we were able to go to the action cache, so can skip uploading and running
                info!(
                    "Action result is cached, skipping execution of:\n```\n$ {}\n```\n for action `{}`",
                    request.args().join(" "),
                    action_digest,
                );
                response
            }
            Ok(None) => return ControlFlow::Continue(manager),
        };

        ControlFlow::Break(
            download_action_results(
                &*self.materializer,
                &self.re_client,
                manager,
                // TODO (torozco): We should deduplicate this and ActionExecutionKind.
                buck2_data::CacheHit {
                    action_digest: action_digest.to_string(),
                }
                .into(),
                action_paths,
                request.outputs(),
                action_digest,
                &response,
            )
            .await,
        )
    }
}

#[async_trait]
impl PreparedCommandExecutor for CacheCheckingExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let manager = self
            .try_action_cache_fetch(
                manager,
                command.request,
                &command.action_paths,
                &command.prepared_action.action,
            )
            .await?;

        self.inner.exec_cmd(command, manager).await
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        self.inner.re_platform()
    }

    fn name(&self) -> ExecutorName {
        self.inner.name()
    }
}
