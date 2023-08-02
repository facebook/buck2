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

use async_trait::async_trait;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::kind::RemoteCommandExecutionDetails;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandOptionalExecutor;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::action_identity::ReActionIdentity;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use dupe::Dupe;
use more_futures::cancellation::CancellationContext;
use tracing::info;

use crate::re::download::download_action_results;
use crate::re::download::DownloadResult;
use crate::re::paranoid_download::ParanoidDownloader;

pub struct ActionCacheChecker {
    pub artifact_fs: ArtifactFs,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub re_use_case: RemoteExecutorUseCase,
    pub re_action_key: Option<String>,
    pub upload_all_actions: bool,
    pub knobs: ExecutorGlobalKnobs,
    pub paranoid: Option<ParanoidDownloader>,
}

#[async_trait]
impl PreparedCommandOptionalExecutor for ActionCacheChecker {
    async fn maybe_execute(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let request = command.request;
        let action_digest = &command.prepared_action.action;
        let platform = &command.prepared_action.platform;
        let action_blobs = &command.prepared_action.blobs;
        let digest_config = command.digest_config;
        let re_client = &self.re_client;

        let action_cache_response = executor_stage_async(
            buck2_data::CacheQuery {
                action_digest: action_digest.to_string(),
            },
            re_client.action_cache(action_digest.dupe(), self.re_use_case),
        )
        .await;

        if self.upload_all_actions {
            match re_client
                .upload(
                    self.artifact_fs.fs(),
                    &self.materializer,
                    action_blobs,
                    ProjectRelativePath::empty(),
                    request.paths().input_directory(),
                    self.re_use_case,
                    digest_config,
                )
                .await
            {
                Err(e) => {
                    return ControlFlow::Break(manager.error("upload", e));
                }
                Ok(_) => {}
            };
        }

        let response = match action_cache_response {
            Err(e) => {
                return ControlFlow::Break(manager.error("remote_action_cache", e));
            }
            Ok(Some(response)) => {
                // we were able to go to the action cache, so can skip uploading and running
                info!(
                    "Action result is cached, skipping execution of:\n```\n$ {}\n```\n for action `{}`",
                    request.all_args_str(),
                    action_digest,
                );
                response
            }
            Ok(None) => return ControlFlow::Continue(manager),
        };

        let action_key = if self.knobs.log_action_keys {
            let identity = ReActionIdentity::new(
                command.target,
                self.re_action_key.as_deref(),
                request.paths(),
            );
            Some(identity.action_key)
        } else {
            None
        };

        let res = download_action_results(
            request,
            &*self.materializer,
            &self.re_client,
            self.re_use_case,
            digest_config,
            manager,
            // TODO (torozco): We should deduplicate this and ActionExecutionKind.
            buck2_data::CacheHit {
                action_digest: action_digest.to_string(),
                action_key,
            }
            .into(),
            request.paths(),
            request.outputs(),
            RemoteCommandExecutionDetails {
                action_digest: action_digest.dupe(),
                session_id: self.re_client.get_session_id().await.ok(),
                use_case: self.re_use_case,
                platform: platform.clone(),
            },
            &response,
            self.paranoid.as_ref(),
            cancellations,
            response.action_result.exit_code,
        )
        .await;

        let DownloadResult::Result(res) = res;

        ControlFlow::Break(res)
    }
}
