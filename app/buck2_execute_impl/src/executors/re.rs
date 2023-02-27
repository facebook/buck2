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

use async_trait::async_trait;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::prepared::ActionPaths;
use buck2_execute::execute::prepared::PreparedAction;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::action_identity::ReActionIdentity;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_execute::re::remote_action_result::RemoteActionResult;
use dupe::Dupe;
use futures::FutureExt;
use indexmap::IndexMap;
use remote_execution as RE;
use remote_execution::ExecuteResponse;
use remote_execution::TCode;
use thiserror::Error;
use tracing::info;

use crate::re::download::download_action_results;

#[derive(Debug, Error)]
pub enum RemoteExecutorError {
    #[error("Trying to execute a `local_only = True` action on remote executor for {0}")]
    LocalOnlyAction(String),
}

pub struct ReExecutor {
    pub artifact_fs: ArtifactFs,
    pub project_fs: ProjectRoot,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub re_use_case: RemoteExecutorUseCase,
    pub re_action_key: Option<String>,
    pub re_max_input_files_bytes: u64,
    pub knobs: ExecutorGlobalKnobs,
    pub skip_cache_lookup: bool,
    pub re_max_queue_time_ms: Option<u64>,
}

impl ReExecutor {
    /// Indicate whether an action is too big to run on RE.
    pub fn is_action_too_large(&self, action_paths: &ActionPaths) -> bool {
        action_paths.input_files_bytes > self.re_max_input_files_bytes
    }

    async fn upload(
        &self,
        manager: CommandExecutionManager,
        blobs: &ActionBlobs,
        action_paths: &ActionPaths,
        digest_config: DigestConfig,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let re_client = &self.re_client;

        let upload_response = executor_stage_async(
            buck2_data::ReStage {
                stage: Some(buck2_data::ReUpload {}.into()),
            },
            re_client.upload(
                &self.materializer,
                blobs,
                ProjectRelativePath::empty(),
                &action_paths.inputs,
                self.re_use_case,
                digest_config,
            ),
        )
        .await;

        match upload_response {
            Ok(()) => {}
            Err(e) => return ControlFlow::Break(manager.error("remote_upload_error", e)),
        };

        ControlFlow::Continue(manager)
    }

    async fn re_execute(
        &self,
        mut manager: CommandExecutionManager,
        action: &CommandExecutionTarget<'_>,
        request: &CommandExecutionRequest,
        action_digest: &ActionDigest,
        action_paths: &ActionPaths,
        digest_config: DigestConfig,
        platform: &RE::Platform,
    ) -> ControlFlow<CommandExecutionResult, (CommandExecutionManager, ExecuteResponse)> {
        info!(
            "RE command line:\n```\n$ {}\n```\n for action `{}`",
            request.args().join(" "),
            action_digest,
        );

        let identity = ReActionIdentity::new(action, self.re_action_key.as_deref(), action_paths);

        let execute_response = self
            .re_client
            .execute(
                action_digest.dupe(),
                platform,
                self.re_use_case,
                &identity,
                &mut manager,
                self.skip_cache_lookup,
                self.re_max_queue_time_ms.map(Duration::from_millis),
            )
            .await;

        let response = match execute_response {
            Ok(result) => result,
            Err(e) => return ControlFlow::Break(manager.error("remote_exec_error", e)),
        };

        let action_result = &response.action_result;

        if response.error.code != TCode::OK {
            return ControlFlow::Break(manager.error(
                "remote_exec_error",
                ReErrorWrapper {
                    action_digest: action_digest.dupe(),
                    inner: response.error,
                },
            ));
        }
        if action_result.exit_code != 0 {
            return ControlFlow::Break(manager.failure(
                CommandExecutionKind::Remote {
                    digest: action_digest.dupe(),
                },
                // TODO: we want to expose RE outputs even when actions fail,
                //   this will allow tpx to correctly retrieve the output of
                //   failing tests running on RE. See D34344489 for context.
                IndexMap::new(),
                CommandStdStreams::Remote(response.std_streams(
                    &self.re_client,
                    self.re_use_case,
                    digest_config,
                )),
                Some(action_result.exit_code),
            ));
        }

        ControlFlow::Continue((manager, response))
    }
}

#[async_trait]
impl PreparedCommandExecutor for ReExecutor {
    async fn exec_cmd(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
    ) -> CommandExecutionResult {
        let PreparedCommand {
            request,
            target,
            action_paths,
            prepared_action:
                PreparedAction {
                    action: action_digest,
                    blobs,
                    platform,
                },
            digest_config,
        } = command;

        if command.request.executor_preference().requires_local() {
            let error = anyhow::anyhow!(RemoteExecutorError::LocalOnlyAction(target.to_string()));
            return ControlFlow::Break(manager.error("remote_prepare", error))?;
        }

        let manager = self
            .upload(manager, blobs, action_paths, *digest_config)
            .await?;

        let (manager, response) = self
            .re_execute(
                manager,
                target,
                request,
                action_digest,
                action_paths,
                *digest_config,
                platform,
            )
            .await?;

        download_action_results(
            request,
            &*self.materializer,
            &self.re_client,
            self.re_use_case,
            *digest_config,
            manager,
            buck2_data::ReStage {
                stage: Some(buck2_data::ReDownload {}.into()),
            }
            .into(),
            action_paths,
            request.outputs(),
            action_digest,
            &response,
        )
        .boxed()
        .await
    }
}

#[derive(Error, Debug)]
#[error(
    "action_digest={}, re_code={}, re_location={}, re_message={}",
    .action_digest,
    .inner.code,
    .inner.error_location,
    .inner.message
)]
pub struct ReErrorWrapper {
    action_digest: ActionDigest,
    inner: remote_execution::REError,
}
