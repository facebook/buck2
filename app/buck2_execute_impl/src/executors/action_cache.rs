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
use buck2_action_metadata_proto::RemoteDepFile;
use buck2_action_metadata_proto::REMOTE_DEP_FILE_KEY;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::action_digest::ActionDigestKind;
use buck2_execute::execute::dep_file_digest::DepFileDigest;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::kind::CommandExecutionKind;
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
use buck2_execute::re::remote_action_result::ActionCacheResult;
use buck2_futures::cancellation::CancellationContext;
use dupe::Dupe;
use prost::Message;

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
    pub remote_dep_file_checker: Arc<dyn PreparedCommandOptionalExecutor>,
}

enum CacheType {
    ActionCache,
    RemoteDepFileCache(DepFileDigest),
}

impl CacheType {
    fn to_proto(&self) -> buck2_data::CacheType {
        match self {
            CacheType::ActionCache => buck2_data::CacheType::ActionCache,
            CacheType::RemoteDepFileCache(_) => buck2_data::CacheType::RemoteDepFileCache,
        }
    }
}

async fn query_action_cache_and_download_result(
    // Differentiate between regular action cache look up and remote dep file based look up
    cache_type: CacheType,
    artifact_fs: &ArtifactFs,
    materializer: &Arc<dyn Materializer>,
    re_client: &ManagedRemoteExecutionClient,
    re_use_case: RemoteExecutorUseCase,
    re_action_key: &Option<String>,
    paranoid: &Option<ParanoidDownloader>,
    action_digest: &ActionDigest,
    command: &PreparedCommand<'_, '_>,
    manager: CommandExecutionManager,
    cancellations: &CancellationContext,
    upload_all_actions: bool,
    log_action_keys: bool,
    details: RemoteCommandExecutionDetails,
) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
    let request = command.request;
    let action_blobs = &command.prepared_action.action_and_blobs.blobs;
    let digest_config = command.digest_config;

    let digest = match &cache_type {
        CacheType::RemoteDepFileCache(key) => key.dupe().coerce::<ActionDigestKind>(),
        CacheType::ActionCache => action_digest.dupe(),
    };

    let action_cache_response = executor_stage_async(
        buck2_data::CacheQuery {
            action_digest: digest.to_string(),
            cache_type: cache_type.to_proto().into(),
        },
        re_client.action_cache(digest.dupe(), re_use_case),
    )
    .await;

    let identity = None; // TODO(#503): implement this
    if upload_all_actions {
        match re_client
            .upload(
                artifact_fs.fs(),
                materializer,
                action_blobs,
                ProjectRelativePath::empty(),
                request.paths().input_directory(),
                re_use_case,
                identity,
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
        Ok(Some(response)) => response,
        Ok(None) => return ControlFlow::Continue(manager),
    };

    let action_exit_code = response.action_result.exit_code;

    let dep_file_metadata: Option<RemoteDepFile> = match &cache_type {
        CacheType::ActionCache => None,
        CacheType::RemoteDepFileCache(_) => {
            let metadata = response
                .action_result
                .execution_metadata
                .auxiliary_metadata
                .iter()
                .find(|k| k.type_url == REMOTE_DEP_FILE_KEY);

            if metadata.is_none() {
                // No entry found
                return ControlFlow::Continue(manager);
            }
            let dep_file_entry = match RemoteDepFile::decode(metadata.unwrap().value.as_slice()) {
                Ok(entry) => entry,
                Err(e) => {
                    return ControlFlow::Break(manager.error("remote_dep_file", e));
                }
            };
            Some(dep_file_entry)
        }
    };

    let identity = ReActionIdentity::new(
        command.target,
        re_action_key.as_deref(),
        command.request.paths(),
    );

    let response = ActionCacheResult(response, cache_type.to_proto());
    let res = download_action_results(
        request,
        materializer.as_ref(),
        re_client,
        re_use_case,
        digest_config,
        manager,
        &identity,
        buck2_data::CacheHit {
            action_digest: digest.to_string(),
            action_key: if log_action_keys {
                Some(identity.action_key.clone())
            } else {
                None
            },
            cache_type: cache_type.to_proto().into(),
        }
        .into(),
        request.paths(),
        request.outputs(),
        details,
        &response,
        paranoid.as_ref(),
        cancellations,
        action_exit_code,
        artifact_fs,
        false,
        false,
        None,
    )
    .await;

    let DownloadResult::Result(mut res) = res;
    match &cache_type {
        CacheType::RemoteDepFileCache(key) => {
            tracing::trace!(
                "Found an action result for remote dep file key`{}`, moving onto dep file verification",
                key,
            );
            res.dep_file_metadata = dep_file_metadata;
        }
        CacheType::ActionCache => {
            tracing::info!(
                "Action result is cached, skipping execution of:\n```\n$ {}\n```\n for action `{}`",
                command.request.all_args_str(),
                action_digest,
            );
            res.action_result = Some(response.0.action_result);
        }
    }

    ControlFlow::Break(res)
}

#[async_trait]
impl PreparedCommandOptionalExecutor for ActionCacheChecker {
    async fn maybe_execute(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let action_digest = &command.prepared_action.action_and_blobs.action;
        let details = RemoteCommandExecutionDetails::new(
            action_digest.dupe(),
            *command.request.remote_dep_file_key(),
            self.re_client.get_session_id().await.ok(),
            self.re_use_case,
            &command.prepared_action.platform,
        );
        let cache_type = CacheType::ActionCache;
        let manager = manager.with_execution_kind(command_execution_kind_for_cache_type(
            &cache_type,
            details.clone(),
        ));
        let result = query_action_cache_and_download_result(
            cache_type,
            &self.artifact_fs,
            &self.materializer,
            &self.re_client,
            self.re_use_case,
            &self.re_action_key,
            &self.paranoid,
            action_digest,
            command,
            manager,
            cancellations,
            self.upload_all_actions,
            self.knobs.log_action_keys,
            details,
        )
        .await;

        // If continue (not a cache hit), invoke the remote dep file cache checker
        match result {
            ControlFlow::Continue(manager) => {
                self.remote_dep_file_checker
                    .maybe_execute(command, manager, cancellations)
                    .await
            }
            ControlFlow::Break(result) => ControlFlow::Break(result),
        }
    }
}

pub struct RemoteDepFileCacheChecker {
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
impl PreparedCommandOptionalExecutor for RemoteDepFileCacheChecker {
    async fn maybe_execute(
        &self,
        command: &PreparedCommand<'_, '_>,
        manager: CommandExecutionManager,
        cancellations: &CancellationContext,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        // If the remote dep file key is not set, just fallback to the next execution method
        let remote_dep_file_key = match command.request.remote_dep_file_key() {
            None => {
                return ControlFlow::Continue(manager);
            }
            Some(key) => key.dupe(),
        };

        let cache_type = CacheType::RemoteDepFileCache(remote_dep_file_key);
        let action_digest = remote_dep_file_key.dupe().coerce::<ActionDigestKind>();
        let details = RemoteCommandExecutionDetails::new(
            action_digest.dupe(),
            Some(remote_dep_file_key.dupe()),
            self.re_client.get_session_id().await.ok(),
            self.re_use_case,
            &command.prepared_action.platform,
        );
        let manager = manager.with_execution_kind(command_execution_kind_for_cache_type(
            &cache_type,
            details.clone(),
        ));

        query_action_cache_and_download_result(
            cache_type,
            &self.artifact_fs,
            &self.materializer,
            &self.re_client,
            self.re_use_case,
            &self.re_action_key,
            &self.paranoid,
            &action_digest,
            command,
            manager,
            cancellations,
            self.upload_all_actions,
            self.knobs.log_action_keys,
            details,
        )
        .await
    }
}

fn command_execution_kind_for_cache_type(
    cache_type: &CacheType,
    details: RemoteCommandExecutionDetails,
) -> CommandExecutionKind {
    match cache_type {
        CacheType::ActionCache => CommandExecutionKind::ActionCache { details },
        CacheType::RemoteDepFileCache(_) => CommandExecutionKind::RemoteDepFileCache { details },
    }
}
