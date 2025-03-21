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
use buck2_core::execution_types::executor_config::MetaInternalExtraParams;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_events::dispatch::span_async;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::kind::RemoteCommandExecutionDetails;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::prepared::PreparedAction;
use buck2_execute::execute::prepared::PreparedCommand;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::execute::result::CommandCancellationReason;
use buck2_execute::execute::result::CommandExecutionErrorType;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::action_identity::ReActionIdentity;
use buck2_execute::re::client::CancellationReason;
use buck2_execute::re::client::ExecuteResponseOrCancelled;
use buck2_execute::re::error::get_re_error_tag;
use buck2_execute::re::error::RemoteExecutionError;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_execute::re::remote_action_result::RemoteActionResult;
use buck2_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::FutureExt;
use indexmap::IndexMap;
use remote_execution as RE;
use remote_execution::ExecuteResponse;
use remote_execution::TCode;
use tracing::info;

use crate::re::download::download_action_results;
use crate::re::download::DownloadResult;
use crate::re::paranoid_download::ParanoidDownloader;
use crate::storage_resource_exhausted::is_storage_resource_exhausted;

#[derive(Debug, buck2_error::Error)]
pub enum RemoteExecutorError {
    #[error("Trying to execute a `local_only = True` action on remote executor")]
    #[buck2(input)]
    LocalOnlyAction,
}

pub struct ReExecutor {
    pub artifact_fs: ArtifactFs,
    pub project_fs: ProjectRoot,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub re_use_case: RemoteExecutorUseCase,
    pub re_action_key: Option<String>,
    pub knobs: ExecutorGlobalKnobs,
    pub skip_cache_read: bool,
    pub skip_cache_write: bool,
    pub re_max_queue_time_ms: Option<u64>,
    pub re_resource_units: Option<i64>,
    pub paranoid: Option<ParanoidDownloader>,
    pub materialize_failed_inputs: bool,
    pub materialize_failed_outputs: bool,
    pub dependencies: Vec<RemoteExecutorDependency>,
}

impl ReExecutor {
    async fn upload(
        &self,
        manager: CommandExecutionManager,
        identity: &ReActionIdentity<'_>,
        blobs: &ActionBlobs,
        paths: &CommandExecutionPaths,
        digest_config: DigestConfig,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let re_client = &self.re_client;

        let upload_response = span_async(buck2_data::ReUploadStart {}, async move {
            let res = re_client
                .upload(
                    &self.project_fs,
                    &self.materializer,
                    blobs,
                    ProjectRelativePath::empty(),
                    paths.input_directory(),
                    self.re_use_case,
                    Some(identity),
                    digest_config,
                )
                .await;
            match res {
                Ok(stats) => (
                    Ok(()),
                    buck2_data::ReUploadEnd {
                        digests_uploaded: Some(stats.total.digests_uploaded),
                        bytes_uploaded: Some(stats.total.bytes_uploaded),
                        stats_by_extension: stats.by_extension,
                    },
                ),
                Err(e) => (Err(e), buck2_data::ReUploadEnd::default()),
            }
        })
        .await;

        match upload_response {
            Ok(()) => {}
            Err(e) => {
                let e: buck2_error::Error = e.into();
                let is_storage_resource_exhausted = e
                    .find_typed_context::<RemoteExecutionError>()
                    .map_or(false, |re_client_error| {
                        is_storage_resource_exhausted(re_client_error.as_ref())
                    });
                let error_type = if is_storage_resource_exhausted {
                    CommandExecutionErrorType::StorageResourceExhausted
                } else {
                    CommandExecutionErrorType::Other
                };
                return ControlFlow::Break(manager.error_classified(
                    "remote_upload_error",
                    e,
                    error_type,
                ));
            }
        };

        ControlFlow::Continue(manager)
    }

    async fn re_execute<'a>(
        &self,
        mut manager: CommandExecutionManager,
        identity: &ReActionIdentity<'_>,
        request: &CommandExecutionRequest,
        action_digest: &ActionDigest,
        digest_config: DigestConfig,
        platform: &RE::Platform,
        dependencies: impl IntoIterator<Item = &'a RemoteExecutorDependency>,
        meta_internal_extra_params: &MetaInternalExtraParams,
    ) -> ControlFlow<CommandExecutionResult, (CommandExecutionManager, ExecuteResponse)> {
        info!(
            "RE command line:\n```\n$ {}\n```\n for action `{}`",
            request.all_args_str(),
            action_digest,
        );

        let execute_response = self
            .re_client
            .execute(
                action_digest.dupe(),
                platform,
                dependencies,
                self.re_use_case,
                &identity,
                &mut manager,
                self.skip_cache_read,
                self.skip_cache_write,
                self.re_max_queue_time_ms.map(Duration::from_millis),
                self.re_resource_units,
                &self.knobs,
                meta_internal_extra_params,
            )
            .await;

        let response = match execute_response {
            Ok(ExecuteResponseOrCancelled::Response(result)) => result,
            Ok(ExecuteResponseOrCancelled::Cancelled(cancelled)) => {
                let reason = cancelled.reason.map(|reason| match reason {
                    CancellationReason::NotSpecified => CommandCancellationReason::NotSpecified,
                    CancellationReason::ReQueueTimeout => CommandCancellationReason::ReQueueTimeout,
                });
                return ControlFlow::Break(manager.cancel(reason));
            }
            Err(e) => return ControlFlow::Break(manager.error("remote_call_error", e)),
        };

        let remote_details = RemoteCommandExecutionDetails::new(
            action_digest.dupe(),
            None,
            self.re_client.get_session_id().await.ok(),
            self.re_use_case,
            &platform,
        );

        let execution_kind = response.execution_kind(remote_details);
        let manager = manager.with_execution_kind(execution_kind.clone());
        let additional_message = if response.status.message.is_empty() {
            None
        } else {
            Some(response.status.message.clone())
        };

        if response.status.code != TCode::OK {
            let res = if let Some(out) = as_missing_outputs_error(&response.status) {
                // TODO: Add a dedicated report variant for this.
                // NOTE: We don't get stdout / stderr from RE when this happens, so the best we can
                // do here is just pass on the error.
                manager.failure(
                    execution_kind,
                    IndexMap::new(),
                    CommandStdStreams::Local {
                        stdout: Vec::new(),
                        stderr: out.to_owned().into(),
                    },
                    // We also don't get this output so don't put trash in here.
                    None,
                    Default::default(),
                    additional_message,
                )
            } else if is_timeout_error(&response.status) && request.timeout().is_some() {
                manager.timeout(
                    execution_kind,
                    // Checked above: we fallthrough to the error path if we didn't set a timeout
                    // and yet received one.
                    request.timeout().unwrap(),
                    CommandStdStreams::Remote(response.std_streams(
                        &self.re_client,
                        self.re_use_case,
                        digest_config,
                    )),
                    response.timing(),
                    additional_message,
                )
            } else {
                let error_type = if is_storage_resource_exhausted(&response.status) {
                    CommandExecutionErrorType::StorageResourceExhausted
                } else {
                    CommandExecutionErrorType::Other
                };
                manager.error_classified(
                    "remote_exec_error",
                    ReErrorWrapper {
                        action_digest: action_digest.dupe(),
                        inner: response.status,
                    },
                    error_type,
                )
            };

            return ControlFlow::Break(res);
        }

        if let Some(timeout) = request.timeout() {
            let execution_time = response.timing().execution_time;

            if execution_time > timeout {
                let res = soft_error!(
                    "re_timeout_exceeded",
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
                        "Command {} exceeded its timeout (ran for {}s, timeout was {}s)",
                        &identity.action_key,
                        execution_time.as_secs(),
                        timeout.as_secs(),
                    )
                    .into()
                );

                if let Err(e) = res {
                    return ControlFlow::Break(manager.error("re_timeout_exceeded", e));
                }
            }
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
        cancellations: &CancellationContext,
    ) -> CommandExecutionResult {
        let PreparedCommand {
            request,
            target,
            prepared_action:
                PreparedAction {
                    action_and_blobs,
                    platform,
                    remote_execution_dependencies,
                },
            digest_config,
        } = command;

        let details = RemoteCommandExecutionDetails::new(
            command.prepared_action.digest(),
            command.request.remote_dep_file_key,
            self.re_client.get_session_id().await.ok(),
            self.re_use_case,
            &platform,
        );
        let manager = manager.with_execution_kind(CommandExecutionKind::Remote {
            details: details.clone(),
            queue_time: Duration::ZERO,
            materialized_inputs_for_failed: None,
            materialized_outputs_for_failed_actions: None,
        });

        if command.request.executor_preference().requires_local() {
            return ControlFlow::Break(
                manager.error("remote_prepare", RemoteExecutorError::LocalOnlyAction),
            )?;
        }

        let identity =
            ReActionIdentity::new(*target, self.re_action_key.as_deref(), request.paths());

        // TODO(bobyf, torozco): remote execution probably needs to explicitly handle cancellations
        let manager = self
            .upload(
                manager,
                &identity,
                &action_and_blobs.blobs,
                request.paths(),
                *digest_config,
            )
            .await?;

        let (manager, response) = self
            .re_execute(
                manager,
                &identity,
                request,
                &action_and_blobs.action,
                *digest_config,
                platform,
                self.dependencies
                    .iter()
                    .chain(remote_execution_dependencies.iter()),
                &command.request.meta_internal_extra_params(),
            )
            .await?;

        let exit_code = response.action_result.exit_code;
        let additional_message = if response.status.message.is_empty() {
            None
        } else {
            Some(response.status.message.clone())
        };

        let res = download_action_results(
            request,
            &*self.materializer,
            &self.re_client,
            self.re_use_case,
            *digest_config,
            manager,
            &identity,
            buck2_data::ReStage {
                stage: Some(buck2_data::ReDownload {}.into()),
            }
            .into(),
            request.paths(),
            request.outputs(),
            details,
            &response,
            self.paranoid.as_ref(),
            cancellations,
            exit_code,
            &self.artifact_fs,
            self.materialize_failed_inputs,
            self.materialize_failed_outputs,
            additional_message,
        )
        .boxed()
        .await;

        let DownloadResult::Result(mut res) = res;
        res.action_result = Some(response.action_result);
        res
    }

    fn is_local_execution_possible(&self, _executor_preference: ExecutorPreference) -> bool {
        false
    }
}

#[derive(buck2_error::Error, Debug)]
#[error(
    "action_digest={}, re_code={}, re_message={}",
    action_digest,
    inner.code,
    inner.message
)]
#[buck2(tier0, tag = get_re_error_tag(&inner.code))]
struct ReErrorWrapper {
    action_digest: ActionDigest,
    inner: remote_execution::TStatus,
}

fn as_missing_outputs_error(err: &remote_execution::TStatus) -> Option<&str> {
    // A dedicated error code would be better for this :(
    if err.message.contains("OUTMISS") {
        Some(&err.message)
    } else {
        None
    }
}

fn is_timeout_error(err: &remote_execution::TStatus) -> bool {
    #[cfg(fbcode_build)]
    {
        // Not ideal, but DEADLINE_EXCEEDED will show up if you e.g. timeout connecting to RE, so we
        // need to actually match on the message :(
        err.code == TCode::DEADLINE_EXCEEDED && err.message.contains("Execution timed out")
    }

    #[cfg(not(fbcode_build))]
    {
        // Not obvious what this looks like in the GRPC API.
        let _ignored = err;
        false
    }
}
