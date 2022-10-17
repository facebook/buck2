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
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::sorted_hash_map::SortedHashMap;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blobs::ActionBlobs;
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
use gazebo::prelude::*;
use indexmap::IndexMap;
use remote_execution as RE;
use remote_execution::ExecuteResponse;
use remote_execution::TCode;
use thiserror::Error;
use tracing::info;

use crate::re::download::download_action_results;

// temporary platform like thing to build apple. We probably eventually want to replace this with
// the action/target/execution group platform.
#[derive(Clone)]
pub enum ReExecutionPlatform {
    Linux,
    MacOS { xcode_version: String },
    Windows,
}

#[derive(Debug, Error)]
pub enum RemoteExecutorError {
    #[error("Trying to execute a `local_only = True` action on remote executor for {0}")]
    LocalOnlyAction(String),
}

impl ReExecutionPlatform {
    pub fn intrinsic_properties(&self) -> SortedHashMap<String, String> {
        match self {
            Self::Linux => SortedHashMap::from_iter([(
                "platform".to_owned(),
                "linux-remote-execution".to_owned(),
            )]),
            Self::MacOS { xcode_version } => SortedHashMap::from_iter([
                ("platform".to_owned(), "mac".to_owned()),
                ("subplatform".to_owned(), format!("xcode-{}", xcode_version)),
            ]),
            Self::Windows => {
                SortedHashMap::from_iter([("platform".to_owned(), "windows".to_owned())])
            }
        }
    }
}

pub struct ReExecutor {
    pub artifact_fs: ArtifactFs,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub project_fs: ProjectRoot,
    pub re_platform: RE::Platform,
    pub re_action_key: Option<String>,
    pub re_max_input_files_bytes: u64,
    pub re_use_case: RemoteExecutorUseCase,
    pub knobs: ExecutorGlobalKnobs,
}

impl ReExecutor {
    pub fn new(
        artifact_fs: ArtifactFs,
        project_fs: ProjectRoot,
        materializer: Arc<dyn Materializer>,
        re_client: ManagedRemoteExecutionClient,
        re_properties: Vec<(String, String)>,
        re_action_key: Option<String>,
        re_max_input_files_bytes: u64,
        re_use_case: RemoteExecutorUseCase,
        knobs: ExecutorGlobalKnobs,
    ) -> Self {
        Self {
            artifact_fs,
            materializer,
            re_client,
            project_fs,
            re_platform: RE::Platform {
                properties: re_properties.into_map(|(name, value)| RE::Property { name, value }),
            },
            re_action_key,
            re_max_input_files_bytes,
            re_use_case,
            knobs,
        }
    }

    /// Indicate whether an action is too big to run on RE.
    pub fn is_action_too_large(&self, action_paths: &ActionPaths) -> bool {
        action_paths.input_files_bytes > self.re_max_input_files_bytes
    }

    async fn upload(
        &self,
        mut manager: CommandExecutionManager,
        blobs: &ActionBlobs,
        action_paths: &ActionPaths,
    ) -> ControlFlow<CommandExecutionResult, CommandExecutionManager> {
        let re_client = &self.re_client;

        let upload_response = manager
            .stage_async(
                buck2_data::ReStage {
                    stage: Some(buck2_data::ReUpload {}.into()),
                },
                re_client.upload(
                    &self.materializer,
                    blobs,
                    ProjectRelativePath::empty(),
                    &action_paths.inputs,
                    self.re_use_case,
                ),
            )
            .await;

        match upload_response {
            Ok(()) => {}
            Err(e) => return ControlFlow::Break(manager.error("remote_upload_error".into(), e)),
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
                &self.re_platform,
                self.re_use_case,
                &identity,
                &mut manager,
            )
            .await;

        let response = match execute_response {
            Ok(result) => result,
            Err(e) => return ControlFlow::Break(manager.error("remote_exec_error".into(), e)),
        };

        let action_result = &response.action_result;

        if response.error.code != TCode::OK {
            return ControlFlow::Break(
                manager.error(
                    "remote_exec_error".into(),
                    ReErrorWrapper {
                        action_digest: action_digest.dupe(),
                        inner: response.error,
                    }
                    .into(),
                ),
            );
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
                CommandStdStreams::Remote(response.std_streams(&self.re_client, self.re_use_case)),
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
                },
        } = command;

        if command.request.executor_preference().requires_local() {
            let error = anyhow::anyhow!(RemoteExecutorError::LocalOnlyAction(target.to_string()));
            return ControlFlow::Break(manager.error("remote_prepare".into(), error))?;
        }

        let manager = self.upload(manager, blobs, action_paths).await?;

        let (manager, response) = self
            .re_execute(manager, target, request, action_digest, action_paths)
            .await?;

        download_action_results(
            request,
            &*self.materializer,
            &self.re_client,
            self.re_use_case,
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
        .await
    }

    fn re_platform(&self) -> Option<&RE::Platform> {
        Some(&self.re_platform)
    }

    fn re_use_case(&self) -> RemoteExecutorUseCase {
        self.re_use_case
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
