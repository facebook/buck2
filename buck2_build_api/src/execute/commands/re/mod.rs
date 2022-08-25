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
use std::time::SystemTime;

use async_trait::async_trait;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_node::execute::config::RemoteExecutorUseCase;
use gazebo::prelude::*;
use indexmap::IndexMap;
use remote_execution as RE;
use remote_execution::ActionResultResponse;
use remote_execution::ExecuteResponse;
use remote_execution::TCode;
use remote_execution::TDirectory2;
use remote_execution::TExecutedActionMetadata;
use remote_execution::TFile;
use remote_execution::TTimestamp;
use starlark::collections::SmallMap;
use thiserror::Error;
use tracing::info;

use crate::actions::artifact::ArtifactFs;
use crate::actions::directory::ActionImmutableDirectory;
use crate::execute::commands::output::RemoteCommandStdStreams;
use crate::execute::commands::re::client::ActionDigest;
use crate::execute::commands::re::client::PreparedAction;
use crate::execute::commands::re::download::download_action_results;
use crate::execute::commands::re::manager::ManagedRemoteExecutionClient;
use crate::execute::commands::re::uploader::ActionBlobs;
use crate::execute::commands::CommandExecutionKind;
use crate::execute::commands::CommandExecutionManager;
use crate::execute::commands::CommandExecutionRequest;
use crate::execute::commands::CommandExecutionResult;
use crate::execute::commands::CommandExecutionTarget;
use crate::execute::commands::CommandExecutionTimingData;
use crate::execute::commands::ExecutorName;
use crate::execute::commands::PreparedCommand;
use crate::execute::commands::PreparedCommandExecutor;
use crate::execute::materializer::Materializer;

pub mod caching_executor;
pub mod client;
pub mod download;
pub mod manager;
pub mod metadata;
pub mod uploader;

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
    pub fn intrinsic_properties(&self) -> SmallMap<String, String> {
        let mut map = SmallMap::new();

        match self {
            Self::Linux => {
                map.insert("platform".to_owned(), "linux-remote-execution".to_owned());
            }
            Self::MacOS { xcode_version } => {
                map.insert("platform".to_owned(), "mac".to_owned());
                map.insert("subplatform".to_owned(), format!("xcode-{}", xcode_version));
            }
            Self::Windows => {
                map.insert("platform".to_owned(), "windows".to_owned());
            }
        };

        map
    }
}

pub struct ActionPaths {
    pub inputs: ActionImmutableDirectory,
    pub outputs: Vec<ProjectRelativePathBuf>,

    /// Total size of input files.
    pub input_files_bytes: u64,
}

/// Daemon-level config that can tweak how the ReExecutor works.
#[derive(Clone, Dupe, Default)]
pub struct ReExecutorGlobalKnobs {
    pub always_check_ttls: bool,
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
    pub knobs: ReExecutorGlobalKnobs,
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
        knobs: ReExecutorGlobalKnobs,
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
                    &self.re_use_case,
                    &self.knobs,
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
                &self.re_use_case,
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
            return ControlFlow::Break(
                manager.failure(
                    CommandExecutionKind::Remote {
                        digest: action_digest.dupe(),
                    },
                    // TODO: we want to expose RE outputs even when actions fail,
                    //   this will allow tpx to correctly retrieve the output of
                    //   failing tests running on RE. See D34344489 for context.
                    IndexMap::new(),
                    response
                        .std_streams(&self.re_client, &self.re_use_case)
                        .into(),
                    Some(action_result.exit_code),
                ),
            );
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
            &self.re_use_case,
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

    fn name(&self) -> ExecutorName {
        ExecutorName("remote")
    }
}

pub trait RemoteActionResult: Send + Sync {
    fn output_files(&self) -> &[TFile];
    fn output_directories(&self) -> &[TDirectory2];

    fn execution_kind(&self, digest: ActionDigest) -> CommandExecutionKind;

    fn timing(&self) -> CommandExecutionTimingData;

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: &RemoteExecutorUseCase,
    ) -> RemoteCommandStdStreams;

    /// The TTL given by RE for the outputs for this action.
    fn ttl(&self) -> i64;
}

impl RemoteActionResult for ExecuteResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn execution_kind(&self, digest: ActionDigest) -> CommandExecutionKind {
        CommandExecutionKind::Remote { digest }
    }

    fn timing(&self) -> CommandExecutionTimingData {
        timing_from_re_metadata(&self.action_result.execution_metadata)
    }

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: &RemoteExecutorUseCase,
    ) -> RemoteCommandStdStreams {
        RemoteCommandStdStreams::new(&self.action_result, client, use_case)
    }

    fn ttl(&self) -> i64 {
        self.action_result_ttl
    }
}

impl RemoteActionResult for ActionResultResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn execution_kind(&self, digest: ActionDigest) -> CommandExecutionKind {
        CommandExecutionKind::ActionCache { digest }
    }

    fn timing(&self) -> CommandExecutionTimingData {
        let mut timing = timing_from_re_metadata(&self.action_result.execution_metadata);
        timing.wall_time = Duration::ZERO; // This was a cache hit so we didn't wait.
        timing
    }

    fn std_streams(
        &self,
        client: &ManagedRemoteExecutionClient,
        use_case: &RemoteExecutorUseCase,
    ) -> RemoteCommandStdStreams {
        RemoteCommandStdStreams::new(&self.action_result, client, use_case)
    }

    fn ttl(&self) -> i64 {
        self.ttl
    }
}

fn timing_from_re_metadata(meta: &TExecutedActionMetadata) -> CommandExecutionTimingData {
    let execution_time = meta
        .execution_completed_timestamp
        .saturating_duration_since(&meta.execution_start_timestamp);

    let start_time = SystemTime::UNIX_EPOCH
        + meta
            .execution_start_timestamp
            .saturating_duration_since(&TTimestamp::unix_epoch());

    CommandExecutionTimingData {
        wall_time: execution_time,
        execution_time,
        start_time,
    }
}

pub struct ReActionIdentity<'a, 'b> {
    /// This is currently unused, but historically it has been useful to add logging in the RE
    /// client, so it's worth keeping around.
    _target: &'a CommandExecutionTarget<'b>,

    /// Actions with the same action key share e.g. memory requirements learnt by RE.
    action_key: String,

    /// Actions with the same affinity key get scheduled on similar hosts.
    affinity_key: String,

    /// Details about the action collected while uploading
    action_paths: &'a ActionPaths,
}

impl<'a, 'b> ReActionIdentity<'a, 'b> {
    fn new(
        target: &'a CommandExecutionTarget<'b>,
        executor_action_key: Option<&str>,
        action_paths: &'a ActionPaths,
    ) -> Self {
        let mut action_key = target.re_action_key();
        if let Some(executor_action_key) = executor_action_key {
            action_key = format!("{} {}", executor_action_key, action_key);
        }

        Self {
            _target: target,
            action_key,
            affinity_key: target.re_affinity_key(),
            action_paths,
        }
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
