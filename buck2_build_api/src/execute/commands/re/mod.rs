/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    ops::ControlFlow,
    sync::Arc,
    time::{Duration, SystemTime},
};

use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_core::fs::project::{ProjectFilesystem, ProjectRelativePathBuf};
use futures::future;
use gazebo::prelude::*;
use indexmap::{indexmap, IndexMap};
use remote_execution as RE;
use remote_execution::{
    ActionResultResponse, ExecuteResponse, TCode, TDirectory2, TExecutedActionMetadata, TFile,
    TTimestamp,
};
use thiserror::Error;
use tracing::info;

use crate::{
    actions::{
        artifact::ArtifactFs,
        digest::{FileDigestReExt, ReDigest},
        directory::ActionImmutableDirectory,
    },
    execute::{
        commands::{
            re::{
                client::{ActionDigest, PreparedAction},
                download::download_action_results,
                manager::ManagedRemoteExecutionClient,
                uploader::ActionBlobs,
            },
            CommandExecutionManager, CommandExecutionRequest, CommandExecutionResult,
            CommandExecutionTarget, CommandExecutionTimingData, ExecutorName, PreparedCommand,
            PreparedCommandExecutor,
        },
        materializer::Materializer,
        ActionExecutionKind,
    },
};

pub mod cache_check;
pub mod client;
pub mod download;
pub mod manager;
pub mod uploader;

// temporary platform like thing to build apple. We probably eventually want to replace this with
// the action/target/execution group platform.
#[derive(Clone)]
pub enum ExecutionPlatform {
    Linux,
    MacOS { xcode_version: String },
    Windows,
}

impl ExecutionPlatform {
    pub fn intrinsic_properties(&self) -> IndexMap<String, String> {
        match self {
            ExecutionPlatform::Linux => indexmap! {
                "platform".to_owned() => "linux-remote-execution".to_owned()
            },
            ExecutionPlatform::MacOS { xcode_version } => indexmap! {
                "platform".to_owned() => "mac".to_owned(),
                "subplatform".to_owned() => format!("xcode-{}", xcode_version)
            },
            ExecutionPlatform::Windows => indexmap! {
                "platform".to_owned() => "windows-remote-execution".to_owned()
            },
        }
    }
}

async fn disp_stream(
    raw: &Option<Vec<u8>>,
    digest: &Option<ReDigest>,
    client: &ManagedRemoteExecutionClient,
) -> String {
    // 4MBs seems like a reasonably large volume of output. There is no research or science behind
    // this number.
    const MAX_STREAM_DOWNLOAD_SIZE: i64 = 4 * 1024 * 1024;

    match (raw, digest) {
        (Some(raw), _) if !raw.is_empty() => String::from_utf8_lossy(raw).to_string(),
        (_, Some(digest)) if digest.size_in_bytes <= MAX_STREAM_DOWNLOAD_SIZE => {
            match client.download_blob(digest).await {
                Ok(bytes) => String::from_utf8_lossy(&bytes).to_string(),
                Err(e) => {
                    tracing::warn!("Failed to download action stderr: {:#}", e);
                    format!(
                        "Result could not be downloaded - to view type `frecli cas download-blob {}`",
                        FileDigest::from_re(digest),
                    )
                }
            }
        }
        (_, Some(digest)) => format!(
            "Result too large to display - to view type `frecli cas download-blob {}`",
            FileDigest::from_re(digest),
        ),
        (_, None) => String::new(),
    }
}

pub struct ActionPaths {
    pub inputs: ActionImmutableDirectory,
    pub outputs: Vec<ProjectRelativePathBuf>,

    /// Total size of input files.
    pub input_files_bytes: u64,
}

pub struct ReExecutor {
    pub artifact_fs: ArtifactFs,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub project_fs: ProjectFilesystem,
    pub re_platform: RE::Platform,
    pub re_action_key: Option<String>,
    pub re_max_input_files_bytes: u64,
}

impl ReExecutor {
    pub fn new(
        artifact_fs: ArtifactFs,
        project_fs: ProjectFilesystem,
        materializer: Arc<dyn Materializer>,
        re_client: ManagedRemoteExecutionClient,
        re_properties: Vec<(String, String)>,
        re_action_key: Option<String>,
        re_max_input_files_bytes: u64,
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
                re_client.upload(self.materializer.dupe(), blobs, &action_paths.inputs),
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
            let (stdout, stderr) = future::join(
                response.stdout(&self.re_client),
                response.stderr(&self.re_client),
            )
            .await;

            return ControlFlow::Break(manager.failure(
                ActionExecutionKind::Remote {
                    digest: action_digest.dupe(),
                },
                // TODO: we want to expose RE outputs even when actions fail,
                //   this will allow tpx to correctly retrieve the output of
                //   failing tests running on RE. See D34344489 for context.
                IndexMap::new(),
                stdout,
                stderr,
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

        let manager = self.upload(manager, blobs, action_paths).await?;

        let (manager, response) = self
            .re_execute(manager, target, request, action_digest, action_paths)
            .await?;

        download_action_results(
            &*self.materializer,
            &self.re_client,
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

#[async_trait]
pub trait RemoteActionResult: Send + Sync {
    fn output_files(&self) -> &[TFile];
    fn output_directories(&self) -> &[TDirectory2];

    fn execution_kind(&self, digest: ActionDigest) -> ActionExecutionKind;

    fn timing(&self) -> CommandExecutionTimingData;

    async fn stderr(&self, client: &ManagedRemoteExecutionClient) -> String;

    async fn stdout(&self, client: &ManagedRemoteExecutionClient) -> String;
}

#[async_trait]
impl RemoteActionResult for ExecuteResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn execution_kind(&self, digest: ActionDigest) -> ActionExecutionKind {
        ActionExecutionKind::Remote { digest }
    }

    fn timing(&self) -> CommandExecutionTimingData {
        timing_from_re_metadata(&self.action_result.execution_metadata)
    }

    async fn stderr(&self, client: &ManagedRemoteExecutionClient) -> String {
        disp_stream(
            &self.action_result.stderr_raw,
            &self.action_result.stderr_digest,
            client,
        )
        .await
    }

    async fn stdout(&self, client: &ManagedRemoteExecutionClient) -> String {
        disp_stream(
            &self.action_result.stdout_raw,
            &self.action_result.stdout_digest,
            client,
        )
        .await
    }
}

#[async_trait]
impl RemoteActionResult for ActionResultResponse {
    fn output_files(&self) -> &[TFile] {
        &self.action_result.output_files
    }

    fn output_directories(&self) -> &[TDirectory2] {
        &self.action_result.output_directories
    }

    fn execution_kind(&self, digest: ActionDigest) -> ActionExecutionKind {
        ActionExecutionKind::ActionCache { digest }
    }

    fn timing(&self) -> CommandExecutionTimingData {
        let mut timing = timing_from_re_metadata(&self.action_result.execution_metadata);
        timing.wall_time = Duration::ZERO; // This was a cache hit so we didn't wait.
        timing
    }

    async fn stderr(&self, client: &ManagedRemoteExecutionClient) -> String {
        disp_stream(
            &self.action_result.stderr_raw,
            &self.action_result.stderr_digest,
            client,
        )
        .await
    }

    async fn stdout(&self, client: &ManagedRemoteExecutionClient) -> String {
        disp_stream(
            &self.action_result.stdout_raw,
            &self.action_result.stdout_digest,
            client,
        )
        .await
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
