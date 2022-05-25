/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{ops::ControlFlow, sync::Arc, time::SystemTime};

use anyhow::Context as _;
use buck2_common::file_ops::{FileDigestData, FileMetadata, TrackedFileDigest};
use buck2_core::{directory::DirectoryEntry, fs::paths::ForwardRelativePath};
use futures::future;
use gazebo::prelude::*;
use indexmap::IndexMap;
use thiserror::Error;

use crate::{
    actions::{
        artifact::ArtifactValue,
        digest::FileDigestFromReExt,
        directory::{convert_re_tree, extract_artifact_value, ActionDirectoryMember},
    },
    execute::{
        commands::{
            re::{
                client::ActionDigest, manager::ManagedRemoteExecutionClient, ActionPaths,
                RemoteActionResult,
            },
            ClaimedRequest, CommandExecutionManager, CommandExecutionOutput,
            CommandExecutionOutputRef, CommandExecutionResult,
        },
        materializer::{CasDownloadInfo, Materializer},
        CommandExecutionRequest,
    },
};

pub async fn download_action_results<'a>(
    request: &CommandExecutionRequest,
    materializer: &dyn Materializer,
    re_client: &ManagedRemoteExecutionClient,
    mut manager: CommandExecutionManager,
    stage: buck2_data::executor_stage_start::Stage,
    action_paths: &ActionPaths,
    requested_outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
    action_digest: &ActionDigest,
    response: &dyn RemoteActionResult,
) -> CommandExecutionResult {
    // Claim the request before starting the download.
    let claim = match manager.try_claim() {
        None => return manager.claim_rejected(),
        Some(v) => v,
    };

    let downloader = CasDownloader {
        materializer,
        re_client,
    };

    let download = downloader.download(
        manager,
        stage,
        claim,
        action_paths,
        requested_outputs,
        action_digest,
        response,
    );

    let std_streams = response.std_streams(re_client);
    let std_streams = async {
        if request.prefetch_lossy_stderr() {
            std_streams.prefetch_lossy_stderr().await
        } else {
            std_streams
        }
    };

    let (download, std_streams) = future::join(download, std_streams).await;
    let (manager, claim, outputs) = download?;

    manager.success(
        claim,
        response.execution_kind(action_digest.dupe()),
        outputs,
        std_streams.into(),
        response.timing(),
    )
}

pub struct CasDownloader<'a> {
    pub materializer: &'a dyn Materializer,
    pub re_client: &'a ManagedRemoteExecutionClient,
}

impl CasDownloader<'_> {
    async fn download<'a>(
        &self,
        mut manager: CommandExecutionManager,
        stage: buck2_data::executor_stage_start::Stage,
        claim: ClaimedRequest,
        action_paths: &ActionPaths,
        requested_outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
        action_digest: &ActionDigest,
        output_spec: &dyn RemoteActionResult,
    ) -> ControlFlow<
        CommandExecutionResult,
        (
            CommandExecutionManager,
            ClaimedRequest,
            IndexMap<CommandExecutionOutput, ArtifactValue>,
        ),
    > {
        let download_response = manager
            .stage_async(
                stage,
                self.materialize_files(action_paths, requested_outputs, action_digest, output_spec),
            )
            .await;

        let outputs = match download_response {
            Ok(outputs) => outputs,
            Err(e) => {
                return ControlFlow::Break(manager.error(
                    "download".into(),
                    e.context(format!("action_digest={}", action_digest)),
                ));
            }
        };

        ControlFlow::Continue((manager, claim, outputs))
    }

    async fn materialize_files<'a>(
        &self,
        action_paths: &ActionPaths,
        requested_outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
        action_digest: &ActionDigest,
        output_spec: &dyn RemoteActionResult,
    ) -> anyhow::Result<IndexMap<CommandExecutionOutput, ArtifactValue>> {
        let ttl: u64 = output_spec.ttl().try_into().unwrap_or(0);
        let expires = SystemTime::now() + std::time::Duration::from_secs(ttl);

        // Download process:
        // 1. merges all the outputs (files and trees) into the inputs structure
        // 2. computes the ArtifactValue for all outputs from that merged structure
        // 3. pass those new ArtifactValue to the materializer
        let ActionPaths {
            inputs: input_dir,
            outputs: output_paths,
            ..
        } = action_paths;
        let mut input_dir = input_dir.clone().into_builder();

        for x in output_spec.output_files() {
            let digest = FileDigestData::from_re(&x.digest.digest);
            let digest = TrackedFileDigest::new_expires(digest.sha1, digest.size, expires);

            let entry = DirectoryEntry::Leaf(ActionDirectoryMember::File(FileMetadata {
                digest,
                is_executable: x.executable,
            }));

            input_dir.insert(re_forward_path(x.name.as_str())?, entry)?;
        }

        // Compute the re_outputs from the output_directories
        // This requires traversing the trees to find symlinks that point outside such trees
        let trees = self
            .re_client
            .download_trees(
                output_spec
                    .output_directories()
                    .map(|x| x.tree_digest.clone()),
            )
            .await
            .context(DownloadError::DownloadTrees)?;

        for (dir, tree) in output_spec.output_directories().iter().zip(trees) {
            let entry = convert_re_tree(&tree, &expires)?;
            input_dir.insert(
                re_forward_path(dir.path.as_str())?,
                DirectoryEntry::Dir(entry),
            )?;
        }

        let mut to_declare = Vec::with_capacity(output_paths.len());
        let mut mapped_outputs = IndexMap::with_capacity(output_paths.len());

        for (requested, path) in requested_outputs.zip(output_paths.iter()) {
            let value = extract_artifact_value(&input_dir, path.as_ref())?;
            if let Some(value) = value {
                to_declare.push((path.clone(), value.dupe()));
                mapped_outputs.insert(requested.cloned(), value);
            }
        }

        // Declare the outputs to the materializer
        self.materializer
            .declare_cas_many(
                Arc::new(CasDownloadInfo::new(action_digest.dupe())),
                to_declare,
            )
            .await
            .context(DownloadError::Materialization)?;

        Ok(mapped_outputs)
    }
}

/// Takes a path that came from RE and tries to convert it to
/// a `ForwardRelativePath`. These paths are supposed to be forward relative,
/// so if the conversion fails, RE is broken.
fn re_forward_path(re_path: &str) -> anyhow::Result<&ForwardRelativePath> {
    // RE sends us paths with trailing slash.
    ForwardRelativePath::new_trim_trailing_slashes(re_path)
        .context(DownloadError::InvalidPathFromRe)
}

#[derive(Error, Debug)]
enum DownloadError {
    #[error("Failed to declare in materializer")]
    Materialization,

    #[error("Failed to download trees")]
    DownloadTrees,

    #[error("Path received from RE is not normalized.")]
    InvalidPathFromRe,
}
