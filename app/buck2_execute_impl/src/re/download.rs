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

use anyhow::Context as _;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::DirectoryEntry;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestFromReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::extract_artifact_value;
use buck2_execute::directory::re_tree_to_directory;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::action_digest::TrackedActionDigest;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::manager::CommandExecutionManagerWithClaim;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionOutputRef;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_execute::re::remote_action_result::RemoteActionResult;
use chrono::DateTime;
use chrono::Duration;
use chrono::Utc;
use dupe::Dupe;
use futures::future;
use futures::FutureExt;
use gazebo::prelude::*;
use indexmap::IndexMap;
use more_futures::cancellation::CancellationContext;
use remote_execution as RE;
use thiserror::Error;

pub async fn download_action_results<'a>(
    request: &CommandExecutionRequest,
    materializer: &dyn Materializer,
    re_client: &ManagedRemoteExecutionClient,
    re_use_case: RemoteExecutorUseCase,
    digest_config: DigestConfig,
    manager: CommandExecutionManager,
    stage: buck2_data::executor_stage_start::Stage,
    paths: &CommandExecutionPaths,
    requested_outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
    action_digest: &ActionDigest,
    response: &dyn RemoteActionResult,
    cancellations: &CancellationContext,
) -> CommandExecutionResult {
    // Claim the request before starting the download.
    let manager = manager.claim().await;

    let downloader = CasDownloader {
        materializer,
        re_client,
        re_use_case,
        digest_config,
    };

    let download = downloader.download(
        manager,
        stage,
        paths,
        requested_outputs,
        action_digest,
        response,
        cancellations,
    );

    let std_streams = response.std_streams(re_client, re_use_case, digest_config);
    let std_streams = async {
        if request.prefetch_lossy_stderr() {
            std_streams.prefetch_lossy_stderr().await
        } else {
            std_streams
        }
    };

    let (download, std_streams) = future::join(download, std_streams).await;
    let (manager, outputs) = download?;

    manager.success(
        response.execution_kind(action_digest.dupe()),
        outputs,
        CommandStdStreams::Remote(std_streams),
        response.timing(),
    )
}

pub struct CasDownloader<'a> {
    pub materializer: &'a dyn Materializer,
    pub re_client: &'a ManagedRemoteExecutionClient,
    pub re_use_case: RemoteExecutorUseCase,
    pub digest_config: DigestConfig,
}

impl CasDownloader<'_> {
    async fn download<'a>(
        &self,
        manager: CommandExecutionManagerWithClaim,
        stage: buck2_data::executor_stage_start::Stage,
        paths: &CommandExecutionPaths,
        requested_outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
        action_digest: &ActionDigest,
        output_spec: &dyn RemoteActionResult,
        cancellations: &CancellationContext,
    ) -> ControlFlow<
        CommandExecutionResult,
        (
            CommandExecutionManagerWithClaim,
            IndexMap<CommandExecutionOutput, ArtifactValue>,
        ),
    > {
        executor_stage_async(stage, async {
            let artifacts = self
                .extract_artifacts(paths, requested_outputs, output_spec)
                .await;

            let artifacts = match artifacts {
                Ok(artifacts) => artifacts,
                Err(e) => {
                    return ControlFlow::Break(manager.error(
                        "extract_artifacts",
                        e.context(format!("action_digest={}", action_digest)),
                    ));
                }
            };

            let outputs = self
                .materialize_outputs(artifacts, action_digest, cancellations)
                .await;

            let outputs = match outputs {
                Ok(outputs) => outputs,
                Err(e) => {
                    return ControlFlow::Break(manager.error(
                        "materialize_outputs",
                        e.context(format!("action_digest={}", action_digest)),
                    ));
                }
            };

            ControlFlow::Continue((manager, outputs))
        })
        .await
    }

    async fn extract_artifacts<'a>(
        &self,
        paths: &CommandExecutionPaths,
        requested_outputs: impl Iterator<Item = CommandExecutionOutputRef<'a>>,
        output_spec: &dyn RemoteActionResult,
    ) -> anyhow::Result<ExtractedArtifacts> {
        static FAIL_RE_DOWNLOADS: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_FAIL_RE_DOWNLOADS");
        if FAIL_RE_DOWNLOADS.get()?.copied().unwrap_or_default() {
            return Err(anyhow::anyhow!("Injected error"));
        }

        let now = Utc::now();
        let ttl = Duration::seconds(output_spec.ttl());
        let expires = now + ttl;

        // Download process:
        // 1. merges all the outputs (files and trees) into the inputs structure
        // 2. computes the ArtifactValue for all outputs from that merged structure
        // 3. pass those new ArtifactValue to the materializer
        let input_dir = paths.input_directory();
        let output_paths = paths.output_paths();
        let mut input_dir = input_dir.clone().into_builder();

        for x in output_spec.output_files() {
            let digest = FileDigest::from_re(&x.digest.digest, self.digest_config)?;
            let digest = TrackedFileDigest::new_expires(
                digest,
                expires,
                self.digest_config.cas_digest_config(),
            );

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
            .download_typed_blobs::<RE::Tree>(
                output_spec
                    .output_directories()
                    .map(|x| x.tree_digest.clone()),
                self.re_use_case,
            )
            .boxed()
            .await
            .context(DownloadError::DownloadTrees)?;

        for (dir, tree) in output_spec.output_directories().iter().zip(trees) {
            let entry = re_tree_to_directory(&tree, &expires, self.digest_config)?;
            input_dir.insert(
                re_forward_path(dir.path.as_str())?,
                DirectoryEntry::Dir(entry),
            )?;
        }

        let mut to_declare = Vec::with_capacity(output_paths.len());
        let mut mapped_outputs = IndexMap::with_capacity(output_paths.len());

        for (requested, (path, _)) in requested_outputs.zip(output_paths.iter()) {
            let value = extract_artifact_value(&input_dir, path, self.digest_config)?;
            if let Some(value) = value {
                to_declare.push((path.to_owned(), value.dupe()));
                mapped_outputs.insert(requested.cloned(), value);
            }
        }

        Ok(ExtractedArtifacts {
            to_declare,
            mapped_outputs,
            now,
            expires,
            ttl,
        })
    }

    async fn materialize_outputs<'a>(
        &self,
        artifacts: ExtractedArtifacts,
        action_digest: &ActionDigest,
        cancellations: &CancellationContext,
    ) -> anyhow::Result<IndexMap<CommandExecutionOutput, ArtifactValue>> {
        // Declare the outputs to the materializer
        self.materializer
            .declare_cas_many(
                Arc::new(CasDownloadInfo::new_execution(
                    TrackedActionDigest::new_expires(
                        action_digest.dupe(),
                        artifacts.expires,
                        self.digest_config.cas_digest_config(),
                    ),
                    self.re_use_case,
                    artifacts.now,
                    artifacts.ttl,
                )),
                artifacts.to_declare,
                cancellations,
            )
            .boxed()
            .await
            .context(DownloadError::Materialization)?;

        Ok(artifacts.mapped_outputs)
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

struct ExtractedArtifacts {
    to_declare: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    mapped_outputs: IndexMap<CommandExecutionOutput, ArtifactValue>,
    now: DateTime<Utc>,
    expires: DateTime<Utc>,
    ttl: Duration,
}
