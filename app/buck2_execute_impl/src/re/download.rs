/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::ops::ControlFlow;
use std::ops::FromResidual;
use std::path::Path;
use std::sync::Arc;

use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::paths::RelativePathBuf;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::console_message;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestFromReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::extract_artifact_value;
use buck2_execute::directory::re_tree_to_directory;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::Symlink;
use buck2_execute::execute::action_digest::TrackedActionDigest;
use buck2_execute::execute::executor_stage_async;
use buck2_execute::execute::kind::RemoteCommandExecutionDetails;
use buck2_execute::execute::manager::CommandExecutionManager;
use buck2_execute::execute::manager::CommandExecutionManagerExt;
use buck2_execute::execute::manager::CommandExecutionManagerWithClaim;
use buck2_execute::execute::output::CommandStdStreams;
use buck2_execute::execute::request::CommandExecutionOutput;
use buck2_execute::execute::request::CommandExecutionOutputRef;
use buck2_execute::execute::request::CommandExecutionPaths;
use buck2_execute::execute::request::CommandExecutionRequest;
use buck2_execute::execute::result::CommandExecutionErrorType;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::action_identity::ReActionIdentity;
use buck2_execute::re::error::RemoteExecutionError;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_execute::re::remote_action_result::RemoteActionResult;
use buck2_futures::cancellation::CancellationContext;
use chrono::DateTime;
use chrono::Duration;
use chrono::Utc;
use dupe::Dupe;
use futures::future;
use futures::FutureExt;
use gazebo::prelude::*;
use indexmap::IndexMap;
use remote_execution as RE;

use crate::executors::local::materialize_build_outputs;
use crate::executors::local::materialize_inputs;
use crate::re::paranoid_download::ParanoidDownloader;
use crate::storage_resource_exhausted::is_storage_resource_exhausted;

pub async fn download_action_results<'a>(
    request: &CommandExecutionRequest,
    materializer: &dyn Materializer,
    re_client: &ManagedRemoteExecutionClient,
    re_use_case: RemoteExecutorUseCase,
    digest_config: DigestConfig,
    manager: CommandExecutionManager,
    identity: &ReActionIdentity<'_>,
    stage: buck2_data::executor_stage_start::Stage,
    paths: &CommandExecutionPaths,
    requested_outputs: impl IntoIterator<Item = CommandExecutionOutputRef<'a>>,
    details: RemoteCommandExecutionDetails,
    response: &dyn RemoteActionResult,
    paranoid: Option<&ParanoidDownloader>,
    cancellations: &CancellationContext,
    action_exit_code: i32,
    artifact_fs: &ArtifactFs,
    materialize_failed_re_action_inputs: bool,
    materialize_failed_re_action_outputs: bool,
    additional_message: Option<String>,
) -> DownloadResult {
    let std_streams = response.std_streams(re_client, re_use_case, digest_config);
    let std_streams = async {
        if request.prefetch_lossy_stderr() {
            std_streams.prefetch_lossy_stderr().await
        } else {
            std_streams
        }
    };

    if action_exit_code != 0 && manager.inner.intend_to_fallback_on_failure {
        // Do not attempt to download outputs in this case so
        // as to avoid cancelling in-flight local execution:
        // either local already finished and the outputs are
        // already there, or local hasn't finished, and then
        // it will produce outputs.

        let std_streams = std_streams.await;
        return DownloadResult::Result(manager.failure(
            response.execution_kind(details),
            IndexMap::new(),
            CommandStdStreams::Remote(std_streams),
            Some(action_exit_code),
            response.timing(),
            additional_message,
        ));
    }
    let downloader = CasDownloader {
        materializer,
        re_client,
        re_use_case,
        digest_config,
        paranoid,
    };

    let download = downloader.download(
        manager,
        identity,
        stage,
        paths,
        requested_outputs,
        response,
        &details,
        cancellations,
    );

    let (download, std_streams) = future::join(download, std_streams).await;
    let (manager, outputs) = download?;

    let res = match action_exit_code {
        0 => manager.success(
            response.execution_kind(details),
            outputs,
            CommandStdStreams::Remote(std_streams),
            response.timing(),
        ),
        e => {
            let materialized_inputs = if materialize_failed_re_action_inputs {
                executor_stage_async(
                    buck2_data::ReStage {
                        stage: Some(buck2_data::MaterializeFailedInputs {}.into()),
                    },
                    async move {
                        match materialize_inputs(artifact_fs, materializer, request).await {
                            Ok(materialized_paths) => Some(materialized_paths.paths.clone()),
                            Err(e) => {
                                // TODO(minglunli): Properly handle this and the error below and add a test for it.
                                console_message(format!(
                                    "Failed to materialize inputs for failed action: {}",
                                    e
                                ));
                                None
                            }
                        }
                    },
                )
                .await
            } else {
                None
            };

            let materialized_outputs = if materialize_failed_re_action_outputs {
                match materialize_build_outputs(artifact_fs, materializer, request).await {
                    Ok(materialized_paths) => Some(materialized_paths.clone()),
                    Err(e) => {
                        console_message(format!(
                            "Failed to materialize outputs for failed action: {}",
                            e
                        ));
                        None
                    }
                }
            } else if !request.outputs_for_error_handler().is_empty() {
                match materializer
                    .ensure_materialized(request.outputs_for_error_handler().to_vec())
                    .await
                {
                    Ok(()) => Some(request.outputs_for_error_handler().to_vec()),
                    // Do nothing here, handle file not materialized/doesn't exit case in the error handler.
                    // This way local/remote behavior would be consistent and errors are handled at the same place
                    Err(_) => None,
                }
            } else {
                None
            };

            manager.failure(
                response.execution_kind_for_failed_actions(
                    details,
                    materialized_inputs,
                    materialized_outputs,
                ),
                outputs,
                CommandStdStreams::Remote(std_streams),
                Some(e),
                response.timing(),
                additional_message,
            )
        }
    };

    DownloadResult::Result(res)
}

pub struct CasDownloader<'a> {
    pub materializer: &'a dyn Materializer,
    pub re_client: &'a ManagedRemoteExecutionClient,
    pub re_use_case: RemoteExecutorUseCase,
    pub digest_config: DigestConfig,
    pub paranoid: Option<&'a ParanoidDownloader>,
}

impl CasDownloader<'_> {
    async fn download<'a>(
        &self,
        manager: CommandExecutionManager,
        identity: &ReActionIdentity<'_>,
        stage: buck2_data::executor_stage_start::Stage,
        paths: &CommandExecutionPaths,
        requested_outputs: impl IntoIterator<Item = CommandExecutionOutputRef<'a>>,
        output_spec: &dyn RemoteActionResult,
        details: &RemoteCommandExecutionDetails,
        cancellations: &CancellationContext,
    ) -> ControlFlow<
        DownloadResult,
        (
            CommandExecutionManagerWithClaim,
            IndexMap<CommandExecutionOutput, ArtifactValue>,
        ),
    > {
        let manager = manager.with_execution_kind(output_spec.execution_kind(details.clone()));
        executor_stage_async(stage, async {
            let artifacts = self
                .extract_artifacts(identity, paths, requested_outputs, output_spec)
                .await;

            let artifacts =
                match artifacts {
                    Ok(artifacts) => artifacts,
                    Err(e) => {
                        let error: buck2_error::Error = e
                            .context(format!("action_digest={}", details.action_digest))
                            .into();
                        let is_storage_resource_exhausted = error
                            .find_typed_context::<RemoteExecutionError>()
                            .map_or(false, |re_client_error| {
                                is_storage_resource_exhausted(re_client_error.as_ref())
                            });
                        let error_type = if is_storage_resource_exhausted {
                            CommandExecutionErrorType::StorageResourceExhausted
                        } else {
                            CommandExecutionErrorType::Other
                        };
                        return ControlFlow::Break(DownloadResult::Result(
                            manager.error_classified("extract_artifacts", error, error_type),
                        ));
                    }
                };

            let info = CasDownloadInfo::new_execution(
                TrackedActionDigest::new_expires(
                    details.action_digest.dupe(),
                    artifacts.expires,
                    self.digest_config.cas_digest_config(),
                ),
                self.re_use_case,
                artifacts.now,
                artifacts.ttl,
            );

            let (manager, outputs) = match self.paranoid {
                Some(paranoid) => {
                    let manager = paranoid
                        .declare_cas_many(
                            self.materializer,
                            manager,
                            info,
                            artifacts.to_declare,
                            cancellations,
                        )
                        .await
                        .map_break(DownloadResult::Result)?;
                    (manager, artifacts.mapped_outputs)
                }
                None => {
                    // Claim the request before starting the download.
                    let manager = manager.claim().await;

                    let outputs = self
                        .materialize_outputs(artifacts, info, cancellations)
                        .await;

                    let outputs = match outputs {
                        Ok(outputs) => outputs,
                        Err(e) => {
                            return ControlFlow::Break(DownloadResult::Result(manager.error(
                                "materialize_outputs",
                                e.context(format!("action_digest={}", details.action_digest)),
                            )));
                        }
                    };

                    (manager, outputs)
                }
            };

            ControlFlow::Continue((manager, outputs))
        })
        .await
    }

    async fn extract_artifacts<'a>(
        &self,
        identity: &ReActionIdentity<'_>,
        paths: &CommandExecutionPaths,
        requested_outputs: impl IntoIterator<Item = CommandExecutionOutputRef<'a>>,
        output_spec: &dyn RemoteActionResult,
    ) -> buck2_error::Result<ExtractedArtifacts> {
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

        for x in output_spec.output_symlinks() {
            let entry = DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(Arc::new(
                Symlink::new(RelativePathBuf::from_path(Path::new(&x.target))?),
            )));
            input_dir.insert(re_forward_path(x.name.as_str())?, entry)?;
        }

        // Compute the re_outputs from the output_directories
        // This requires traversing the trees to find symlinks that point outside such trees
        let trees = self
            .re_client
            .download_typed_blobs::<RE::Tree>(
                Some(identity),
                output_spec
                    .output_directories()
                    .map(|x| x.tree_digest.clone()),
                self.re_use_case,
            )
            .boxed()
            .await
            .buck_error_context(DownloadError::DownloadTrees)?;

        for (dir, tree) in output_spec.output_directories().iter().zip(trees) {
            let entry = re_tree_to_directory(&tree, &expires, self.digest_config)?;
            input_dir.insert(
                re_forward_path(dir.path.as_str())?,
                DirectoryEntry::Dir(entry),
            )?;
        }

        let mut to_declare = Vec::with_capacity(output_paths.len());
        let mut mapped_outputs = IndexMap::with_capacity(output_paths.len());

        for (requested, (path, _)) in requested_outputs.into_iter().zip(output_paths.iter()) {
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
        info: CasDownloadInfo,
        cancellations: &CancellationContext,
    ) -> buck2_error::Result<IndexMap<CommandExecutionOutput, ArtifactValue>> {
        // Declare the outputs to the materializer
        self.materializer
            .declare_cas_many(Arc::new(info), artifacts.to_declare, cancellations)
            .boxed()
            .await
            .buck_error_context(DownloadError::Materialization)?;

        Ok(artifacts.mapped_outputs)
    }
}

/// Takes a path that came from RE and tries to convert it to
/// a `ForwardRelativePath`. These paths are supposed to be forward relative,
/// so if the conversion fails, RE is broken.
fn re_forward_path(re_path: &str) -> buck2_error::Result<&ForwardRelativePath> {
    // RE sends us paths with trailing slash.
    ForwardRelativePath::new_trim_trailing_slashes(re_path)
        .buck_error_context(DownloadError::InvalidPathFromRe)
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
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

/// Did this download work out?
pub enum DownloadResult {
    /// Got a result: might be a success, might be a failure. Caller needs to deal with this
    /// result.
    Result(CommandExecutionResult),
}

impl FromResidual<ControlFlow<Self, Infallible>> for DownloadResult {
    fn from_residual(residual: ControlFlow<Self, Infallible>) -> Self {
        match residual {
            ControlFlow::Break(v) => v,
        }
    }
}
