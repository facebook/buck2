/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::SystemTime;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::directory::DirectoryEntry;
use buck2_core::env_helper::EnvHelper;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_events::dispatch::span_async;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::directory_to_re_tree;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::cache_uploader::CacheUploadInfo;
use buck2_execute::execute::cache_uploader::DepFileEntry;
use buck2_execute::execute::cache_uploader::UploadCache;
use buck2_execute::execute::kind::CommandExecutionKind;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::execute::result::CommandExecutionStatus;
use buck2_execute::execute::target::CommandExecutionTarget;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use derive_more::Display;
use dupe::Dupe;
use futures::future;
use futures::future::FutureExt;
use remote_execution::DigestWithStatus;
use remote_execution::NamedDigest;
use remote_execution::REClientError;
use remote_execution::TActionResult2;
use remote_execution::TCode;
use remote_execution::TDirectory2;
use remote_execution::TExecutedActionMetadata;
use remote_execution::TFile;
use remote_execution::TStatus;
use remote_execution::TTimestamp;

// Whether to throw errors when cache uploads fail (primarily for tests).
static ERROR_ON_CACHE_UPLOAD: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_ERROR_ON_CACHE_UPLOAD");

/// A PreparedCommandExecutor that will write to cache after invoking the inner executor
pub struct CacheUploader {
    pub artifact_fs: ArtifactFs,
    pub materializer: Arc<dyn Materializer>,
    pub re_client: ManagedRemoteExecutionClient,
    pub re_use_case: RemoteExecutorUseCase,
    pub knobs: ExecutorGlobalKnobs,
    pub max_bytes: Option<u64>,
}

impl CacheUploader {
    /// Upload an action result to the RE action cache, assuming conditions for the upload are met:
    /// the action must have been successful and must have run locally (not much point in caching
    /// something that ran on RE and is already cached), and cache uploads must be enabled for this particular action.
    /// The CacheUploader should only be used if cache uploads are enabled.
    async fn maybe_perform_cache_upload(
        &self,
        target: &dyn CommandExecutionTarget,
        digest: &ActionDigest,
        result: &CommandExecutionResult,
        digest_config: DigestConfig,
    ) -> anyhow::Result<Option<CacheUploadOutcome>> {
        let output_bytes = result.calc_output_size_bytes();

        match &result.report.status {
            CommandExecutionStatus::Success {
                execution_kind: CommandExecutionKind::Local { .. },
            } => {
                // Fall through to the upload.
            }
            _ => {
                // In all other cases, skip.
                return Ok(None);
            }
        }
        let reason = buck2_data::CacheUploadReason::LocalExecution;
        let outcome = span_async(
            buck2_data::CacheUploadStart {
                key: Some(target.as_proto_action_key()),
                name: Some(target.as_proto_action_name()),
                action_digest: digest.to_string(),
                reason: reason.into(),
            },
            async move {
                let mut file_digests = Vec::new();
                let mut tree_digests = Vec::new();

                let res = async {
                    // NOTE: If the size exceeds the limit, we still log that attempt, since
                    // whoever is configuring this can't easily anticipate how large the outputs
                    // will be, so some logging is useful.
                    if let Some(max_bytes) = self.max_bytes {
                        if output_bytes > max_bytes {
                            return Ok(CacheUploadOutcome::Rejected(
                                CacheUploadRejectionReason::OutputExceedsLimit { max_bytes },
                            ));
                        }
                    }

                    self.perform_cache_upload(
                        digest,
                        result,
                        &mut file_digests,
                        &mut tree_digests,
                        digest_config,
                    )
                    .await
                }
                .await;

                let (success, error, re_error_code) = match &res {
                    Ok(CacheUploadOutcome::Success) => (true, String::new(), None),
                    Ok(CacheUploadOutcome::Rejected(reason)) => {
                        (false, format!("Rejected: {}", reason), None)
                    }
                    Err(e) => (
                        false,
                        format!("{:#}", e),
                        e.downcast_ref::<REClientError>()
                            .map(|e| e.code.to_string()),
                    ),
                };

                (
                    res,
                    Box::new(buck2_data::CacheUploadEnd {
                        key: Some(target.as_proto_action_key()),
                        name: Some(target.as_proto_action_name()),
                        action_digest: digest.to_string(),
                        success,
                        error,
                        re_error_code,
                        file_digests,
                        tree_digests,
                        output_bytes: Some(output_bytes),
                        reason: reason.into(),
                    }),
                )
            },
        )
        .await?;

        Ok(Some(outcome))
    }

    async fn perform_cache_upload(
        &self,
        digest: &ActionDigest,
        result: &CommandExecutionResult,
        file_digests: &mut Vec<String>,
        tree_digests: &mut Vec<String>,
        digest_config: DigestConfig,
    ) -> anyhow::Result<CacheUploadOutcome> {
        tracing::debug!("Uploading action result for `{}`", digest);

        let timing = result.report.timing;

        let mut upload_futs = vec![];
        let mut output_files = vec![];
        let mut output_directories = vec![];

        for (output, value) in result.resolve_outputs(&self.artifact_fs) {
            match value.entry().as_ref() {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                    output_files.push(TFile {
                        digest: DigestWithStatus {
                            digest: f.digest.to_re(),
                            status: TStatus {
                                code: TCode::OK,
                                message: String::new(),
                                ..Default::default()
                            },
                            ..Default::default()
                        },
                        name: output.path().to_string(),
                        executable: f.is_executable,
                        ..Default::default()
                    });

                    let fut = async move {
                        let name = self
                            .artifact_fs
                            .fs()
                            .resolve(output.path())
                            .as_maybe_relativized_str()?
                            .to_owned();

                        self.re_client
                            .upload_files_and_directories(
                                vec![NamedDigest {
                                    name,
                                    digest: f.digest.to_re(),
                                    ..Default::default()
                                }],
                                vec![],
                                vec![],
                                self.re_use_case,
                            )
                            .await
                    };

                    file_digests.push(f.digest.to_string());
                    upload_futs.push(fut.boxed());
                }
                DirectoryEntry::Dir(d) => {
                    let tree = directory_to_re_tree(d);
                    let mut action_blobs = ActionBlobs::new(digest_config);
                    let tree_digest = action_blobs.add_protobuf_message(&tree, digest_config);

                    output_directories.push(TDirectory2 {
                        path: output.path().to_string(),
                        tree_digest: tree_digest.to_re(),
                        root_directory_digest: d.fingerprint().to_re(),
                        ..Default::default()
                    });

                    let fut = async move {
                        self.re_client
                            .upload(
                                self.artifact_fs.fs(),
                                &self.materializer,
                                &action_blobs,
                                output.path(),
                                &d.dupe().as_immutable(),
                                self.re_use_case,
                                digest_config,
                            )
                            .await
                            .map(|_| ())
                    };

                    upload_futs.push(fut.boxed());
                    tree_digests.push(tree_digest.to_string());
                }
                DirectoryEntry::Leaf(..) => {
                    // Bail, there is something that is not a file here and we don't handle this.
                    // This will happen if the value is a symlink. The primary output of a command
                    // being a symlink is probably unlikely. Unfortunately, we can't represent this
                    // in RE's action output, so we either have to lie about the output and pretend
                    // it's a file, or bail.
                    return Ok(CacheUploadOutcome::Rejected(
                        CacheUploadRejectionReason::SymlinkOutput,
                    ));
                }
            }
        }

        let uploads = async {
            future::try_join_all(upload_futs)
                .await
                .context("Error uploading outputs")?;

            Ok(())
        };

        let std_streams = async {
            result
                .report
                .std_streams
                .clone()
                .into_re(&self.re_client, self.re_use_case)
                .await
                .context("Error accessing std_streams")
        };

        let ((), std_streams) = future::try_join(uploads, std_streams).await?;

        let worker;

        #[cfg(fbcode_build)]
        {
            let hostname = hostname::get()?;
            worker = hostname.to_string_lossy().into_owned();
        }

        #[cfg(not(fbcode_build))]
        {
            worker = "".to_owned();
        }

        let (stdout_raw, stdout_digest) = std_streams.stdout.into_raw_or_digest();
        let (stderr_raw, stderr_digest) = std_streams.stderr.into_raw_or_digest();

        let result = TActionResult2 {
            output_files,
            output_directories,
            exit_code: 0,
            stdout_raw,
            stdout_digest,
            stderr_raw,
            stderr_digest,
            execution_metadata: TExecutedActionMetadata {
                worker,
                execution_dir: "".to_owned(),
                execution_start_timestamp: systemtime_to_ttimestamp(timing.start_time)?,
                execution_completed_timestamp: systemtime_to_ttimestamp(
                    timing.start_time + timing.wall_time,
                )?,
                execution_attempts: 1,
                ..Default::default()
            },
            ..Default::default()
        };

        self.re_client
            .write_action_result(digest.to_re(), result, self.re_use_case)
            .await?;

        Ok(CacheUploadOutcome::Success)
    }
}

/// Whether we completed a cache upload.
#[derive(Copy, Clone, Dupe, Debug)]
enum CacheUploadOutcome {
    Success,
    Rejected(CacheUploadRejectionReason),
}

/// A reason why we chose not to upload.
#[derive(Copy, Clone, Dupe, Debug, Display)]
enum CacheUploadRejectionReason {
    #[display(fmt = "SymlinkOutput")]
    SymlinkOutput,
    #[display(fmt = "OutputExceedsLimit({})", max_bytes)]
    OutputExceedsLimit { max_bytes: u64 },
}

#[async_trait]
impl UploadCache for CacheUploader {
    async fn upload(
        &self,
        info: &CacheUploadInfo<'_>,
        res: &CommandExecutionResult,
        _dep_file_entry: Option<DepFileEntry>,
    ) -> anyhow::Result<bool> {
        let error_on_cache_upload = match ERROR_ON_CACHE_UPLOAD.get_copied() {
            Ok(r) => r.unwrap_or_default(),
            Err(e) => return Err(e).context("cache_upload"),
        };
        let action = &info.action_digest;

        // TODO(bobyf, torozco) should these be critical sections?
        let upload_res = self
            .maybe_perform_cache_upload(info.target.dupe(), action, res, info.digest_config)
            .await;

        match upload_res {
            Ok(Some(CacheUploadOutcome::Success)) => {
                tracing::info!("Cache upload for `{}` succeeded", action);
                Ok(true)
            }
            Ok(Some(CacheUploadOutcome::Rejected(reason))) => {
                tracing::info!("Cache upload for `{}` rejected: {:#}", action, reason);
                Ok(false)
            }
            Ok(None) => {
                tracing::info!("Cache upload for `{}` not attempted", action);
                Ok(false)
            }
            Err(error) => {
                if error_on_cache_upload {
                    return Err(error).context("cache_upload");
                } else {
                    tracing::warn!("Cache upload for `{}` failed: {:#}", action, error);
                }
                Ok(false)
            }
        }
    }
}

fn systemtime_to_ttimestamp(time: SystemTime) -> anyhow::Result<TTimestamp> {
    let duration = time.duration_since(SystemTime::UNIX_EPOCH)?;
    Ok(TTimestamp {
        seconds: duration.as_secs().try_into().context("Invalid duration")?,
        // Max 1B so it won't wrap around.
        nanos: duration.subsec_nanos() as _,
        ..Default::default()
    })
}
