/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::sync::Arc;
use std::time::SystemTime;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_action_metadata_proto::REMOTE_DEP_FILE_KEY;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::buck2_env;
use buck2_core::directory::DirectoryEntry;
use buck2_core::execution_types::executor_config::RePlatformFields;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_events::dispatch::span_async;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::directory_to_re_tree;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::action_digest::ActionDigest;
use buck2_execute::execute::action_digest::ActionDigestKind;
use buck2_execute::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::cache_uploader::CacheUploadInfo;
use buck2_execute::execute::cache_uploader::CacheUploadResult;
use buck2_execute::execute::cache_uploader::DepFileEntry;
use buck2_execute::execute::cache_uploader::UploadCache;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use derive_more::Display;
use dupe::Dupe;
use futures::future;
use futures::future::FutureExt;
use gazebo::prelude::VecExt;
use prost::Message;
use remote_execution::DigestWithStatus;
use remote_execution::NamedDigest;
use remote_execution::REClientError;
use remote_execution::TActionResult2;
use remote_execution::TAny;
use remote_execution::TCode;
use remote_execution::TDirectory2;
use remote_execution::TExecutedActionMetadata;
use remote_execution::TFile;
use remote_execution::TStatus;
use remote_execution::TTimestamp;

use crate::executors::action_cache_upload_permission_checker::ActionCacheUploadPermissionChecker;
use crate::executors::to_re_platform::RePlatformFieldsToRePlatform;

// Whether to throw errors when cache uploads fail (primarily for tests).
fn error_on_cache_unload() -> anyhow::Result<bool> {
    buck2_env!("BUCK2_TEST_ERROR_ON_CACHE_UPLOAD", bool)
}

#[derive(Copy, Clone, Debug, Dupe, Eq, PartialEq)]
enum CacheUploadSuccessful {
    Yes,
    No,
}

/// A PreparedCommandExecutor that will write to cache after invoking the inner executor
pub struct CacheUploader {
    artifact_fs: ArtifactFs,
    materializer: Arc<dyn Materializer>,
    re_client: ManagedRemoteExecutionClient,
    re_use_case: RemoteExecutorUseCase,
    platform: RePlatformFields,
    max_bytes: Option<u64>,
    cache_upload_permission_checker: Arc<ActionCacheUploadPermissionChecker>,
}

impl CacheUploader {
    pub fn new(
        artifact_fs: ArtifactFs,
        materializer: Arc<dyn Materializer>,
        re_client: ManagedRemoteExecutionClient,
        re_use_case: RemoteExecutorUseCase,
        platform: RePlatformFields,
        max_bytes: Option<u64>,
        cache_upload_permission_checker: Arc<ActionCacheUploadPermissionChecker>,
    ) -> CacheUploader {
        CacheUploader {
            artifact_fs,
            materializer,
            re_client,
            re_use_case,
            platform,
            max_bytes,
            cache_upload_permission_checker,
        }
    }

    // Only return error on upload failure if we pass a flag
    fn modify_upload_result(
        digest: &dyn Display,
        result: anyhow::Result<CacheUploadSuccessful>,
        error_on_cache_upload: bool,
    ) -> anyhow::Result<CacheUploadSuccessful> {
        match result {
            Err(e) => {
                if error_on_cache_upload {
                    Err(e).context("cache_upload")
                } else {
                    tracing::warn!("Cache upload for `{}` failed: {:#}", digest, e);
                    Ok(CacheUploadSuccessful::No)
                }
            }
            _ => result,
        }
    }

    async fn upload_action_result(
        &self,
        info: &CacheUploadInfo<'_>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
        result: &CommandExecutionResult,
        error_on_cache_upload: bool,
    ) -> anyhow::Result<CacheUploadSuccessful> {
        tracing::debug!(
            "Uploading action result for `{}`",
            action_digest_and_blobs.action
        );
        let result = self
            .perform_cache_upload(
                info,
                result,
                action_digest_and_blobs.action.dupe(),
                vec![],
                buck2_data::CacheUploadReason::LocalExecution,
                &action_digest_and_blobs.blobs,
            )
            .await;
        Self::modify_upload_result(
            &action_digest_and_blobs.action,
            result,
            error_on_cache_upload,
        )
    }

    /// Upload an action result with additional information about dep files to the RE action cache.
    /// The conditions for the upload are: the action must have been successful and produced a depfile
    /// and cache uploads must have been enabled for this action.
    async fn upload_dep_file_result(
        &self,
        info: &CacheUploadInfo<'_>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
        result: &CommandExecutionResult,
        dep_file_entry: DepFileEntry,
        error_on_cache_upload: bool,
    ) -> anyhow::Result<CacheUploadSuccessful> {
        tracing::debug!(
            "Uploading dep file entry for action `{}` with dep file key `{}`",
            action_digest_and_blobs.action,
            dep_file_entry.key
        );
        let digest_re = dep_file_entry.key.coerce::<ActionDigestKind>();
        let dep_file_tany = TAny {
            type_url: REMOTE_DEP_FILE_KEY.to_owned(),
            value: dep_file_entry.entry.encode_to_vec(),
            ..Default::default()
        };
        let result = self
            .perform_cache_upload(
                info,
                result,
                digest_re,
                vec![dep_file_tany],
                buck2_data::CacheUploadReason::DepFile,
                &action_digest_and_blobs.blobs,
            )
            .await;

        Self::modify_upload_result(&dep_file_entry.key, result, error_on_cache_upload)
    }

    /// Upload an action result to the RE action cache, assuming conditions for the upload are met:
    /// the action must have been successful and must have run locally (not much point in caching
    /// something that ran on RE and is already cached), and cache uploads must be enabled for this particular action.
    /// The CacheUploader should only be used if cache uploads are enabled.
    async fn perform_cache_upload(
        &self,
        info: &CacheUploadInfo<'_>,
        result: &CommandExecutionResult,
        digest: ActionDigest,
        metadata: Vec<TAny>,
        reason: buck2_data::CacheUploadReason,
        action_blobs: &ActionBlobs,
    ) -> anyhow::Result<CacheUploadSuccessful> {
        let digest_str = digest.to_string();
        let output_bytes = result.calc_output_size_bytes();

        span_async(
            buck2_data::CacheUploadStart {
                key: Some(info.target.as_proto_action_key()),
                name: Some(info.target.as_proto_action_name()),
                action_digest: digest_str.clone(),
                reason: reason.into(),
            },
            async {
                let mut file_digests = Vec::new();
                let mut tree_digests = Vec::new();

                let res: std::result::Result<CacheUploadOutcome, anyhow::Error> = async {
                    if let Some(max_bytes) = self.max_bytes {
                        if output_bytes > max_bytes {
                            return Ok(CacheUploadOutcome::Rejected(
                                CacheUploadRejectionReason::OutputExceedsLimit { max_bytes },
                            ));
                        }
                    }

                    if let Err(reason) = self
                        .cache_upload_permission_checker
                        .has_permission_to_upload_to_cache(self.re_use_case, &self.platform)
                        .await?
                    {
                        return Ok(CacheUploadOutcome::Rejected(
                            CacheUploadRejectionReason::PermissionDenied(reason),
                        ));
                    }

                    // upload Action to CAS.
                    // This is necessary when writing to the ActionCache through CAS, since CAS needs to inspect the Action related to the ActionResult.
                    // Without storing the Action itself to CAS, ActionCache writes would fail.
                    self.re_client
                        .upload_files_and_directories(
                            vec![],
                            vec![],
                            action_blobs.to_inlined_blobs(),
                            self.re_use_case,
                        )
                        .await?;

                    // upload ActionResult to ActionCache
                    let result: TActionResult2 = match self
                        .upload_files_and_directories(
                            result,
                            &mut file_digests,
                            &mut tree_digests,
                            info.digest_config,
                            metadata,
                        )
                        .await?
                    {
                        Err(rejection) => {
                            return Ok(CacheUploadOutcome::Rejected(rejection));
                        }
                        Ok(taction2) => taction2,
                    };

                    self.re_client
                        .write_action_result(
                            digest,
                            result,
                            self.re_use_case,
                            &self.platform.to_re_platform(),
                        )
                        .await?;

                    Ok(CacheUploadOutcome::Success)
                }
                .await;

                let (success, error, re_error_code) = match &res {
                    Ok(CacheUploadOutcome::Success) => {
                        tracing::info!("Cache upload for `{}` succeeded", digest_str);
                        (CacheUploadSuccessful::Yes, String::new(), None)
                    }
                    Ok(CacheUploadOutcome::Rejected(reason)) => {
                        tracing::info!("Cache upload for `{}` rejected: {:#}", digest_str, reason);
                        let re_error_code = match reason {
                            CacheUploadRejectionReason::SymlinkOutput
                            | CacheUploadRejectionReason::OutputExceedsLimit { .. } => None,
                            CacheUploadRejectionReason::PermissionDenied(_) => {
                                Some(TCode::PERMISSION_DENIED.to_string())
                            }
                        };
                        (
                            CacheUploadSuccessful::No,
                            format!("Rejected: {}", reason),
                            re_error_code,
                        )
                    }
                    Err(e) => (
                        CacheUploadSuccessful::No,
                        format!("{:#}", e),
                        e.downcast_ref::<REClientError>()
                            .map(|e| e.code.to_string()),
                    ),
                };

                (
                    Ok(success),
                    Box::new(buck2_data::CacheUploadEnd {
                        key: Some(info.target.as_proto_action_key()),
                        name: Some(info.target.as_proto_action_name()),
                        action_digest: digest_str.clone(),
                        success: success == CacheUploadSuccessful::Yes,
                        error,
                        re_error_code,
                        file_digests: file_digests.into_map(|d| d.to_string()),
                        tree_digests: tree_digests.into_map(|d| d.to_string()),
                        output_bytes: Some(output_bytes),
                        reason: reason.into(),
                    }),
                )
            },
        )
        .await
    }

    async fn upload_files_and_directories(
        &self,
        result: &CommandExecutionResult,
        file_digests: &mut Vec<TrackedFileDigest>,
        tree_digests: &mut Vec<TrackedFileDigest>,
        digest_config: DigestConfig,
        // metadata to be added in the auxiliary_metadata field of TActionResult
        metadata: Vec<TAny>,
    ) -> anyhow::Result<Result<TActionResult2, CacheUploadRejectionReason>> {
        let mut upload_futs = vec![];
        let mut output_files: Vec<TFile> = Vec::new();
        let mut output_directories: Vec<TDirectory2> = Vec::new();

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

                    file_digests.push(f.digest.dupe());
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

                    let identity = None; //TODO: implement this
                    let fut = async move {
                        self.re_client
                            .upload(
                                self.artifact_fs.fs(),
                                &self.materializer,
                                &action_blobs,
                                output.path(),
                                &d.dupe().as_immutable(),
                                self.re_use_case,
                                identity,
                                digest_config,
                            )
                            .await
                            .map(|_| ())
                    };

                    upload_futs.push(fut.boxed());
                    tree_digests.push(tree_digest);
                }
                DirectoryEntry::Leaf(
                    ActionDirectoryMember::Symlink(..) | ActionDirectoryMember::ExternalSymlink(..),
                ) => {
                    // Bail, there is something that is not a file here and we don't handle this.
                    // This will happen if the value is a symlink. The primary output of a command
                    // being a symlink is probably unlikely. Unfortunately, we can't represent this
                    // in RE's action output, so we either have to lie about the output and pretend
                    // it's a file, or bail.
                    return Ok(Err(CacheUploadRejectionReason::SymlinkOutput));
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
                execution_start_timestamp: systemtime_to_ttimestamp(
                    result.report.timing.start_time,
                )?,
                execution_completed_timestamp: systemtime_to_ttimestamp(
                    result.report.timing.end_time(),
                )?,
                execution_attempts: 1,
                auxiliary_metadata: metadata,
                ..Default::default()
            },
            ..Default::default()
        };

        Ok(Ok(result))
    }
}

/// Whether we completed a cache upload.
#[derive(Clone, Debug)]
enum CacheUploadOutcome {
    Success,
    Rejected(CacheUploadRejectionReason),
}

/// A reason why we chose not to upload.
#[derive(Clone, Debug, Display)]
enum CacheUploadRejectionReason {
    #[display(fmt = "SymlinkOutput")]
    SymlinkOutput,
    #[display(fmt = "OutputExceedsLimit({})", max_bytes)]
    OutputExceedsLimit { max_bytes: u64 },
    #[display(fmt = "PermissionDenied (permission check error: {})", _0)]
    PermissionDenied(String),
}

#[async_trait]
impl UploadCache for CacheUploader {
    async fn upload(
        &self,
        info: &CacheUploadInfo<'_>,
        res: &CommandExecutionResult,
        dep_file_entry: Option<DepFileEntry>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
    ) -> anyhow::Result<CacheUploadResult> {
        let error_on_cache_upload = error_on_cache_unload().context("cache_upload")?;

        let did_cache_upload = if res.was_locally_executed() {
            // TODO(bobyf, torozco) should these be critical sections?
            self.upload_action_result(info, action_digest_and_blobs, res, error_on_cache_upload)
                .await?
                == CacheUploadSuccessful::Yes
        } else {
            tracing::info!(
                "Cache upload for `{}` not attempted",
                action_digest_and_blobs.action
            );
            false
        };

        // Cache upload should only invoked for successful actions only. Double check here.
        let did_dep_file_cache_upload = match dep_file_entry {
            Some(dep_file_entry) if res.was_success() => {
                self.upload_dep_file_result(
                    info,
                    action_digest_and_blobs,
                    res,
                    dep_file_entry,
                    error_on_cache_upload,
                )
                .await?
                    == CacheUploadSuccessful::Yes
            }
            _ => {
                tracing::info!(
                    "Dep file cache upload for `{}` not attempted",
                    action_digest_and_blobs.action
                );
                false
            }
        };

        Ok(CacheUploadResult {
            did_cache_upload,
            did_dep_file_cache_upload,
        })
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
