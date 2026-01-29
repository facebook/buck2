/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::sync::Arc;
use std::time::SystemTime;

use async_trait::async_trait;
use buck2_action_metadata_proto::REMOTE_DEP_FILE_KEY;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::buck2_env;
use buck2_core::execution_types::executor_config::RePlatformFields;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::span_async;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::directory_to_re_tree;
use buck2_execute::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::cache_uploader::CacheUploadInfo;
use buck2_execute::execute::cache_uploader::CacheUploadResult;
use buck2_execute::execute::cache_uploader::IntoRemoteDepFile;
use buck2_execute::execute::cache_uploader::UploadCache;
use buck2_execute::execute::result::CommandExecutionResult;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::client::ActionCacheWriteType;
use buck2_execute::re::error::RemoteExecutionError;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use derive_more::Display;
use dupe::Dupe;
use futures::future;
use futures::future::FutureExt;
use gazebo::prelude::VecExt;
use prost::Message;
use remote_execution::DigestWithStatus;
use remote_execution::NamedDigest;
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
fn error_on_cache_upload() -> buck2_error::Result<bool> {
    buck2_env!(
        "BUCK2_TEST_ERROR_ON_CACHE_UPLOAD",
        bool,
        applicability = testing
    )
}

/// A PreparedCommandExecutor that will write to cache after invoking the inner executor
pub struct CacheUploader {
    artifact_fs: ArtifactFs,
    materializer: Arc<dyn Materializer>,
    re_client: ManagedRemoteExecutionClient,
    platform: RePlatformFields,
    max_bytes: Option<u64>,
    cache_upload_permission_checker: Arc<ActionCacheUploadPermissionChecker>,
    deduplicate_get_digests_ttl_calls: bool,
}

impl CacheUploader {
    pub fn new(
        artifact_fs: ArtifactFs,
        materializer: Arc<dyn Materializer>,
        re_client: ManagedRemoteExecutionClient,
        platform: RePlatformFields,
        max_bytes: Option<u64>,
        cache_upload_permission_checker: Arc<ActionCacheUploadPermissionChecker>,
        deduplicate_get_digests_ttl_calls: bool,
    ) -> CacheUploader {
        CacheUploader {
            artifact_fs,
            materializer,
            re_client,
            platform,
            max_bytes,
            cache_upload_permission_checker,
            deduplicate_get_digests_ttl_calls,
        }
    }

    /// Upload an action result to the RE action cache, assuming conditions for the upload are met:
    /// the action must have been successful and must have run locally (not much point in caching
    /// something that ran on RE and is already cached), and cache uploads must be enabled for this particular action.
    /// The CacheUploader should only be used if cache uploads are enabled.
    async fn upload_local_outputs(
        &self,
        info: &CacheUploadInfo<'_>,
        result: &CommandExecutionResult,
        action_digest_and_blobs: &ActionDigestAndBlobs,
        error_on_cache_upload: bool,
        has_depfile_entry: bool,
    ) -> buck2_error::Result<CacheUploadOutcome> {
        let digest = action_digest_and_blobs.action;
        let digest_str = digest.to_string();
        let output_bytes = result.calc_output_size_bytes();

        span_async(
            buck2_data::CacheUploadStart {
                key: Some(info.target.as_proto_action_key()),
                name: Some(info.target.as_proto_action_name()),
                action_digest: digest_str.clone(),
            },
            async {
                let mut file_digests = Vec::new();
                let mut tree_digests = Vec::new();

                let outcome = async {
                    if let Some(max_bytes) = self.max_bytes {
                        if output_bytes > max_bytes {
                            return Ok(CacheUploadOutcome::Rejected(
                                CacheUploadRejectionReason::OutputExceedsLimit { max_bytes },
                            ));
                        }
                    }

                    if let Err(rejected) = self.check_upload_permission(info).await? {
                        return Ok(rejected);
                    }

                    // upload Action to CAS.
                    // This is necessary when writing to the ActionCache through CAS, since CAS needs to inspect the Action related to the ActionResult.
                    // Without storing the Action itself to CAS, ActionCache writes would fail.
                    self.re_client
                        .upload_files_and_directories(
                            vec![],
                            vec![],
                            action_digest_and_blobs.blobs.to_inlined_blobs(),
                        )
                        .await?;

                    // upload ActionResult to ActionCache
                    let result: TActionResult2 = match self
                        .upload_files_and_directories(
                            result,
                            &mut file_digests,
                            &mut tree_digests,
                            info.digest_config,
                        )
                        .await?
                    {
                        Err(rejection) => {
                            return Ok(CacheUploadOutcome::Rejected(rejection));
                        }
                        Ok(taction2) => taction2,
                    };
                    // Skip expensive clone if it's not needed
                    let result_for_dep_file = if has_depfile_entry {
                        Some(result.clone())
                    } else {
                        None
                    };

                    self.re_client
                        .write_action_result(
                            digest,
                            result,
                            &self.platform.to_re_platform(),
                            ActionCacheWriteType::LocalCacheUpload,
                        )
                        .await?;

                    Ok(CacheUploadOutcome::Success(result_for_dep_file))
                }
                .await
                .map_err(|e: buck2_error::Error| buck2_error::Error::from(e))
                .unwrap_or_else(CacheUploadOutcome::Failed);

                let cache_upload_end_event = buck2_data::CacheUploadEnd {
                    key: Some(info.target.as_proto_action_key()),
                    name: Some(info.target.as_proto_action_name()),
                    action_digest: digest_str.clone(),
                    success: outcome.uploaded(),
                    error: outcome.error(),
                    re_error_code: outcome.re_error_code(),
                    file_digests: file_digests.into_map(|d| d.to_string()),
                    tree_digests: tree_digests.into_map(|d| d.to_string()),
                    output_bytes: Some(output_bytes),
                };
                (
                    outcome.log_and_create_result(&digest_str, error_on_cache_upload),
                    Box::new(cache_upload_end_event),
                )
            },
        )
        .await
    }

    /// Upload an action result with additional information about dep files to the RE action cache.
    /// The conditions for the upload are: the action must have been successful and produced a depfile
    /// and cache uploads must have been enabled for this action.
    async fn upload_dep_file(
        &self,
        info: &CacheUploadInfo<'_>,
        result: &CommandExecutionResult,
        action_result: Option<TActionResult2>,
        dep_file_bundle: &mut dyn IntoRemoteDepFile,
        remote_dep_file_action: &ActionDigestAndBlobs,
        error_on_cache_upload: bool,
    ) -> buck2_error::Result<CacheUploadOutcome> {
        let remote_dep_file_key = remote_dep_file_action.action.to_string();
        span_async(
            buck2_data::DepFileUploadStart {
                key: Some(info.target.as_proto_action_key()),
                name: Some(info.target.as_proto_action_name()),
                remote_dep_file_key: remote_dep_file_key.clone(),
            },
            async {
                let outcome = async {
                    let mut action_result = action_result.ok_or(
                        DepFileReActionResultMissingError(remote_dep_file_key.clone()),
                    )?;

                    if let Err(rejected) = self.check_upload_permission(info).await? {
                        return Ok(rejected);
                    }
                    let remote_dep_file = dep_file_bundle
                        .make_remote_dep_file(
                            info.digest_config,
                            &self.artifact_fs,
                            self.materializer.as_ref(),
                            result,
                        )
                        .await?
                        .ok_or_else(|| {
                            DepFileUploadNoDeclaredDepFiles(remote_dep_file_key.clone())
                        })?;

                    let digest = remote_dep_file_action.action;
                    let dep_file_tany = TAny {
                        type_url: REMOTE_DEP_FILE_KEY.to_owned(),
                        value: remote_dep_file.encode_to_vec(),
                        ..Default::default()
                    };
                    action_result.execution_metadata.auxiliary_metadata = vec![dep_file_tany];

                    // upload Action to CAS.
                    // This is necessary when writing to the ActionCache through CAS, since CAS needs to inspect the Action related to the ActionResult.
                    // Without storing the Action itself to CAS, ActionCache writes would fail.
                    self.re_client
                        .upload_files_and_directories(
                            vec![],
                            vec![],
                            remote_dep_file_action.blobs.to_inlined_blobs(),
                        )
                        .await?;

                    // upload ActionResult to ActionCache
                    self.re_client
                        .write_action_result(
                            digest,
                            action_result,
                            &self.platform.to_re_platform(),
                            ActionCacheWriteType::RemoteDepFile,
                        )
                        .await?;

                    Ok(CacheUploadOutcome::Success(None))
                }
                .await
                .unwrap_or_else(CacheUploadOutcome::Failed);

                let end_event = buck2_data::DepFileUploadEnd {
                    key: Some(info.target.as_proto_action_key()),
                    name: Some(info.target.as_proto_action_name()),
                    remote_dep_file_key: remote_dep_file_key.clone(),
                    success: outcome.uploaded(),
                    error: outcome.error(),
                    re_error_code: outcome.re_error_code(),
                };
                (
                    outcome.log_and_create_result(&remote_dep_file_key, error_on_cache_upload),
                    end_event,
                )
            },
        )
        .await
    }

    async fn check_upload_permission(
        &self,
        info: &CacheUploadInfo<'_>,
    ) -> buck2_error::Result<Result<(), CacheUploadOutcome>> {
        let outcome = if let Err(reason) = self
            .cache_upload_permission_checker
            .has_permission_to_upload_to_cache(&self.re_client, &self.platform, info.digest_config)
            .await?
        {
            Err(CacheUploadOutcome::Rejected(
                CacheUploadRejectionReason::PermissionDenied(reason),
            ))
        } else {
            Ok(())
        };
        Ok(outcome)
    }

    async fn upload_files_and_directories(
        &self,
        result: &CommandExecutionResult,
        file_digests: &mut Vec<TrackedFileDigest>,
        tree_digests: &mut Vec<TrackedFileDigest>,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<Result<TActionResult2, CacheUploadRejectionReason>> {
        let mut upload_futs = vec![];
        let mut output_files: Vec<TFile> = Vec::new();
        let mut output_directories: Vec<TDirectory2> = Vec::new();

        for output_result in result.resolve_outputs(&self.artifact_fs) {
            let (output, value) = output_result?;
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

                    let identity = None; // TODO(#503): implement this
                    let fut = async move {
                        self.re_client
                            .upload(
                                self.artifact_fs.fs(),
                                &self.materializer,
                                &action_blobs,
                                output.path(),
                                &d.dupe().as_immutable(),
                                identity,
                                digest_config,
                                self.deduplicate_get_digests_ttl_calls,
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
            buck2_util::future::try_join_all(upload_futs)
                .await
                .buck_error_context("Error uploading outputs")?;

            Ok(())
        };

        let std_streams = async {
            result
                .report
                .std_streams
                .clone()
                .into_re(&self.re_client, digest_config)
                .await
                .buck_error_context("Error accessing std_streams")
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
                ..Default::default()
            },
            ..Default::default()
        };

        Ok(Ok(result))
    }
}

/// Whether we completed a cache upload.
#[allow(clippy::large_enum_variant)]
enum CacheUploadOutcome {
    Success(Option<TActionResult2>),
    Rejected(CacheUploadRejectionReason),
    Failed(buck2_error::Error),
}

impl CacheUploadOutcome {
    fn uploaded(&self) -> bool {
        matches!(self, CacheUploadOutcome::Success(_))
    }

    fn error(&self) -> String {
        match self {
            CacheUploadOutcome::Success(_) => String::new(),
            CacheUploadOutcome::Rejected(reason) => format!("Rejected: {reason}"),
            CacheUploadOutcome::Failed(e) => format!("{e:#}"),
        }
    }

    fn re_error_code(&self) -> Option<String> {
        match self {
            CacheUploadOutcome::Success(_) => None,
            CacheUploadOutcome::Rejected(reason) => match reason {
                CacheUploadRejectionReason::SymlinkOutput
                | CacheUploadRejectionReason::OutputExceedsLimit { .. } => None,
                CacheUploadRejectionReason::PermissionDenied(_) => {
                    Some(TCode::PERMISSION_DENIED.to_string())
                }
            },
            CacheUploadOutcome::Failed(e) => match e.find_typed_context::<RemoteExecutionError>() {
                Some(e) => Some(e.code.to_string()),
                _ => Some("OTHER_ERRORS".to_owned()),
            },
        }
    }

    fn log_and_create_result(
        self,
        digest_str: &String,
        error_on_cache_upload: bool,
    ) -> buck2_error::Result<CacheUploadOutcome> {
        match &self {
            CacheUploadOutcome::Success(_) => {
                tracing::info!("Cache upload for `{}` succeeded", digest_str);
            }
            CacheUploadOutcome::Rejected(reason) => {
                tracing::info!("Cache upload for `{}` rejected: {:#}", digest_str, reason);
            }
            CacheUploadOutcome::Failed(e) => {
                tracing::warn!("Cache upload for `{}` failed: {:#}", digest_str, e);
            }
        };
        if !self.uploaded() && error_on_cache_upload {
            Err(buck2_error::buck2_error!(
                buck2_error::ErrorTag::CacheUploadFailed,
                "cache_upload_failed"
            ))
        } else {
            Ok(self)
        }
    }
}

/// A reason why we chose not to upload.
#[derive(Clone, Debug, Display)]
enum CacheUploadRejectionReason {
    #[display("SymlinkOutput")]
    SymlinkOutput,
    #[display("OutputExceedsLimit({})", max_bytes)]
    OutputExceedsLimit { max_bytes: u64 },
    #[display("PermissionDenied (permission check error: {})", _0)]
    PermissionDenied(String),
}

#[derive(Debug, buck2_error::Error)]
#[error("Missing action result for dep file key `{0}`")]
#[buck2(tag = Tier0)]
struct DepFileReActionResultMissingError(String);

#[derive(Debug, buck2_error::Error)]
#[error("No dep files were declared, nothing to upload for dep file key `{0}`")]
#[buck2(tag = Input)]
struct DepFileUploadNoDeclaredDepFiles(String);

#[async_trait]
impl UploadCache for CacheUploader {
    async fn upload(
        &self,
        info: &CacheUploadInfo<'_>,
        res: &CommandExecutionResult,
        re_result: Option<TActionResult2>,
        dep_file_bundle: Option<&mut dyn IntoRemoteDepFile>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
    ) -> buck2_error::Result<CacheUploadResult> {
        let error_on_cache_upload = error_on_cache_upload().buck_error_context("cache_upload")?;

        let (did_cache_upload, action_result) = if res.was_locally_executed() {
            tracing::debug!(
                "Uploading action result for `{}`",
                action_digest_and_blobs.action
            );
            // TODO(bobyf, torozco) should these be critical sections?
            let outcome = self
                .upload_local_outputs(
                    info,
                    res,
                    &action_digest_and_blobs,
                    error_on_cache_upload,
                    dep_file_bundle.is_some(),
                )
                .await?;

            (
                outcome.uploaded(),
                if let CacheUploadOutcome::Success(action_result) = outcome {
                    action_result
                } else {
                    None
                },
            )
        } else if dep_file_bundle.is_some() {
            (false, re_result)
        } else {
            tracing::info!(
                "Cache upload for `{}` not attempted",
                action_digest_and_blobs.action
            );
            (false, None)
        };

        let should_upload_dep_file =
            res.was_locally_executed() || res.was_remotely_executed() || res.was_action_cache_hit();

        let (did_dep_file_cache_upload, dep_file_cache_upload_key) = if let Some(dep_file_bundle) =
            dep_file_bundle
            && should_upload_dep_file
        {
            let remote_dep_file_action = dep_file_bundle.remote_dep_file_action(
                info.digest_config,
                info.mergebase,
                info.re_platform,
            );
            (
                self.upload_dep_file(
                    info,
                    res,
                    action_result,
                    dep_file_bundle,
                    &remote_dep_file_action,
                    error_on_cache_upload,
                )
                .await?
                .uploaded(),
                Some(remote_dep_file_action.action.coerce()),
            )
        } else {
            tracing::info!(
                "Dep file cache upload for `{}` not attempted",
                action_digest_and_blobs.action
            );
            (false, None)
        };

        Ok(CacheUploadResult {
            did_cache_upload,
            did_dep_file_cache_upload,
            dep_file_cache_upload_key,
        })
    }
}

fn systemtime_to_ttimestamp(time: SystemTime) -> buck2_error::Result<TTimestamp> {
    let duration = time.duration_since(SystemTime::UNIX_EPOCH)?;
    Ok(TTimestamp {
        seconds: duration
            .as_secs()
            .try_into()
            .buck_error_context("Invalid duration")?,
        // Max 1B so it won't wrap around.
        nanos: duration.subsec_nanos() as _,
        ..Default::default()
    })
}
