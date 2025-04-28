/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_action_metadata_proto::RemoteDepFile;
use buck2_core::buck2_env;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use remote_execution::TActionResult2;

use crate::digest_config::DigestConfig;
use crate::execute::action_digest_and_blobs::ActionDigestAndBlobs;
use crate::execute::result::CommandExecutionResult;
use crate::execute::target::CommandExecutionTarget;
use crate::materialize::materializer::Materializer;

pub struct CacheUploadInfo<'a> {
    pub target: &'a dyn CommandExecutionTarget,
    pub digest_config: DigestConfig,
}

#[async_trait]
pub trait IntoRemoteDepFile: Send {
    fn remote_dep_file_action(&self) -> &ActionDigestAndBlobs;

    async fn make_remote_dep_file(
        &mut self,
        digest_config: DigestConfig,
        fs: &ArtifactFs,
        materializer: &dyn Materializer,
    ) -> buck2_error::Result<RemoteDepFile>;
}

pub struct CacheUploadResult {
    pub did_cache_upload: bool,
    pub did_dep_file_cache_upload: bool,
}

// This is for quick testing of cache upload without configuring executors.
pub fn force_cache_upload() -> buck2_error::Result<bool> {
    buck2_env!(
        "BUCK2_TEST_FORCE_CACHE_UPLOAD",
        bool,
        applicability = testing
    )
}

/// A single purpose trait to handle cache uploads
#[async_trait]
pub trait UploadCache: Send + Sync {
    /// Given information about the command and its result, upload the result
    /// and related items to the cache.
    /// Return a boolean to indicate whether a cache upload took place.
    async fn upload(
        &self,
        info: &CacheUploadInfo<'_>,
        execution_result: &CommandExecutionResult,
        re_result: Option<TActionResult2>,
        dep_file_bundle: Option<&mut dyn IntoRemoteDepFile>,
        action_digest_and_blobs: &ActionDigestAndBlobs,
    ) -> buck2_error::Result<CacheUploadResult>;
}

/// A no-op cache uploader for when cache uploading is disabled
pub struct NoOpCacheUploader {}

#[async_trait]
impl UploadCache for NoOpCacheUploader {
    async fn upload(
        &self,
        _info: &CacheUploadInfo<'_>,
        _execution_result: &CommandExecutionResult,
        _re_result: Option<TActionResult2>,
        _dep_file_bundle: Option<&mut dyn IntoRemoteDepFile>,
        _action_digest_and_blobs: &ActionDigestAndBlobs,
    ) -> buck2_error::Result<CacheUploadResult> {
        Ok(CacheUploadResult {
            did_cache_upload: false,
            did_dep_file_cache_upload: false,
        })
    }
}
