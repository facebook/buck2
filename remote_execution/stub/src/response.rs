/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::digest::*;
use crate::error::*;

#[derive(Clone, Default)]
pub struct TTimestamp {
    pub seconds: i64,
    pub nanos: i32,
}

impl TTimestamp {
    /// Returns the amount of time elapsed from another timestamp to this one,
    /// or zero duration if that timestamp is later than this one.
    pub fn saturating_duration_since(&self, earlier: &TTimestamp) -> std::time::Duration {
        let mut seconds = self.seconds - earlier.seconds;
        let mut nanos = self.nanos - earlier.nanos;
        if seconds < 0 && nanos > 0 {
            seconds += 1;
            nanos -= 1000000000;
        } else if seconds > 0 && nanos < 0 {
            seconds -= 1;
            nanos += 1000000000;
        }
        std::time::Duration::new(
            seconds.try_into().unwrap_or_default(),
            nanos.try_into().unwrap_or_default(),
        )
    }

    pub fn unix_epoch() -> Self {
        Self {
            seconds: 0,
            nanos: 0,
        }
    }
}

#[derive(Clone, Default)]
pub struct TExecutedActionMetadata {
    pub worker: String,
    pub queued_timestamp: TTimestamp,
    pub worker_start_timestamp: TTimestamp,
    pub worker_completed_timestamp: TTimestamp,
    pub input_fetch_start_timestamp: TTimestamp,
    pub input_fetch_completed_timestamp: TTimestamp,
    pub execution_start_timestamp: TTimestamp,
    pub execution_completed_timestamp: TTimestamp,
    pub output_upload_start_timestamp: TTimestamp,
    pub output_upload_completed_timestamp: TTimestamp,
    pub execution_dir: String,
    pub input_analyzing_start_timestamp: TTimestamp,
    pub input_analyzing_completed_timestamp: TTimestamp,
    pub execution_attempts: i32,
    pub last_queued_timestamp: TTimestamp,
}

#[derive(Clone, Default)]
pub struct TActionResult2 {
    pub output_files: Vec<TFile>,
    pub output_directories: Vec<TDirectory2>,
    pub exit_code: i32,
    pub stdout_raw: Option<Vec<u8>>,
    pub stdout_digest: Option<TDigest>,
    pub stderr_raw: Option<Vec<u8>>,
    pub stderr_digest: Option<TDigest>,
    pub execution_metadata: TExecutedActionMetadata,
}

#[derive(Clone, Default)]
pub struct ActionResultResponse {
    pub action_result: TActionResult2,
    pub ttl: i64,
}

#[derive(Clone, Default)]
pub struct WriteActionResultResponse {}

#[derive(Clone, Default)]
pub struct DownloadResponse {
    pub inlined_blobs: Option<Vec<InlinedDigestWithStatus>>,
    pub directories: Option<Vec<DigestWithStatus>>,
}

#[derive(Clone, Default)]
pub struct InlinedDigestWithStatus {
    pub digest: TDigest,
    pub status: TStatus,
    pub blob: Vec<u8>,
}

#[derive(Clone, Default)]
pub struct DigestWithStatus {
    pub digest: TDigest,
    pub status: TStatus,
}

#[derive(Clone, Default)]
pub struct TStatus {
    pub code: TCode,
    pub message: String,
}

#[derive(Clone, Default)]
pub struct FindMissingBlobsResponse {
    pub missing_digests: Vec<TDigest>,
}

#[derive(Clone, Default)]
pub struct DigestWithTtl {
    pub digest: TDigest,
    pub ttl: i64,
}

#[derive(Clone, Default)]
pub struct GetDigestsTtlResponse {
    pub digests_with_ttl: Vec<DigestWithTtl>,
}

#[derive(Clone, Default)]
pub struct ExecuteResponse {
    pub action_result: TActionResult2,
    pub error: REError,
    pub cached_result: bool,
    pub action_digest: TDigest,
    pub action_result_digest: TDigest,
    pub action_result_ttl: i64,
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Default)]
pub struct Stage(pub i32);

impl Stage {
    pub const UNKNOWN: Self = Stage(0i32);
    pub const CACHE_CHECK: Self = Stage(1i32);
    pub const QUEUED: Self = Stage(2i32);
    pub const EXECUTING: Self = Stage(3i32);
    pub const COMPLETED: Self = Stage(4i32);
    pub const MATERIALIZING_INPUT: Self = Stage(100i32);
    pub const UPLOADING_OUTPUT: Self = Stage(101i32);
    pub const KEEP_ALIVE: Self = Stage(102i32);
}

#[derive(Clone, Default)]
pub struct ExecuteWithProgressResponse {
    pub stage: Stage,
    pub execute_response: Option<ExecuteResponse>,
}

#[derive(Clone, Default)]
pub struct UploadResponse {}

#[derive(Clone, Default)]
pub struct TDirectory2 {
    pub path: String,
    pub tree_digest: TDigest,
    pub root_directory_digest: TDigest,
}

#[derive(Clone, Default)]
pub struct TFile {
    pub digest: DigestWithStatus,
    pub name: String,
    pub existed: bool,
    pub executable: bool,
    pub ttl: i64,
}

#[derive(Clone, Default)]
pub struct NetworkStatisticsResponse {
    pub uploaded: i64,
    pub downloaded: i64,
    // Compatibility with the Thrift structs
    pub _dot_dot_default: (),
}
