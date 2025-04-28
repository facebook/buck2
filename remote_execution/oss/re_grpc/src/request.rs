/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub use crate::digest::*;
use crate::grpc::Platform as TPlatform;
use crate::response::TActionResult2;

#[derive(Default)]
pub struct ActionResultRequest {
    pub digest: TDigest,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct DownloadRequest {
    pub inlined_digests: Option<Vec<TDigest>>,
    pub file_digests: Option<Vec<NamedDigestWithPermissions>>,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct NamedDigestWithPermissions {
    pub named_digest: NamedDigest,
    pub is_executable: bool,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct NamedDigest {
    pub name: String,
    pub digest: TDigest,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct UploadRequest {
    pub files_with_digest: Option<Vec<NamedDigest>>,
    pub inlined_blobs_with_digest: Option<Vec<InlinedBlobWithDigest>>,
    pub directories: Option<Vec<Path>>,
    pub upload_only_missing: bool,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct Path {
    pub path: String,
    pub follow_symlinks: bool,
    pub digest: Option<TDigest>,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct InlinedBlobWithDigest {
    pub blob: Vec<u8>,
    pub digest: TDigest,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct FindMissingBlobsRequest {
    pub digests: Vec<TDigest>,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct GetDigestsTtlRequest {
    pub digests: Vec<TDigest>,
    pub _dot_dot: (),
}

#[derive(Default)]
pub struct ExtendDigestsTtlRequest {
    pub digests: Vec<TDigest>,
    pub ttl: i64,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct ExecuteRequest {
    pub action_digest: TDigest,
    pub skip_cache_lookup: bool,
    pub execution_policy: Option<TExecutionPolicy>,
    pub host_runtime_requirements: THostRuntimeRequirements,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct TDependency {
    pub smc_tier: String,
    pub id: String,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct TExecutionPolicy {
    pub priority: i32,
    pub affinity_keys: Vec<String>,
    pub region_preference: String,
    pub setup_preference_key: String,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct THostResourceRequirements {
    pub mem_bytes: i64,
    pub cpu_units: i64,
    pub input_files_bytes: i64,
    pub resource_units: i64,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct THostRuntimeRequirements {
    pub platform: TPlatform,
    pub host_resource_requirements: THostResourceRequirements,
    pub dependencies: Vec<TDependency>,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct WriteActionResultRequest {
    pub action_digest: TDigest,
    pub action_result: TActionResult2,
    pub _dot_dot: (),
}
