/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

pub use re_client_lib::{
    create_default_config, ActionHistoryInfo, ActionResultRequest, ActionResultResponse,
    CASDaemonClientCfg, CopyPolicy, DownloadRequest, EmbeddedCASDaemonClientCfg, ExecuteRequest,
    ExecuteResponse, ExecuteWithProgressResponse, FindMissingBlobsRequest, GetDigestsTtlRequest,
    HostResourceRequirements, InlinedBlobWithDigest, NamedDigest, NamedDigestWithPermissions,
    NetworkStatisticsResponse, Path, REClient, REClientBuilder, REClientError, REError,
    RemoteExecutionMetadata, Stage, TActionResult2, TCode, TDigest, TDirectory2,
    TExecutedActionMetadata, TExecutionPolicy, TFile, TPlatform, TProperty, TResultsCachePolicy,
    TTimestamp, UploadRequest, ZdbRichClientMode,
};
pub use re_grpc::remote_execution::{
    command::EnvironmentVariable, platform::Property, Action, Command, Digest, Directory,
    DirectoryNode, FileNode, Platform, SymlinkNode, Tree,
};
