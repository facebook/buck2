/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(unused_variables)] // Because a lot of these are stubbed out

use std::pin::Pin;

use futures::Stream;

use crate::{config::*, metadata::*, request::*, response::*};

pub struct REClientBuilder {}

pub(crate) fn stub() -> ! {
    unimplemented!("Not implemented in remote execution stub")
}

impl REClientBuilder {
    pub fn new<T>(fb_init: T) -> Self {
        stub()
    }

    pub fn build(self) -> REClient {
        stub()
    }

    pub fn with_config(self, thrift_cfg: ClientCfg) -> Self {
        stub()
    }

    pub fn with_logger<T>(self, logger: T) -> Self {
        stub()
    }
}

pub struct REClient {}

impl Drop for REClient {
    fn drop(&mut self) {
        // Important we have a drop implementation since the real one does, and we
        // don't want errors coming from the stub not having one
    }
}

impl REClient {
    pub fn new<A, B>(fb_init: A, logger: Option<B>) -> Self {
        stub()
    }

    pub async fn get_action_result(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ActionResultRequest,
    ) -> anyhow::Result<ActionResultResponse> {
        stub()
    }

    pub async fn execute(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ExecuteRequest,
    ) -> anyhow::Result<ExecuteResponse> {
        stub()
    }

    pub async fn execute_with_progress(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ExecuteRequest,
    ) -> anyhow::Result<
        Pin<Box<dyn Stream<Item = anyhow::Result<ExecuteWithProgressResponse>> + Send>>,
    > {
        stub()
    }

    pub async fn upload(
        &self,
        metadata: RemoteExecutionMetadata,
        request: UploadRequest,
    ) -> anyhow::Result<UploadResponse> {
        stub()
    }

    pub async fn download(
        &self,
        metadata: RemoteExecutionMetadata,
        request: DownloadRequest,
    ) -> anyhow::Result<DownloadResponse> {
        stub()
    }

    pub async fn find_missing_blobs(
        &self,
        metadata: RemoteExecutionMetadata,
        request: FindMissingBlobsRequest,
    ) -> anyhow::Result<FindMissingBlobsResponse> {
        stub()
    }

    pub async fn get_digests_ttl(
        &self,
        metadata: RemoteExecutionMetadata,
        request: GetDigestsTtlRequest,
    ) -> anyhow::Result<GetDigestsTtlResponse> {
        stub()
    }

    pub fn get_execution_client(&self) -> &Self {
        self
    }

    pub fn get_cas_client(&self) -> &Self {
        self
    }

    pub fn get_action_cache_client(&self) -> &Self {
        self
    }

    pub fn get_metrics_client(&self) -> &Self {
        self
    }

    pub fn get_session_id(&self) -> &str {
        stub()
    }

    pub fn get_network_stats(&self) -> anyhow::Result<NetworkStatisticsResponse> {
        stub()
    }
}
