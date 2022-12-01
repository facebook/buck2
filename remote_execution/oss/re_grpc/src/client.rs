/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(unused_variables)] // Because a lot of these are stubbed out
use std::future;
use std::pin::Pin;
use std::sync::Mutex;

use futures::lock::Mutex as AMutex;
use futures::stream;
use futures::Stream;
use gazebo::prelude::*;
use re_grpc_proto::build::bazel::remote::execution::v2::content_addressable_storage_client::ContentAddressableStorageClient;
use re_grpc_proto::build::bazel::remote::execution::v2::execution_client::ExecutionClient;
use re_grpc_proto::build::bazel::remote::execution::v2::Digest;
use slog::*;
use tonic::transport::Channel;

use crate::config::*;
use crate::error::*;
use crate::metadata::*;
use crate::request::*;
use crate::response::*;

// TODO(aloiscochard): take address, instance_name from settings
const ADDRESS: &str = "grpc://localhost:8980";
const INSTANCE_NAME: &str = "";

#[derive(Default)]
pub struct REClientBuilder {
    logger: Option<slog::Logger>,
    cfg: Option<ClientCfg>,
}

fn tdigest_to(tdigest: TDigest) -> Digest {
    Digest {
        hash: tdigest.hash,
        size_bytes: tdigest.size_in_bytes,
    }
}

fn tdigest_from(digest: Digest) -> TDigest {
    TDigest {
        hash: digest.hash,
        size_in_bytes: digest.size_bytes,
        ..Default::default()
    }
}

fn tstatus_ok() -> TStatus {
    TStatus {
        code: TCode::OK,
        message: "".to_owned(),
        ..Default::default()
    }
}

fn ttimestamp_from(ts: Option<::prost_types::Timestamp>) -> TTimestamp {
    match ts {
        Some(timestamp) => TTimestamp {
            seconds: timestamp.seconds,
            nanos: timestamp.nanos,
            ..Default::default()
        },
        None => TTimestamp::unix_epoch(),
    }
}

pub(crate) fn stub(msg: &str) -> ! {
    unimplemented!("Not implemented: {:?}", msg)
}

impl REClientBuilder {
    pub fn new<T>(_fb_init: T) -> Self {
        REClientBuilder::default()
    }

    pub fn build(self) -> REClient {
        let cfg = self.cfg.unwrap_or_default();
        // TODO(aloiscochard) we ignore self.logger as it's discarded
        let logger = {
            /*
            let log_path = "buck-out/v2/re_logs/grpc.log";
            let file = OpenOptions::new()
                .create(true)
                .write(true)
                .truncate(true)
                .open(log_path)
                .unwrap();

            let decorator = slog_term::PlainDecorator::new(file);
            let drain = slog_term::FullFormat::new(decorator).build().fuse();
            slog_async::Async::new(drain).build().fuse();
            */
            let drain = slog::Discard;

            slog::Logger::root(drain, o!())
        };
        REClient::new(cfg, logger)
    }

    pub fn with_config(mut self, cfg: ClientCfg) -> Self {
        self.cfg = Some(cfg);
        self
    }

    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = Some(logger);
        self
    }
}

#[derive(Default)]
pub struct GRPCClients {
    cas_client: Option<ContentAddressableStorageClient<Channel>>,
    execution_client: Option<ExecutionClient<Channel>>,
}

impl GRPCClients {
    // TODO(aloiscochard): Avoid code duplication with handling of different clients
    async fn cas_client(
        &mut self,
    ) -> anyhow::Result<&mut ContentAddressableStorageClient<Channel>> {
        if self.cas_client.is_none() {
            let client = ContentAddressableStorageClient::connect(ADDRESS).await?;
            self.cas_client = Some(client);
            self.unwrap_cas_client().await
        } else {
            self.unwrap_cas_client().await
        }
    }

    async fn unwrap_cas_client(
        &mut self,
    ) -> anyhow::Result<&mut ContentAddressableStorageClient<Channel>> {
        match &mut self.cas_client {
            Some(client) => Ok(client),
            None => Err(anyhow::anyhow!("Client not found")),
        }
    }

    async fn execution_client(&mut self) -> anyhow::Result<&mut ExecutionClient<Channel>> {
        if self.execution_client.is_none() {
            let client = ExecutionClient::connect(ADDRESS).await?;
            self.execution_client = Some(client);
            self.unwrap_execution_client().await
        } else {
            self.unwrap_execution_client().await
        }
    }

    async fn unwrap_execution_client(&mut self) -> anyhow::Result<&mut ExecutionClient<Channel>> {
        match &mut self.execution_client {
            Some(client) => Ok(client),
            None => Err(anyhow::anyhow!("Client not found")),
        }
    }
}

#[derive(Default)]
pub struct REState {
    // TODO(aloiscochard): Update those values
    network_uploaded: i64,   // in bytes
    network_downloaded: i64, // in bytes
}

pub struct REClient {
    logger: Logger,
    grpc_clients: AMutex<GRPCClients>,
    state: Mutex<REState>,
}

impl Drop for REClient {
    fn drop(&mut self) {
        // Important we have a drop implementation since the real one does, and we
        // don't want errors coming from the stub not having one
    }
}

impl REClient {
    pub fn new(_cfg: ClientCfg, logger: Logger) -> Self {
        REClient {
            logger,
            grpc_clients: AMutex::new(GRPCClients::default()),
            state: Mutex::new(REState::default()),
        }
    }

    pub async fn get_action_result(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ActionResultRequest,
    ) -> anyhow::Result<ActionResultResponse> {
        // TODO(aloiscochard): For now we just say no action result are found
        Err(anyhow::anyhow!("Not found: {}", request.digest))
    }

    pub async fn write_action_result(
        &self,
        metadata: RemoteExecutionMetadata,
        request: WriteActionResultRequest,
    ) -> anyhow::Result<WriteActionResultResponse> {
        // TODO(aloiscochard)
        stub("write_action_result")
    }

    pub async fn execute(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ExecuteRequest,
    ) -> anyhow::Result<ExecuteResponse> {
        // TODO(aloiscochard)
        stub("execute")
    }

    pub async fn execute_with_progress(
        &self,
        metadata: RemoteExecutionMetadata,
        execute_request: ExecuteRequest,
    ) -> anyhow::Result<
        Pin<Box<dyn Stream<Item = anyhow::Result<ExecuteWithProgressResponse>> + Send>>,
    > {
        use prost::Message;
        use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteRequest as GExecuteRequest;
        use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteResponse as GExecuteResponse;
        // TODO(aloiscochard): Map those properly in the request
        // use crate::proto::build::bazel::remote::execution::v2::ExecutionPolicy;
        use re_grpc_proto::build::bazel::remote::execution::v2::ResultsCachePolicy;
        use re_grpc_proto::google::longrunning::operation::Result as OpResult;

        let mut grpc_clients = self.grpc_clients.lock().await;
        let client = grpc_clients.execution_client().await?;

        let action_digest = tdigest_to(execute_request.action_digest.clone());
        let action_tdigest = execute_request.action_digest.clone();

        let request = GExecuteRequest {
            instance_name: INSTANCE_NAME.into(),
            skip_cache_lookup: false,
            execution_policy: None,
            results_cache_policy: Some(ResultsCachePolicy { priority: 0 }),
            action_digest: Some(action_digest.clone()),
        };

        let mut response = client.execute(request).await?;
        let stream = response.get_mut();
        let mut result = None;

        while let Some(operation) = stream.message().await? {
            if operation.done {
                result = operation.result;
            }
        }

        match result.expect("The operation's result is not defined.") {
            OpResult::Error(rpc_status) => Err(anyhow::anyhow!(
                "Unable to execute action '{:?}', rpc status code: {}, message: \"{}\"",
                action_digest,
                rpc_status.code,
                rpc_status.message
            )),
            OpResult::Response(any) => {
                let execute_response: GExecuteResponse = GExecuteResponse::decode(&any.value[..])?;
                // note: the execute_response.status field is undefined when response is successful
                let action_result = execute_response
                    .result
                    .expect("The action result is not defined.");

                let execution_metadata = action_result
                    .execution_metadata
                    .expect("The execution metadata are not defined.");

                Ok(Box::pin(stream::once(future::ready(Ok(
                    ExecuteWithProgressResponse {
                        stage: Stage::COMPLETED,
                        execute_response: Some(ExecuteResponse {
                            action_result: TActionResult2 {
                                output_files: action_result.output_files.into_map(|output_file| {
                                    TFile {
                                        digest: DigestWithStatus {
                                            status: tstatus_ok(),
                                            digest: tdigest_from(
                                                output_file.digest.expect("Digest not found."),
                                            ),
                                            _dot_dot_default: (),
                                        },
                                        name: output_file.path,
                                        // TODO(aloiscochard): avoid hardcoded value
                                        existed: false,
                                        executable: output_file.is_executable,
                                        // TODO(aloiscochard): avoid hardcoded value
                                        ttl: 0,
                                        _dot_dot_default: (),
                                    }
                                }),
                                output_directories: action_result.output_directories.into_map(
                                    |output_directory| {
                                        let digest = tdigest_from(
                                            output_directory
                                                .tree_digest
                                                .expect("Tree digest not defined."),
                                        );
                                        TDirectory2 {
                                            path: output_directory.path,
                                            tree_digest: digest.clone(),
                                            root_directory_digest: digest,
                                            _dot_dot_default: (),
                                        }
                                    },
                                ),
                                exit_code: action_result.exit_code,
                                stdout_raw: Some(action_result.stdout_raw),
                                stdout_digest: action_result.stdout_digest.map(tdigest_from),
                                stderr_raw: Some(action_result.stderr_raw),
                                stderr_digest: action_result.stderr_digest.map(tdigest_from),

                                execution_metadata: TExecutedActionMetadata {
                                    worker: execution_metadata.worker,
                                    queued_timestamp: ttimestamp_from(
                                        execution_metadata.queued_timestamp,
                                    ),
                                    worker_start_timestamp: ttimestamp_from(
                                        execution_metadata.worker_start_timestamp,
                                    ),
                                    worker_completed_timestamp: ttimestamp_from(
                                        execution_metadata.worker_completed_timestamp,
                                    ),
                                    input_fetch_start_timestamp: ttimestamp_from(
                                        execution_metadata.input_fetch_start_timestamp,
                                    ),
                                    input_fetch_completed_timestamp: ttimestamp_from(
                                        execution_metadata.input_fetch_completed_timestamp,
                                    ),
                                    execution_start_timestamp: ttimestamp_from(
                                        execution_metadata.execution_start_timestamp,
                                    ),
                                    execution_completed_timestamp: ttimestamp_from(
                                        execution_metadata.execution_completed_timestamp,
                                    ),
                                    output_upload_start_timestamp: ttimestamp_from(
                                        execution_metadata.output_upload_start_timestamp,
                                    ),
                                    output_upload_completed_timestamp: ttimestamp_from(
                                        execution_metadata.output_upload_completed_timestamp,
                                    ),
                                    input_analyzing_start_timestamp: Default::default(),
                                    input_analyzing_completed_timestamp: Default::default(),
                                    execution_dir: "".to_owned(),
                                    execution_attempts: 0,
                                    last_queued_timestamp: Default::default(),
                                    _dot_dot_default: (),
                                },
                                _dot_dot_default: (),
                            },

                            // TODO(aloiscochard): For now we pass the action_digest here
                            action_result_digest: action_tdigest.clone(),
                            // TODO(aloiscochard): avoid hardcoded value
                            action_result_ttl: 0,

                            error: REError {
                                code: TCode::OK,
                                message: execute_response.message,
                                error_location: ErrorLocation(0),
                            },
                            cached_result: execute_response.cached_result,
                            action_digest: action_tdigest.clone(),
                        }),
                    },
                )))))
            }
        }
    }

    pub async fn upload(
        &self,
        metadata: RemoteExecutionMetadata,
        request: UploadRequest,
    ) -> anyhow::Result<UploadResponse> {
        use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_request::Request;
        use re_grpc_proto::build::bazel::remote::execution::v2::compressor;
        use re_grpc_proto::build::bazel::remote::execution::v2::BatchUpdateBlobsRequest;
        use re_grpc_proto::google::rpc::Code;

        let mut grpc_clients = self.grpc_clients.lock().await;
        let client = grpc_clients.cas_client().await?;

        let re_request = BatchUpdateBlobsRequest {
            instance_name: INSTANCE_NAME.into(),
            requests: [
                request
                    .inlined_blobs_with_digest
                    .unwrap_or_default()
                    .into_map(|x| Request {
                        digest: Some(tdigest_to(x.digest)),
                        data: x.blob,
                        compressor: compressor::Value::Identity as i32,
                    }),
                request
                    .files_with_digest
                    .unwrap_or_default()
                    .into_map(|x| Request {
                        digest: Some(tdigest_to(x.digest)),
                        data: {
                            // TODO(aloiscochard) extract as function
                            use std::io::Read;
                            let mut f = std::fs::File::open(&x.name).expect("no file found");
                            let metadata =
                                std::fs::metadata(&x.name).expect("unable to read metadata");
                            let mut buffer = vec![0; metadata.len() as usize];
                            f.read_exact(&mut buffer).expect("buffer overflow");
                            buffer
                        },
                        compressor: compressor::Value::Identity as i32,
                    }),
            ]
            .concat(),
        };

        let blob_hashes = re_request
            .requests
            .iter()
            .map(|x| x.digest.as_ref().unwrap().hash.clone())
            .collect::<Vec<String>>();
        let response = client.batch_update_blobs(re_request).await?;

        let failures: Vec<String> = response
            .get_ref()
            .responses
            .iter()
            .filter_map(|r| {
                r.status.as_ref().and_then(|s| {
                    if s.code == (Code::Ok as i32) {
                        None
                    } else {
                        Some(format!(
                            "Unable to upload blob '{}', rpc status code: {}, message: \"{}\"",
                            r.digest.as_ref().map_or("N/A".into(), |d| d.hash.clone()),
                            s.code,
                            s.message
                        ))
                    }
                })
            })
            .collect();

        if failures.is_empty() {
            debug!(self.logger, "uploaded: {:?}", blob_hashes);
            // TODO(aloiscochard): Add something interesting in UploadResponse?
            Ok(UploadResponse {})
        } else {
            Err(anyhow::anyhow!("Batch upload failed: {:?}", failures))
        }
    }

    pub async fn upload_blob(
        &self,
        blob: Vec<u8>,
        metadata: RemoteExecutionMetadata,
    ) -> anyhow::Result<TDigest> {
        stub("upload_blob")
    }

    pub async fn download(
        &self,
        metadata: RemoteExecutionMetadata,
        request: DownloadRequest,
    ) -> anyhow::Result<DownloadResponse> {
        use re_grpc_proto::build::bazel::remote::execution::v2::compressor;
        use re_grpc_proto::build::bazel::remote::execution::v2::BatchReadBlobsRequest;

        let mut grpc_clients = self.grpc_clients.lock().await;
        let client = grpc_clients.cas_client().await?;

        let re_request = BatchReadBlobsRequest {
            instance_name: INSTANCE_NAME.into(),
            digests: [
                request
                    .inlined_digests
                    .unwrap_or_default()
                    .into_map(tdigest_to),
                request
                    .file_digests
                    .unwrap_or_default()
                    .into_map(|x| tdigest_to(x.named_digest.digest)),
            ]
            .concat(),
            acceptable_compressors: vec![compressor::Value::Identity as i32],
        };

        let response = client.batch_read_blobs(re_request).await?;
        let blobs: Vec<InlinedDigestWithStatus> = response
            .get_ref()
            .responses
            .iter()
            .map(|r| InlinedDigestWithStatus {
                // TODO(aloiscochard): Here we should check if r.status is ok!
                digest: tdigest_from(r.digest.clone().expect("Response digest not found.")),
                status: tstatus_ok(),
                // TODO(aloiscochard): it should be possible to avoid this costly clone here
                blob: r.data.clone(),
            })
            .collect();

        Ok(DownloadResponse {
            inlined_blobs: Some(blobs),
            directories: None,
        })
    }

    pub async fn find_missing_blobs(
        &self,
        metadata: RemoteExecutionMetadata,
        request: FindMissingBlobsRequest,
    ) -> anyhow::Result<FindMissingBlobsResponse> {
        // TODO(aloiscochard)
        stub("find_missing_blobs")
    }

    pub async fn get_digests_ttl(
        &self,
        metadata: RemoteExecutionMetadata,
        request: GetDigestsTtlRequest,
    ) -> anyhow::Result<GetDigestsTtlResponse> {
        // TODO(aloiscochard): For now we just say all digests have expired,
        // we should fetch and return real TTL instead
        Ok(GetDigestsTtlResponse {
            digests_with_ttl: request
                .digests
                .into_map(|digest| DigestWithTtl { digest, ttl: 0 }),
        })
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
        // TODO(aloiscochard): Return a unique ID, ideally from the GRPC client
        "GRPC-SESSION-ID"
    }

    pub fn get_network_stats(&self) -> anyhow::Result<NetworkStatisticsResponse> {
        // TODO(aloiscochard): Is that unwrap safe? could we avoid it by making the caller async?
        let state = self.state.lock().unwrap();
        Ok(NetworkStatisticsResponse {
            downloaded: state.network_downloaded,
            uploaded: state.network_uploaded,
            _dot_dot_default: (),
        })
    }

    pub fn get_experiment_name(&self) -> anyhow::Result<Option<String>> {
        Ok(None)
    }
}
