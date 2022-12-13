/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future;
use std::pin::Pin;
use std::sync::Mutex;

use anyhow::Context;
use buck2_core::fs::fs_util;
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

// TODO(aloiscochard): Get instance_name from settings, what key? need a new one?
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

impl REClientBuilder {
    pub fn new<T>(_fb_init: T) -> Self {
        REClientBuilder::default()
    }

    pub async fn build_and_connect(self) -> anyhow::Result<REClient> {
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

        let cfg = self.cfg.unwrap_or_default();
        let address = cfg
            .execution_client_config
            .address
            .context("Execution client address not defined")?;

        let grpc_clients = GRPCClients {
            cas_client: ContentAddressableStorageClient::connect(address.clone()).await?,
            execution_client: ExecutionClient::connect(address).await?,
        };

        Ok(REClient::new(logger, grpc_clients))
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

pub struct GRPCClients {
    cas_client: ContentAddressableStorageClient<Channel>,
    execution_client: ExecutionClient<Channel>,
}

#[derive(Default)]
pub struct REState {
    // TODO(aloiscochard): Update those values
    network_uploaded: i64,   // in bytes
    network_downloaded: i64, // in bytes
}

pub struct REClient {
    logger: Logger,
    grpc_clients: GRPCClients,
    state: Mutex<REState>,
}

impl Drop for REClient {
    fn drop(&mut self) {
        // Important we have a drop implementation since the real one does, and we
        // don't want errors coming from the stub not having one
    }
}

impl REClient {
    pub fn new(logger: Logger, grpc_clients: GRPCClients) -> Self {
        REClient {
            logger,
            grpc_clients,
            state: Mutex::new(REState::default()),
        }
    }

    pub async fn get_action_result(
        &self,
        _metadata: RemoteExecutionMetadata,
        request: ActionResultRequest,
    ) -> anyhow::Result<ActionResultResponse> {
        Err(anyhow::anyhow!("Not found: {}", request.digest))
    }

    pub async fn write_action_result(
        &self,
        _metadata: RemoteExecutionMetadata,
        _request: WriteActionResultRequest,
    ) -> anyhow::Result<WriteActionResultResponse> {
        Err(anyhow::anyhow!("Not supported"))
    }

    pub async fn execute_with_progress(
        &self,
        _metadata: RemoteExecutionMetadata,
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

        let mut client = self.grpc_clients.execution_client.clone();

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

        match result.with_context(|| "The operation's result is not defined.")? {
            OpResult::Error(rpc_status) => Err(anyhow::anyhow!(
                "Unable to execute action '{:?}', rpc status code: {}, message: \"{}\"",
                action_digest,
                rpc_status.code,
                rpc_status.message
            )),
            OpResult::Response(any) => {
                let execute_response_grpc: GExecuteResponse =
                    GExecuteResponse::decode(&any.value[..])?;
                // note: the execute_response_grpc.status field is undefined when response is successful
                let action_result = execute_response_grpc
                    .result
                    .with_context(|| "The action result is not defined.")?;

                let execution_metadata = action_result
                    .execution_metadata
                    .with_context(|| "The execution metadata are not defined.")?;

                let output_files = action_result.output_files.into_try_map(|output_file| {
                    let output_file_digest =
                        output_file.digest.with_context(|| "Digest not found.")?;

                    anyhow::Ok(TFile {
                        digest: DigestWithStatus {
                            status: tstatus_ok(),
                            digest: tdigest_from(output_file_digest),
                            _dot_dot_default: (),
                        },
                        name: output_file.path,
                        existed: false,
                        executable: output_file.is_executable,
                        ttl: 0,
                        _dot_dot_default: (),
                    })
                })?;

                let output_directories =
                    action_result
                        .output_directories
                        .into_try_map(|output_directory| {
                            let digest = tdigest_from(
                                output_directory
                                    .tree_digest
                                    .with_context(|| "Tree digest not defined.")?,
                            );
                            anyhow::Ok(TDirectory2 {
                                path: output_directory.path,
                                tree_digest: digest.clone(),
                                root_directory_digest: digest,
                                _dot_dot_default: (),
                            })
                        })?;

                let execute_response = ExecuteResponse {
                    action_result: TActionResult2 {
                        output_files,
                        output_directories,
                        exit_code: action_result.exit_code,
                        stdout_raw: Some(action_result.stdout_raw),
                        stdout_digest: action_result.stdout_digest.map(tdigest_from),
                        stderr_raw: Some(action_result.stderr_raw),
                        stderr_digest: action_result.stderr_digest.map(tdigest_from),

                        execution_metadata: TExecutedActionMetadata {
                            worker: execution_metadata.worker,
                            queued_timestamp: ttimestamp_from(execution_metadata.queued_timestamp),
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
                    action_result_digest: TDigest::default(),
                    action_result_ttl: 0,
                    error: REError {
                        code: TCode::OK,
                        message: execute_response_grpc.message,
                        error_location: ErrorLocation(0),
                    },
                    cached_result: execute_response_grpc.cached_result,
                    action_digest: action_tdigest.clone(),
                };

                Ok(Box::pin(stream::once(future::ready(Ok(
                    ExecuteWithProgressResponse {
                        stage: Stage::COMPLETED,
                        execute_response: Some(execute_response),
                    },
                )))))
            }
        }
    }

    pub async fn upload(
        &self,
        _metadata: RemoteExecutionMetadata,
        request: UploadRequest,
    ) -> anyhow::Result<UploadResponse> {
        use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_request::Request;
        use re_grpc_proto::build::bazel::remote::execution::v2::compressor;
        use re_grpc_proto::build::bazel::remote::execution::v2::BatchUpdateBlobsRequest;
        use re_grpc_proto::google::rpc::Code;

        let mut client = self.grpc_clients.cas_client.clone();

        let files_with_digest: Vec<Request> = request
            .files_with_digest
            .unwrap_or_default()
            .into_try_map(|x| {
                anyhow::Ok(Request {
                    digest: Some(tdigest_to(x.digest)),
                    // FIXME: This could do a lot of blocking reads
                    data: fs_util::read(&x.name)?,
                    compressor: compressor::Value::Identity as i32,
                })
            })?;

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
                files_with_digest,
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
                            r.digest.as_ref().map_or("N/A", |d| &d.hash),
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
        _blob: Vec<u8>,
        _metadata: RemoteExecutionMetadata,
    ) -> anyhow::Result<TDigest> {
        // TODO(aloiscochard)
        Err(anyhow::anyhow!("Not implemented (RE upload_blob)"))
    }

    pub async fn download(
        &self,
        _metadata: RemoteExecutionMetadata,
        request: DownloadRequest,
    ) -> anyhow::Result<DownloadResponse> {
        use re_grpc_proto::build::bazel::remote::execution::v2::compressor;
        use re_grpc_proto::build::bazel::remote::execution::v2::BatchReadBlobsRequest;

        let mut client = self.grpc_clients.cas_client.clone();

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

        let blobs: Vec<InlinedDigestWithStatus> =
            response.into_inner().responses.into_try_map(|r| {
                anyhow::Ok(InlinedDigestWithStatus {
                    // TODO(aloiscochard): Here we should check if r.status is ok!
                    digest: tdigest_from(r.digest.with_context(|| "Response digest not found.")?),
                    status: tstatus_ok(),
                    blob: r.data,
                })
            })?;

        Ok(DownloadResponse {
            inlined_blobs: Some(blobs),
            directories: None,
        })
    }

    pub async fn get_digests_ttl(
        &self,
        _metadata: RemoteExecutionMetadata,
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
        let state = self.state.lock().unwrap_or_else(|e| e.into_inner());
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
