/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Mutex;

use anyhow::Context;
use buck2_core::fs::fs_util;
use futures::future::Future;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use prost::Message;
use re_grpc_proto::build::bazel::remote::execution::v2::action_cache_client::ActionCacheClient;
use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_request::Request;
use re_grpc_proto::build::bazel::remote::execution::v2::compressor;
use re_grpc_proto::build::bazel::remote::execution::v2::content_addressable_storage_client::ContentAddressableStorageClient;
use re_grpc_proto::build::bazel::remote::execution::v2::execution_client::ExecutionClient;
use re_grpc_proto::build::bazel::remote::execution::v2::execution_stage;
use re_grpc_proto::build::bazel::remote::execution::v2::ActionResult;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchReadBlobsRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchReadBlobsResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchUpdateBlobsRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::Digest;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteOperationMetadata;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteRequest as GExecuteRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteResponse as GExecuteResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::GetActionResultRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::ResultsCachePolicy;
use re_grpc_proto::google::longrunning::operation::Result as OpResult;
use re_grpc_proto::google::rpc::Code;
use re_grpc_proto::google::rpc::Status;
use slog::Logger;
use tokio::fs::OpenOptions;
use tokio::io::AsyncWriteExt;
use tonic::transport::Channel;

use crate::config::*;
use crate::error::*;
use crate::metadata::*;
use crate::request::*;
use crate::response::*;
const INSTANCE_NAME: &str = "";

#[derive(Default)]
pub struct REClientBuilder {
    logger: Option<Logger>,
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

fn check_status(status: Status) -> Result<(), REClientError> {
    if status.code == 0 {
        return Ok(());
    }

    Err(REClientError {
        code: TCode(status.code),
        message: status.message,
    })
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

            Logger::root(drain, slog::o!())
        };

        let cfg = self.cfg.unwrap_or_default();
        let address = cfg
            .execution_client_config
            .address
            .context("Execution client address not defined")?;

        let grpc_clients = GRPCClients {
            cas_client: ContentAddressableStorageClient::connect(address.clone()).await?,
            execution_client: ExecutionClient::connect(address.clone()).await?,
            action_cache_client: ActionCacheClient::connect(address).await?,
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
    action_cache_client: ActionCacheClient<Channel>,
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
        let mut client = self.grpc_clients.action_cache_client.clone();

        let res = client
            .get_action_result(GetActionResultRequest {
                instance_name: INSTANCE_NAME.into(),
                action_digest: Some(tdigest_to(request.digest)),
                ..Default::default()
            })
            .await?;

        Ok(ActionResultResponse {
            action_result: convert_action_result(res.into_inner())?,
            ttl: 0,
        })
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
        mut execute_request: ExecuteRequest,
    ) -> anyhow::Result<BoxStream<'static, anyhow::Result<ExecuteWithProgressResponse>>> {
        // TODO(aloiscochard): Map those properly in the request
        // use crate::proto::build::bazel::remote::execution::v2::ExecutionPolicy;

        let mut client = self.grpc_clients.execution_client.clone();

        let action_digest = tdigest_to(execute_request.action_digest.clone());

        let request = GExecuteRequest {
            instance_name: INSTANCE_NAME.into(),
            skip_cache_lookup: false,
            execution_policy: None,
            results_cache_policy: Some(ResultsCachePolicy { priority: 0 }),
            action_digest: Some(action_digest.clone()),
        };

        let stream = client.execute(request).await?.into_inner();

        let stream = futures::stream::try_unfold(stream, move |mut stream| async {
            let msg = match stream.try_next().await.context("RE channel error")? {
                Some(msg) => msg,
                None => return Ok(None),
            };

            let status = if msg.done {
                match msg
                    .result
                    .context("Missing `result` when message was `done`")?
                {
                    OpResult::Error(rpc_status) => {
                        return Err(REClientError {
                            code: TCode(rpc_status.code),
                            message: rpc_status.message,
                        }
                        .into());
                    }
                    OpResult::Response(any) => {
                        let execute_response_grpc: GExecuteResponse =
                            GExecuteResponse::decode(&any.value[..])?;

                        check_status(execute_response_grpc.status.unwrap_or_default())?;

                        let action_result = execute_response_grpc
                            .result
                            .with_context(|| "The action result is not defined.")?;

                        let action_result = convert_action_result(action_result)?;

                        let execute_response = ExecuteResponse {
                            action_result,
                            action_result_digest: TDigest::default(),
                            action_result_ttl: 0,
                            error: REError {
                                code: TCode::OK,
                                ..Default::default()
                            },
                            cached_result: execute_response_grpc.cached_result,
                            action_digest: Default::default(), // Filled in below.
                        };

                        ExecuteWithProgressResponse {
                            stage: Stage::COMPLETED,
                            execute_response: Some(execute_response),
                        }
                    }
                }
            } else {
                let meta =
                    ExecuteOperationMetadata::decode(&msg.metadata.unwrap_or_default().value[..])?;

                let stage = match execution_stage::Value::from_i32(meta.stage) {
                    Some(execution_stage::Value::Unknown) => Stage::UNKNOWN,
                    Some(execution_stage::Value::CacheCheck) => Stage::CACHE_CHECK,
                    Some(execution_stage::Value::Queued) => Stage::QUEUED,
                    Some(execution_stage::Value::Executing) => Stage::EXECUTING,
                    Some(execution_stage::Value::Completed) => Stage::COMPLETED,
                    _ => Stage::UNKNOWN,
                };

                ExecuteWithProgressResponse {
                    stage,
                    execute_response: None,
                }
            };

            anyhow::Ok(Some((status, stream)))
        });

        // We fill in the action digest a little later here. We do it this way so we don't have to
        // clone the execute_request into every future we create above.

        let stream = stream.map(move |mut r| {
            match &mut r {
                Ok(ExecuteWithProgressResponse {
                    execute_response: Some(ref mut response),
                    ..
                }) => {
                    response.action_digest = std::mem::take(&mut execute_request.action_digest);
                }
                _ => {}
            };

            r
        });

        Ok(stream.boxed())
    }

    pub async fn upload(
        &self,
        _metadata: RemoteExecutionMetadata,
        request: UploadRequest,
    ) -> anyhow::Result<UploadResponse> {
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
            slog::debug!(self.logger, "uploaded: {:?}", blob_hashes);
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
        download_impl(request, |re_request| async {
            let mut client = self.grpc_clients.cas_client.clone();
            Ok(client.batch_read_blobs(re_request).await?.into_inner())
        })
        .await
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

fn convert_action_result(action_result: ActionResult) -> anyhow::Result<TActionResult2> {
    let execution_metadata = action_result
        .execution_metadata
        .with_context(|| "The execution metadata are not defined.")?;

    let output_files = action_result.output_files.into_try_map(|output_file| {
        let output_file_digest = output_file.digest.with_context(|| "Digest not found.")?;

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

    let output_directories = action_result
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

    let action_result = TActionResult2 {
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
            worker_start_timestamp: ttimestamp_from(execution_metadata.worker_start_timestamp),
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
            ..Default::default()
        },
        ..Default::default()
    };

    Ok(action_result)
}

async fn download_impl<F, Fut>(request: DownloadRequest, f: F) -> anyhow::Result<DownloadResponse>
where
    F: FnOnce(BatchReadBlobsRequest) -> Fut,
    Fut: Future<Output = anyhow::Result<BatchReadBlobsResponse>>,
{
    let inlined_digests = request.inlined_digests.unwrap_or_default();
    let file_digests = request.file_digests.unwrap_or_default();

    let re_request = BatchReadBlobsRequest {
        instance_name: INSTANCE_NAME.into(),
        digests: file_digests
            .iter()
            .map(|req| &req.named_digest.digest)
            .chain(inlined_digests.iter())
            .map(|d| tdigest_to(d.clone()))
            .collect(),
        acceptable_compressors: vec![compressor::Value::Identity as i32],
    };

    let response = f(re_request).await?;

    let response = response
        .responses
        .into_iter()
        .map(|r| {
            check_status(r.status.unwrap_or_default())?;
            let digest = tdigest_from(r.digest.context("Response digest not found.")?);
            anyhow::Ok((digest, r.data))
        })
        .collect::<Result<HashMap<_, _>, _>>()?;

    let inlined_blobs = inlined_digests.into_try_map(|digest| {
        let data = response
            .get(&digest)
            .with_context(|| format!("Did not receive digest data for `{}`", digest))?
            .clone();

        anyhow::Ok(InlinedDigestWithStatus {
            digest,
            status: tstatus_ok(),
            blob: data,
        })
    })?;

    let writes = file_digests.iter().map(|req| async {
        let data = response
            .get(&req.named_digest.digest)
            .with_context(|| {
                format!(
                    "Did not receive digest data for `{}`",
                    req.named_digest.digest
                )
            })?
            .clone();

        let mut opts = OpenOptions::new();
        opts.read(true).write(true).create_new(true);
        #[cfg(unix)]
        {
            if req.is_executable {
                opts.mode(0o755);
            } else {
                opts.mode(0o644);
            }
        }

        async {
            let mut file = opts
                .open(&req.named_digest.name)
                .await
                .context("Error opening")?;
            file.write_all(&data).await.context("Error writing")?;
            file.flush().await.context("Error flushing")?;
            anyhow::Ok(())
        }
        .await
        .with_context(|| {
            format!(
                "Error writing digest `{}` to `{}`",
                req.named_digest.digest, req.named_digest.name,
            )
        })
    });

    futures::future::try_join_all(writes).await?;

    Ok(DownloadResponse {
        inlined_blobs: Some(inlined_blobs),
        directories: None,
    })
}

#[cfg(test)]
mod tests {
    use re_grpc_proto::build::bazel::remote::execution::v2::batch_read_blobs_response;

    use super::*;
    use crate::NamedDigest;
    use crate::NamedDigestWithPermissions;

    #[tokio::test]
    async fn test_download_named() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;

        let path2 = work.path().join("path2");
        let path2 = path2.to_str().context("tempdir is not utf8")?;

        let digest1 = TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest2 = TDigest {
            hash: "bb".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let req = DownloadRequest {
            file_digests: Some(vec![
                NamedDigestWithPermissions {
                    named_digest: NamedDigest {
                        name: path1.to_owned(),
                        digest: digest1.clone(),
                        ..Default::default()
                    },
                    is_executable: true,
                    ..Default::default()
                },
                NamedDigestWithPermissions {
                    named_digest: NamedDigest {
                        name: path2.to_owned(),
                        digest: digest2.clone(),
                        ..Default::default()
                    },
                    is_executable: false,
                    ..Default::default()
                },
            ]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse {
            responses: vec![
                // Reply out of order
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(digest2.clone())),
                    data: vec![4, 5, 6],
                    ..Default::default()
                },
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(digest1.clone())),
                    data: vec![1, 2, 3],
                    ..Default::default()
                },
            ],
        };

        download_impl(req, |req| async move {
            assert_eq!(req.digests.len(), 2);
            assert_eq!(req.digests[0], tdigest_to(digest1.clone()));
            assert_eq!(req.digests[1], tdigest_to(digest2.clone()));
            Ok(res)
        })
        .await?;

        assert_eq!(tokio::fs::read(&path1).await?, vec![1, 2, 3]);
        assert_eq!(tokio::fs::read(&path2).await?, vec![4, 5, 6]);

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            assert_eq!(
                tokio::fs::metadata(&path1).await?.permissions().mode() & 0o111,
                0o111
            );
            assert_eq!(
                tokio::fs::metadata(&path2).await?.permissions().mode() & 0o111,
                0o000
            );
        }

        Ok(())
    }

    #[tokio::test]
    async fn test_download_inlined() -> anyhow::Result<()> {
        let digest1 = &TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest2 = &TDigest {
            hash: "bb".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone(), digest2.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse {
            responses: vec![
                // Reply out of order
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(digest2.clone())),
                    data: vec![4, 5, 6],
                    ..Default::default()
                },
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(digest1.clone())),
                    data: vec![1, 2, 3],
                    ..Default::default()
                },
            ],
        };

        let res = download_impl(req, |req| async move {
            assert_eq!(req.digests.len(), 2);
            assert_eq!(req.digests[0], tdigest_to(digest1.clone()));
            assert_eq!(req.digests[1], tdigest_to(digest2.clone()));
            Ok(res)
        })
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), 2);

        assert_eq!(inlined_blobs[0].digest, *digest1);
        assert_eq!(inlined_blobs[0].blob, vec![1, 2, 3]);

        assert_eq!(inlined_blobs[1].digest, *digest2);
        assert_eq!(inlined_blobs[1].blob, vec![4, 5, 6]);

        Ok(())
    }
}
