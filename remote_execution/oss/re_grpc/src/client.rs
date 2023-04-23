/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::env::VarError;
use std::sync::Arc;
use std::sync::Mutex;

use anyhow::Context;
use buck2_re_configuration::Buck2OssReConfiguration;
use buck2_re_configuration::HttpHeader;
use dupe::Dupe;
use futures::future::Future;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use prost::Message;
use re_grpc_proto::build::bazel::remote::execution::v2::action_cache_client::ActionCacheClient;
use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_request::Request;
use re_grpc_proto::build::bazel::remote::execution::v2::capabilities_client::CapabilitiesClient;
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
use re_grpc_proto::build::bazel::remote::execution::v2::GetCapabilitiesRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::ResultsCachePolicy;
use re_grpc_proto::google::bytestream::byte_stream_client::ByteStreamClient;
use re_grpc_proto::google::bytestream::ReadRequest;
use re_grpc_proto::google::bytestream::ReadResponse;
use re_grpc_proto::google::bytestream::WriteRequest;
use re_grpc_proto::google::longrunning::operation::Result as OpResult;
use re_grpc_proto::google::rpc::Code;
use re_grpc_proto::google::rpc::Status;
use regex::Regex;
use tokio::fs::OpenOptions;
use tokio::io::AsyncBufReadExt;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tonic::codegen::InterceptedService;
use tonic::metadata;
use tonic::metadata::MetadataKey;
use tonic::metadata::MetadataValue;
use tonic::service::Interceptor;
use tonic::transport::channel::ClientTlsConfig;
use tonic::transport::Certificate;
use tonic::transport::Channel;
use tonic::transport::Identity;
use tonic::transport::Uri;

use crate::error::*;
use crate::metadata::*;
use crate::request::*;
use crate::response::*;

const INSTANCE_NAME: &str = "";

// RBE Services (e.g. Buildbarn) may not be robust against having too many files open at
// once. Limit to an arbitrary reasonable number since this information is not expressed
// in the Capabilities message query.
const CONCURRENT_UPLOAD_LIMIT: usize = 512;

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

async fn create_tls_config(opts: &Buck2OssReConfiguration) -> anyhow::Result<ClientTlsConfig> {
    let config = ClientTlsConfig::new();

    let config = match opts.tls_ca_certs.as_ref() {
        Some(tls_ca_certs) => {
            let tls_ca_certs =
                substitute_env_vars(tls_ca_certs).context("Invalid `tls_ca_certs`")?;
            let data = tokio::fs::read(&tls_ca_certs)
                .await
                .with_context(|| format!("Error reading `{}`", tls_ca_certs))?;
            config.ca_certificate(Certificate::from_pem(data))
        }
        None => {
            // We set the `tls-webpki-roots` feature so we'll get that default.
            config
        }
    };

    let config = match opts.tls_client_cert.as_ref() {
        Some(tls_client_cert) => {
            let tls_client_cert =
                substitute_env_vars(tls_client_cert).context("Invalid `tls_client_cert`")?;
            let data = tokio::fs::read(&tls_client_cert)
                .await
                .with_context(|| format!("Error reading `{}`", tls_client_cert))?;
            config.identity(Identity::from_pem(&data, &data))
        }
        None => config,
    };

    Ok(config)
}

fn prepare_uri(uri: Uri, tls: bool) -> anyhow::Result<Uri> {
    // Now do some awkward things with the protocol. Why do we do all this? The reason is
    // because we'd like our configuration to not be super confusing. We don't want to e.g.
    // allow setting the address to `https://foobar` without enabling TLS (or enabling tls
    // and using `http://foobar`), so we restrict ourselves to schemes that are actually
    // remotely valid in GRPC (which is more restrictive than what Tonic allows).

    // This is the GRPC spec for naming: https://github.com/grpc/grpc/blob/master/doc/naming.md
    // Many people (including Bazel), use grpc://, so we tolerate it.

    match uri.scheme_str() {
        Some("grpc") | Some("dns") | Some("ipv4") | Some("ipv6") | None => {}
        Some(scheme) => {
            return Err(anyhow::anyhow!(
                "Invalid URI scheme: `{}` (you should omit it)",
                scheme
            ));
        }
    };

    // And now, let's put back a proper scheme for Tonic to be happy with. First, because
    // Tonic will blow up if we don't. Second, so we get port inference.
    let mut parts = uri.into_parts();
    parts.scheme = Some(if tls {
        http::uri::Scheme::HTTPS
    } else {
        http::uri::Scheme::HTTP
    });

    // Is this API actually designed to be unusable? If you've got a scheme, you must
    // have a path_and_query. I'm sure there's a good reason, so we abide:
    if parts.path_and_query.is_none() {
        parts.path_and_query = Some(http::uri::PathAndQuery::from_static(""));
    }

    Ok(Uri::from_parts(parts)?)
}

/// Contains information queried from the the Remote Execution Capabilities service.
pub struct RECapabilities {
    /// Largest size of a message before being uploaded using bytestream service.
    /// 0 indicates no limit beyond constraint of underlying transport (which is unknown).
    max_msg_size: usize,
}

pub struct REClientBuilder;

impl REClientBuilder {
    pub async fn build_and_connect(opts: &Buck2OssReConfiguration) -> anyhow::Result<REClient> {
        // We just always create this just in case, so that we implicitly validate it if set.
        let tls_config = create_tls_config(opts)
            .await
            .context("Invalid TLS config")?;

        let tls_config = &tls_config;

        let create_channel = |address: Option<String>| async move {
            let address = address.as_ref().context("No address")?;
            let address = substitute_env_vars(address).context("Invalid address")?;
            let uri = address.parse().context("Invalid address")?;
            let uri = prepare_uri(uri, opts.tls).context("Invalid URI")?;

            let mut channel = Channel::builder(uri);
            if opts.tls {
                channel = channel.tls_config(tls_config.clone())?;
            }

            anyhow::Ok(
                channel
                    .connect()
                    .await
                    .with_context(|| format!("Error connecting to `{}`", address))?,
            )
        };

        let (cas, execution, action_cache, bytestream, capabilities) = futures::future::join5(
            create_channel(opts.cas_address.clone()),
            create_channel(opts.engine_address.clone()),
            create_channel(opts.action_cache_address.clone()),
            create_channel(opts.cas_address.clone()),
            create_channel(opts.engine_address.clone()),
        )
        .await;

        let interceptor = InjectHeadersInterceptor::new(&opts.http_headers)?;

        let mut grpc_clients = GRPCClients {
            cas_client: ContentAddressableStorageClient::with_interceptor(
                cas.context("Error creating CAS client")?,
                interceptor.dupe(),
            ),
            execution_client: ExecutionClient::with_interceptor(
                execution.context("Error creating Execution client")?,
                interceptor.dupe(),
            ),
            action_cache_client: ActionCacheClient::with_interceptor(
                action_cache.context("Error creating ActionCache client")?,
                interceptor.dupe(),
            ),
            bytestream_client: ByteStreamClient::with_interceptor(
                bytestream.context("Error creating Bytestream client")?,
                interceptor.dupe(),
            ),
            capabilities_client: CapabilitiesClient::with_interceptor(
                capabilities.context("Error creating Capabilities client")?,
                interceptor.dupe(),
            ),
        };

        let capabilities = Self::fetch_rbe_capabilities(&mut grpc_clients).await?;
        Ok(REClient::new(grpc_clients, capabilities))
    }

    pub async fn fetch_rbe_capabilities(
        clients: &mut GRPCClients,
    ) -> anyhow::Result<RECapabilities> {
        // TODO use more of the capabilities of the remote build executor

        let resp = clients
            .capabilities_client
            .get_capabilities(GetCapabilitiesRequest {
                instance_name: INSTANCE_NAME.into(),
            })
            .await?;

        // Default is a reasonable size for the gRPC transport
        // with enough room for headers.
        let mut max_msg_size = 1000 * 1000 * 4;

        if let Some(cache_cap) = resp.into_inner().cache_capabilities {
            let size = cache_cap.max_batch_total_size_bytes as usize;
            // A value of 0 means no limit is set
            if size != 0 {
                max_msg_size = size;
            }
        }

        Ok(RECapabilities { max_msg_size })
    }
}

#[derive(Clone, Dupe)]
struct InjectHeadersInterceptor {
    headers: Arc<Vec<(MetadataKey<metadata::Ascii>, MetadataValue<metadata::Ascii>)>>,
}

impl InjectHeadersInterceptor {
    pub fn new(headers: &[HttpHeader]) -> anyhow::Result<Self> {
        let headers = headers
            .iter()
            .map(|h| {
                // This means we can't have `$` in a header key or value, which isn't great. On the
                // flip side, env vars are good for things like credentials, which those headers
                // are likely to contain. In time, we should allow escaping.
                let key = substitute_env_vars(&h.key)?;
                let value = substitute_env_vars(&h.value)?;

                let key = MetadataKey::<metadata::Ascii>::from_bytes(key.as_bytes())
                    .with_context(|| format!("Invalid key in header: `{}: {}`", key, value))?;

                let value = MetadataValue::try_from(&value)
                    .with_context(|| format!("Invalid value in header: `{}: {}`", key, value))?;

                anyhow::Ok((key, value))
            })
            .collect::<Result<_, _>>()
            .context("Error converting headers")?;

        Ok(Self {
            headers: Arc::new(headers),
        })
    }
}

impl Interceptor for InjectHeadersInterceptor {
    fn call(
        &mut self,
        mut request: tonic::Request<()>,
    ) -> Result<tonic::Request<()>, tonic::Status> {
        for (k, v) in self.headers.iter() {
            request.metadata_mut().insert(k.clone(), v.clone());
        }
        Ok(request)
    }
}

pub struct GRPCClients {
    cas_client:
        ContentAddressableStorageClient<InterceptedService<Channel, InjectHeadersInterceptor>>,
    execution_client: ExecutionClient<InterceptedService<Channel, InjectHeadersInterceptor>>,
    action_cache_client: ActionCacheClient<InterceptedService<Channel, InjectHeadersInterceptor>>,
    bytestream_client: ByteStreamClient<InterceptedService<Channel, InjectHeadersInterceptor>>,
    capabilities_client: CapabilitiesClient<InterceptedService<Channel, InjectHeadersInterceptor>>,
}

#[derive(Default)]
pub struct REState {
    // TODO(aloiscochard): Update those values
    network_uploaded: i64,   // in bytes
    network_downloaded: i64, // in bytes
}

pub struct REClient {
    grpc_clients: GRPCClients,
    capabilities: RECapabilities,
    state: Mutex<REState>,
}

impl Drop for REClient {
    fn drop(&mut self) {
        // Important we have a drop implementation since the real one does, and we
        // don't want errors coming from the stub not having one
    }
}

/// Information on components of a batch upload.
/// Used to defer reading of NamedDigest contents till
/// actual execution of upload and prevent opening too many
/// files at the same time.
enum BatchUploadRequest {
    Blob(InlinedBlobWithDigest),
    File(NamedDigest),
}

#[derive(Default)]
/// Builds up a vector of batch upload requests based upon the maximum allowed message size.
struct BatchUploader {
    max_msg_size: i64,
    curr_req: Vec<BatchUploadRequest>,
    requests: Vec<Vec<BatchUploadRequest>>,
    curr_request_size: i64,
}

impl BatchUploader {
    pub fn new(max_msg_size: usize) -> Self {
        BatchUploader {
            max_msg_size: max_msg_size as i64,
            ..Default::default()
        }
    }

    pub fn push(&mut self, req: BatchUploadRequest) {
        let size_in_bytes = match &req {
            BatchUploadRequest::Blob(blob) => blob.digest.size_in_bytes,
            BatchUploadRequest::File(file) => file.digest.size_in_bytes,
        };
        self.curr_request_size += size_in_bytes;

        if self.curr_request_size >= self.max_msg_size {
            self.requests.push(std::mem::take(&mut self.curr_req));
            self.curr_request_size = size_in_bytes;
        }
        self.curr_req.push(req);
    }

    pub fn done(mut self) -> Vec<Vec<BatchUploadRequest>> {
        self.requests.push(std::mem::take(&mut self.curr_req));
        self.requests
    }
}

impl REClient {
    pub fn new(grpc_clients: GRPCClients, capabilities: RECapabilities) -> Self {
        REClient {
            grpc_clients,
            capabilities,
            state: Mutex::new(REState::default()),
        }
    }

    pub async fn get_action_result(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ActionResultRequest,
    ) -> anyhow::Result<ActionResultResponse> {
        let mut client = self.grpc_clients.action_cache_client.clone();

        let res = client
            .get_action_result(with_internal_metadata(
                GetActionResultRequest {
                    instance_name: INSTANCE_NAME.into(),
                    action_digest: Some(tdigest_to(request.digest)),
                    ..Default::default()
                },
                metadata,
            ))
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
        metadata: RemoteExecutionMetadata,
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

        let stream = client
            .execute(with_internal_metadata(request, metadata))
            .await?
            .into_inner();

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
                            ..Default::default()
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
                    ..Default::default()
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
        metadata: RemoteExecutionMetadata,
        request: UploadRequest,
    ) -> anyhow::Result<UploadResponse> {
        // NOTE if we stop recording blob_hashes, we can drop out a lot of allocations.
        let mut upload_futures: Vec<
            std::pin::Pin<Box<dyn Future<Output = Result<Vec<String>, anyhow::Error>> + Send>>,
        > = vec![];

        let max_msg_size = self.capabilities.max_msg_size;

        // For small file uploads the client should group them together and call `BatchUpdateBlobs`
        // https://github.com/bazelbuild/remote-apis/blob/main/build/bazel/remote/execution/v2/remote_execution.proto#L205
        let mut batched_blob_updates = BatchUploader::new(max_msg_size);

        // Create futures for any blobs that need uploading.
        for blob in request.inlined_blobs_with_digest.unwrap_or_default() {
            let mut bytestream_client = self.grpc_clients.bytestream_client.clone();
            let hash = blob.digest.hash.clone();
            let size = blob.digest.size_in_bytes;

            if size < max_msg_size as i64 {
                batched_blob_updates.push(BatchUploadRequest::Blob(blob));
                continue;
            }

            let uuid = uuid::Uuid::new_v4().to_string();
            let data = blob.blob;
            let name = format!("uploads/{}/blobs/{}/{}", uuid, hash, size);
            let metadata = metadata.clone();
            let fut = async move {
                // Number of complete (non-partial) messages
                let num_msgs = size as usize / max_msg_size;
                let upload_segments = futures::stream::iter(data).chunks(max_msg_size);
                let requests = upload_segments
                    .enumerate()
                    .map(move |(i, chunk)| WriteRequest {
                        resource_name: name.to_owned(),
                        write_offset: (i * max_msg_size) as i64,
                        finish_write: i == num_msgs,
                        data: chunk.to_owned(),
                    });

                let resp = bytestream_client
                    .write(with_internal_metadata(requests, metadata))
                    .await?;
                if resp.into_inner().committed_size != size as i64 {
                    return Err(anyhow::anyhow!(
                        "Failed to upload inline blob: invalid committed_size from WriteResponse"
                    ));
                }

                Ok(vec![hash])
            };
            upload_futures.push(Box::pin(fut));
        }

        // Create futures for any files that needs uploading.
        for file in request.files_with_digest.unwrap_or_default() {
            let mut bytestream_client = self.grpc_clients.bytestream_client.clone();
            let hash = file.digest.hash.clone();
            let size = file.digest.size_in_bytes;
            let uuid = uuid::Uuid::new_v4().to_string();
            let name = file.name.clone();
            let metadata = metadata.clone();

            if size < max_msg_size as i64 {
                batched_blob_updates.push(BatchUploadRequest::File(file));
                continue;
            }
            let fut = async move {
                let file = tokio::fs::File::open(&name).await?;
                let mut reader = BufReader::with_capacity(max_msg_size, file);
                let mut write_offset = 0;

                let mut upload_segments = Vec::new();
                loop {
                    let resource_name = format!("uploads/{}/blobs/{}/{}", uuid, hash.clone(), size);
                    let data = reader.fill_buf().await?;
                    let length = data.len();

                    upload_segments.push(WriteRequest {
                        resource_name: resource_name.to_owned(),
                        write_offset,
                        finish_write: length == 0,
                        data: data.to_owned(),
                    });

                    write_offset += length as i64;
                    reader.consume(length);
                    if length == 0 {
                        break;
                    }
                }

                let resp = bytestream_client
                    .write(with_internal_metadata(
                        futures::stream::iter(upload_segments),
                        metadata,
                    ))
                    .await?;
                if resp.into_inner().committed_size != size as i64 {
                    return Err(anyhow::anyhow!(
                        "Failed to upload {name}: invalid committed_size from WriteResponse"
                    ));
                }
                Ok(vec![hash])
            };
            upload_futures.push(Box::pin(fut));
        }

        // Create futures for any files small enough that they
        // should be uploaded in batches.
        let batched_blob_updates = batched_blob_updates.done();
        for batch in batched_blob_updates {
            let metadata = metadata.clone();
            let mut cas_client = self.grpc_clients.cas_client.clone();
            let fut = async move {
                let mut re_request = BatchUpdateBlobsRequest {
                    instance_name: INSTANCE_NAME.into(),
                    requests: vec![],
                };
                for blob in batch {
                    match blob {
                        BatchUploadRequest::Blob(blob) => {
                            re_request.requests.push(Request {
                                digest: Some(tdigest_to(blob.digest.clone())),
                                data: blob.blob.clone(),
                                compressor: compressor::Value::Identity as i32,
                            });
                        }
                        BatchUploadRequest::File(file) => {
                            // These should be small files, so no need to use a buffered reader.
                            let mut fin = tokio::fs::File::open(&file.name).await?;
                            let mut data = vec![];
                            fin.read_to_end(&mut data).await?;

                            re_request.requests.push(Request {
                                digest: Some(tdigest_to(file.digest.clone())),
                                data,
                                compressor: compressor::Value::Identity as i32,
                            });
                        }
                    }
                }
                let blob_hashes = re_request
                    .requests
                    .iter()
                    .map(|x| x.digest.as_ref().unwrap().hash.clone())
                    .collect::<Vec<String>>();
                let response = cas_client
                    .batch_update_blobs(with_internal_metadata(re_request, metadata))
                    .await?;
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

                if !failures.is_empty() {
                    return Err(anyhow::anyhow!("Batch upload failed: {:?}", failures));
                }
                Ok(blob_hashes)
            };
            upload_futures.push(Box::pin(fut));
        }

        let upload_stream =
            futures::stream::iter(upload_futures).buffer_unordered(CONCURRENT_UPLOAD_LIMIT);
        let blob_hashes = upload_stream.try_collect::<Vec<Vec<String>>>().await?;

        tracing::debug!("uploaded: {:?}", blob_hashes);
        Ok(UploadResponse {})
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
        metadata: RemoteExecutionMetadata,
        request: DownloadRequest,
    ) -> anyhow::Result<DownloadResponse> {
        download_impl(
            request,
            self.capabilities.max_msg_size,
            |re_request| async {
                let metadata = metadata.clone();
                let mut client = self.grpc_clients.cas_client.clone();
                Ok(client
                    .batch_read_blobs(with_internal_metadata(re_request, metadata))
                    .await?
                    .into_inner())
            },
            |digest| async move {
                let mut client = self.grpc_clients.bytestream_client.clone();

                let resource_name = format!("blobs/{}/{}", digest.hash, digest.size_in_bytes);
                let response = client
                    .read(ReadRequest {
                        resource_name,
                        read_offset: 0,
                        read_limit: 0,
                    })
                    .await?
                    .into_inner();
                Ok(response)
            },
        )
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

async fn download_impl<Bf, BFut, F, Fut>(
    request: DownloadRequest,
    max_msg_size: usize,
    f: F,
    big_f: Bf,
) -> anyhow::Result<DownloadResponse>
where
    Bf: Fn(TDigest) -> BFut,
    BFut: Future<Output = anyhow::Result<tonic::Streaming<ReadResponse>>>,
    F: Fn(BatchReadBlobsRequest) -> Fut,
    Fut: Future<Output = anyhow::Result<BatchReadBlobsResponse>>,
{
    let inlined_digests = request.inlined_digests.unwrap_or_default();
    let file_digests = request.file_digests.unwrap_or_default();

    let mut curr_size = 0;
    let mut requests = vec![];
    let mut curr_digests = vec![];
    let mut bytestream_digests = vec![];
    for digest in file_digests
        .iter()
        .map(|req| &req.named_digest.digest)
        .chain(inlined_digests.iter())
        .map(|d| tdigest_to(d.clone()))
        .filter(|d| d.size_bytes > 0)
    {
        if digest.size_bytes as usize > max_msg_size {
            bytestream_digests.push(digest);
            continue;
        }
        curr_size += digest.size_bytes;
        if curr_size > max_msg_size as i64 {
            let read_blob_req = BatchReadBlobsRequest {
                instance_name: INSTANCE_NAME.into(),
                digests: std::mem::take(&mut curr_digests),
                acceptable_compressors: vec![compressor::Value::Identity as i32],
            };
            requests.push(read_blob_req);
        }
        curr_digests.push(digest.clone());
    }
    let read_blob_req = BatchReadBlobsRequest {
        instance_name: INSTANCE_NAME.into(),
        digests: std::mem::take(&mut curr_digests),
        acceptable_compressors: vec![compressor::Value::Identity as i32],
    };
    requests.push(read_blob_req);

    let mut batched_blobs_response = HashMap::new();
    for read_blob_req in requests {
        let resp = f(read_blob_req).await?;
        for r in resp.responses.into_iter() {
            let digest = tdigest_from(r.digest.context("Response digest not found.")?);
            check_status(r.status.unwrap_or_default())?;
            batched_blobs_response.insert(digest, r.data);
        }
    }

    let get = |digest: &TDigest| -> anyhow::Result<Vec<u8>> {
        if digest.size_in_bytes == 0 {
            return Ok(Vec::new());
        }

        Ok(batched_blobs_response
            .get(digest)
            .with_context(|| format!("Did not receive digest data for `{}`", digest))?
            .clone())
    };

    let inlined_blobs = inlined_digests.into_try_map(|digest| {
        let data = get(&digest)?;

        anyhow::Ok(InlinedDigestWithStatus {
            digest,
            status: tstatus_ok(),
            blob: data,
        })
    })?;

    let writes = file_digests.iter().map(|req| async {
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

        let fut = async {
            let mut file = opts
                .open(&req.named_digest.name)
                .await
                .context("Error opening")?;
            // If the data is small enough to be transfered in a batch
            // blob update, just write it directly to file.
            if req.named_digest.digest.size_in_bytes < max_msg_size as i64 {
                let data = get(&req.named_digest.digest)?;
                file.write_all(&data).await.context("Error writing")?;
            } else {
                let mut responses = big_f(req.named_digest.digest.clone()).await?;
                while let Some(ReadResponse { data }) = responses.message().await? {
                    file.write_all(&data).await.context("Error writing")?;
                }
            }
            file.flush().await.context("Error flushing")?;
            anyhow::Ok(())
        };
        fut.await.with_context(|| {
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

fn with_internal_metadata<T>(t: T, metadata: RemoteExecutionMetadata) -> tonic::Request<T> {
    // This is pretty ugly, but the protobuf spec that defines this is internal, so considering
    // field numbers need to be stable anyway (= low risk), and this is not used in prod (= low
    // impact if this goes wrong), we just inline it here. This is a small hack that lets us use
    // our internal RE using this GRPC client for testing.
    //
    // This is defined in `fbcode/remote_execution/grpc/metadata.proto`.
    #[derive(prost::Message)]
    struct Metadata {
        #[prost(message, optional, tag = "15")]
        platform: Option<crate::grpc::Platform>,
        #[prost(string, optional, tag = "18")]
        use_case_id: Option<String>,
    }

    let mut msg = tonic::Request::new(t);

    // We encode minimal metadata here. This is a bit of a hack to be compatible with internal RE.

    let mut encoded = Vec::new();
    Metadata {
        platform: metadata.platform,
        use_case_id: Some(metadata.use_case_id),
    }
    .encode(&mut encoded)
    .expect("Encoding into a Vec cannot not fail");
    msg.metadata_mut()
        .insert_bin("re-metadata-bin", MetadataValue::from_bytes(&encoded));
    msg
}

/// Replace occurrences of $FOO in a string with the value of the env var $FOO.
fn substitute_env_vars(s: &str) -> anyhow::Result<String> {
    substitute_env_vars_impl(s, |v| std::env::var(v))
}

fn substitute_env_vars_impl(
    s: &str,
    getter: impl Fn(&str) -> Result<String, VarError>,
) -> anyhow::Result<String> {
    static ENV_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new("\\$[a-zA-Z_][a-zA-Z_0-9]*").unwrap());

    let mut out = String::with_capacity(s.len());
    let mut last_idx = 0;

    for mat in ENV_REGEX.find_iter(s) {
        out.push_str(&s[last_idx..mat.start()]);
        let var = &mat.as_str()[1..];
        let val = getter(var).with_context(|| format!("Error substituting `{}`", mat.as_str()))?;
        out.push_str(&val);
        last_idx = mat.end();
    }

    if last_idx < s.len() {
        out.push_str(&s[last_idx..s.len()]);
    }

    Ok(out)
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

        download_impl(
            req,
            10000,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.digests.len(), 2);
                    assert_eq!(req.digests[0], tdigest_to(digest1));
                    assert_eq!(req.digests[1], tdigest_to(digest2));
                    Ok(res.clone())
                }
            },
            |_digest| async move {
                panic!("Should never have reached a Bytestream call");
            },
        )
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

        let res = download_impl(
            req,
            100000,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.digests.len(), 2);
                    assert_eq!(req.digests[0], tdigest_to(digest1));
                    assert_eq!(req.digests[1], tdigest_to(digest2));
                    Ok(res)
                }
            },
            |_digest| async move {
                panic!("Should never have reached a Bytestream call");
            },
        )
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), 2);

        assert_eq!(inlined_blobs[0].digest, *digest1);
        assert_eq!(inlined_blobs[0].blob, vec![1, 2, 3]);

        assert_eq!(inlined_blobs[1].digest, *digest2);
        assert_eq!(inlined_blobs[1].blob, vec![4, 5, 6]);

        Ok(())
    }

    #[tokio::test]
    async fn test_download_empty() -> anyhow::Result<()> {
        let digest1 = &TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 0,
            ..Default::default()
        };

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse { responses: vec![] };

        let res = download_impl(
            req,
            100000,
            |req| {
                let res = res.clone();
                async move {
                    assert_eq!(req.digests.len(), 0);
                    Ok(res)
                }
            },
            |_digest| async move {
                panic!("Should never have reached a Bytestream call");
            },
        )
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), 1);

        assert_eq!(inlined_blobs[0].digest, *digest1);
        assert!(inlined_blobs[0].blob.is_empty());

        Ok(())
    }

    #[test]
    fn test_substitute_env_vars() {
        let getter = |s: &str| match s {
            "FOO" => Ok("foo_value".to_owned()),
            "BAR" => Ok("bar_value".to_owned()),
            "BAZ" => Err(VarError::NotPresent),
            _ => panic!("Unexpected"),
        };

        assert_eq!(
            substitute_env_vars_impl("$FOO", getter).unwrap(),
            "foo_value"
        );
        assert_eq!(
            substitute_env_vars_impl("$FOO$BAR", getter).unwrap(),
            "foo_valuebar_value"
        );
        assert_eq!(
            substitute_env_vars_impl("some$FOO.bar", getter).unwrap(),
            "somefoo_value.bar"
        );
        assert_eq!(substitute_env_vars_impl("foo", getter).unwrap(), "foo");
        assert_eq!(substitute_env_vars_impl("FOO", getter).unwrap(), "FOO");
        assert!(substitute_env_vars_impl("$FOO$BAZ", getter).is_err());
    }
}
