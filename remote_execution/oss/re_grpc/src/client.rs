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
use std::num::NonZeroUsize;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use buck2_re_configuration::Buck2OssReConfiguration;
use buck2_re_configuration::HttpHeader;
use dashmap::DashMap;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use futures::Stream;
use gazebo::prelude::*;
use lru::LruCache;
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
use re_grpc_proto::build::bazel::remote::execution::v2::BatchUpdateBlobsResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::Digest;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteOperationMetadata;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteRequest as GExecuteRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteResponse as GExecuteResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::FindMissingBlobsRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::FindMissingBlobsResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::GetActionResultRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::GetCapabilitiesRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::RequestMetadata;
use re_grpc_proto::build::bazel::remote::execution::v2::ResultsCachePolicy;
use re_grpc_proto::build::bazel::remote::execution::v2::ToolDetails;
use re_grpc_proto::google::bytestream::byte_stream_client::ByteStreamClient;
use re_grpc_proto::google::bytestream::ReadRequest;
use re_grpc_proto::google::bytestream::ReadResponse;
use re_grpc_proto::google::bytestream::WriteRequest;
use re_grpc_proto::google::bytestream::WriteResponse;
use re_grpc_proto::google::longrunning::operation::Result as OpResult;
use re_grpc_proto::google::rpc::Code;
use re_grpc_proto::google::rpc::Status;
use regex::Regex;
use tokio::fs::OpenOptions;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
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

const DEFAULT_MAX_TOTAL_BATCH_SIZE: usize = 4 * 1000 * 1000;

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
        group: TCodeReasonGroup::UNKNOWN,
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
                "Invalid URI scheme: `{}` for `{}` (you should omit it)",
                scheme,
                uri,
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
    max_total_batch_size: usize,
    /// Does the remote server support execution.
    exec_enabled: bool,
}

/// Contains runtime options for the remote execution client as set under `buck2_re_client`
pub struct RERuntimeOpts {
    /// Use the Meta version of the request metadata
    use_fbcode_metadata: bool,
    /// Maximum number of concurrent upload requests.
    max_concurrent_uploads_per_action: Option<usize>,
}

struct InstanceName(Option<String>);

impl InstanceName {
    fn as_str(&self) -> &str {
        match &self.0 {
            Some(instance_name) => instance_name,
            None => "",
        }
    }

    fn as_resource_prefix(&self) -> String {
        match &self.0 {
            Some(instance_name) => format!("{}/", instance_name),
            None => "".to_owned(),
        }
    }
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

        let mut capabilities_client = CapabilitiesClient::with_interceptor(
            capabilities.context("Error creating Capabilities client")?,
            interceptor.dupe(),
        );

        if let Some(max_decoding_message_size) = opts.max_decoding_message_size {
            capabilities_client =
                capabilities_client.max_decoding_message_size(max_decoding_message_size);
        }

        let instance_name = InstanceName(opts.instance_name.clone());

        let capabilities = if opts.capabilities.unwrap_or(true) {
            Self::fetch_rbe_capabilities(
                &mut capabilities_client,
                &instance_name,
                opts.max_total_batch_size,
            )
            .await?
        } else {
            RECapabilities {
                exec_enabled: true,
                max_total_batch_size: DEFAULT_MAX_TOTAL_BATCH_SIZE,
            }
        };

        if !capabilities.exec_enabled {
            return Err(anyhow::anyhow!("Server has remote execution disabled."));
        }

        let max_decoding_msg_size = opts
            .max_decoding_message_size
            .unwrap_or(capabilities.max_total_batch_size * 2);

        if max_decoding_msg_size < capabilities.max_total_batch_size {
            return Err(anyhow::anyhow!(
                "Attribute `max_decoding_message_size` must always be equal or higher to `max_total_batch_size`"
            ));
        }

        let grpc_clients = GRPCClients {
            cas_client: ContentAddressableStorageClient::with_interceptor(
                cas.context("Error creating CAS client")?,
                interceptor.dupe(),
            )
            .max_decoding_message_size(max_decoding_msg_size),
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
            )
            .max_decoding_message_size(max_decoding_msg_size),
        };

        Ok(REClient::new(
            RERuntimeOpts {
                use_fbcode_metadata: opts.use_fbcode_metadata,
                max_concurrent_uploads_per_action: opts.max_concurrent_uploads_per_action,
            },
            grpc_clients,
            capabilities,
            instance_name,
        ))
    }

    async fn fetch_rbe_capabilities(
        client: &mut CapabilitiesClient<GrpcService>,
        instance_name: &InstanceName,
        max_total_batch_size: Option<usize>,
    ) -> anyhow::Result<RECapabilities> {
        // TODO use more of the capabilities of the remote build executor

        let resp = client
            .get_capabilities(GetCapabilitiesRequest {
                instance_name: instance_name.as_str().to_owned(),
            })
            .await
            .context("Failed to query capabilities of remote")?
            .into_inner();

        let mut exec_enabled = true;

        let max_total_batch_size_from_capabilities: Option<usize> =
            if let Some(cache_cap) = resp.cache_capabilities {
                let size = cache_cap.max_batch_total_size_bytes as usize;
                // A value of 0 means no limit is set
                if size != 0 { Some(size) } else { None }
            } else {
                None
            };

        let max_total_batch_size =
            match (max_total_batch_size_from_capabilities, max_total_batch_size) {
                (Some(cap), Some(config)) => std::cmp::min(cap, config),
                (Some(cap), None) => cap,
                (None, Some(config)) => config,
                (None, None) => DEFAULT_MAX_TOTAL_BATCH_SIZE,
            };

        if let Some(exec_cap) = resp.execution_capabilities {
            exec_enabled = exec_cap.exec_enabled;
        }

        Ok(RECapabilities {
            max_total_batch_size,
            exec_enabled,
        })
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

type GrpcService = InterceptedService<Channel, InjectHeadersInterceptor>;

pub struct GRPCClients {
    cas_client: ContentAddressableStorageClient<GrpcService>,
    execution_client: ExecutionClient<GrpcService>,
    action_cache_client: ActionCacheClient<GrpcService>,
    bytestream_client: ByteStreamClient<GrpcService>,
}

enum DigestRemoteState {
    ExistsOnRemote,
    Missing,
}

struct FindMissingCache {
    cache: LruCache<TDigest, DigestRemoteState>,
    /// To avoid a situation where we cache that an artifact is available remotely, but the artifact then expires
    /// we clear our local cache once every `ttl`.
    ttl: Duration,
    last_check: Instant,
}

impl FindMissingCache {
    fn clear_if_ttl_expires(&mut self) {
        if self.last_check.elapsed() > self.ttl {
            self.cache.clear();
            self.last_check = Instant::now();
        }
    }

    pub fn put(&mut self, digest: TDigest, state: DigestRemoteState) {
        self.clear_if_ttl_expires();
        self.cache.put(digest, state);
    }
    pub fn get(&mut self, digest: &TDigest) -> Option<&DigestRemoteState> {
        self.clear_if_ttl_expires();
        self.cache.get(digest)
    }
}

#[derive(Clone)]
enum OngoingUploadStatus {
    Active(tokio::sync::watch::Receiver<Result<(), ()>>),
    Done,
    Error,
}

pub struct REClient {
    runtime_opts: RERuntimeOpts,
    grpc_clients: GRPCClients,
    capabilities: RECapabilities,
    instance_name: InstanceName,
    // buck2 calls find_missing for same blobs
    find_missing_cache: Mutex<FindMissingCache>,
    prev_uploads: DashMap<TDigest, OngoingUploadStatus>,
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

/// Builds up a vector of batch upload requests based upon the maximum allowed message size.
#[derive(Default)]
struct BatchUploadReqAggregator {
    max_msg_size: i64,
    curr_req: Vec<BatchUploadRequest>,
    requests: Vec<Vec<BatchUploadRequest>>,
    curr_request_size: i64,
}

impl BatchUploadReqAggregator {
    pub fn new(max_msg_size: usize) -> Self {
        BatchUploadReqAggregator {
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
        if !self.curr_req.is_empty() {
            self.requests.push(std::mem::take(&mut self.curr_req));
        }
        self.requests
    }
}

impl REClient {
    fn new(
        runtime_opts: RERuntimeOpts,
        grpc_clients: GRPCClients,
        capabilities: RECapabilities,
        instance_name: InstanceName,
    ) -> Self {
        REClient {
            runtime_opts,
            grpc_clients,
            capabilities,
            instance_name,
            find_missing_cache: Mutex::new(FindMissingCache {
                cache: LruCache::new(NonZeroUsize::new(50 << 20).unwrap()), // 50Mb
                ttl: Duration::from_secs(12 * 60 * 60), // 12 hours TODO: Tune this parameter
                last_check: Instant::now(),
            }),
            prev_uploads: DashMap::new(),
        }
    }

    pub async fn get_action_result(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ActionResultRequest,
    ) -> anyhow::Result<ActionResultResponse> {
        let mut client = self.grpc_clients.action_cache_client.clone();

        let res = client
            .get_action_result(with_re_metadata(
                GetActionResultRequest {
                    instance_name: self.instance_name.as_str().to_owned(),
                    action_digest: Some(tdigest_to(request.digest)),
                    ..Default::default()
                },
                metadata,
                self.runtime_opts.use_fbcode_metadata,
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
            instance_name: self.instance_name.as_str().to_owned(),
            skip_cache_lookup: false,
            execution_policy: None,
            results_cache_policy: Some(ResultsCachePolicy { priority: 0 }),
            action_digest: Some(action_digest.clone()),
        };

        let stream = client
            .execute(with_re_metadata(
                request,
                metadata,
                self.runtime_opts.use_fbcode_metadata,
            ))
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
                            group: TCodeReasonGroup::UNKNOWN,
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
                            status: TStatus {
                                code: TCode::OK,
                                message: execute_response_grpc.message,
                                ..Default::default()
                            },
                            cached_result: execute_response_grpc.cached_result,
                            action_digest: Default::default(), // Filled in below.
                            executed_action_details: Default::default(),
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
        upload_impl(
            &self.instance_name,
            request,
            self.capabilities.max_total_batch_size,
            self.runtime_opts.max_concurrent_uploads_per_action,
            &self.prev_uploads,
            |re_request| async {
                let metadata = metadata.clone();
                let mut cas_client = self.grpc_clients.cas_client.clone();
                let resp = cas_client
                    .batch_update_blobs(with_re_metadata(
                        re_request,
                        metadata,
                        self.runtime_opts.use_fbcode_metadata,
                    ))
                    .await?;
                Ok(resp.into_inner())
            },
            |segments| async {
                let metadata = metadata.clone();
                let mut bytestream_client = self.grpc_clients.bytestream_client.clone();
                let requests = futures::stream::iter(segments);
                let resp = bytestream_client
                    .write(with_re_metadata(
                        requests,
                        metadata,
                        self.runtime_opts.use_fbcode_metadata,
                    ))
                    .await?;

                Ok(resp.into_inner())
            },
        )
        .await
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
            &self.instance_name,
            request,
            self.capabilities.max_total_batch_size,
            |re_request| async {
                let metadata = metadata.clone();
                let mut client = self.grpc_clients.cas_client.clone();
                Ok(client
                    .batch_read_blobs(with_re_metadata(
                        re_request,
                        metadata,
                        self.runtime_opts.use_fbcode_metadata,
                    ))
                    .await?
                    .into_inner())
            },
            |read_request| {
                let metadata = metadata.clone();
                async move {
                    let mut client = self.grpc_clients.bytestream_client.clone();
                    let response = client
                        .read(with_re_metadata(
                            read_request,
                            metadata,
                            self.runtime_opts.use_fbcode_metadata,
                        ))
                        .await?
                        .into_inner();
                    Ok(Box::pin(response.into_stream()))
                }
            },
        )
        .await
    }

    pub async fn get_digests_ttl(
        &self,
        metadata: RemoteExecutionMetadata,
        request: GetDigestsTtlRequest,
    ) -> anyhow::Result<GetDigestsTtlResponse> {
        let mut cas_client = self.grpc_clients.cas_client.clone();
        let mut remote_ttl: HashMap<TDigest, DigestWithTtl> = HashMap::new();

        for digest_chunk in request.digests.chunks(100) {
            let mut digest_to_check: Vec<TDigest> = Vec::new();
            {
                let mut find_missing_cache = self.find_missing_cache.lock().unwrap();
                for digest in digest_chunk {
                    // Assume that all digests are present on the remote because the API
                    // returns what is *not* present.
                    remote_ttl.insert(
                        digest.clone(),
                        DigestWithTtl {
                            digest: digest.clone(),
                            // NOTE: This is an arbitrary number because RBE does not return information
                            // on the TTL of the remote blob.
                            ttl: 60,
                        },
                    );
                    match find_missing_cache.get(digest) {
                        Some(DigestRemoteState::Missing) | None => {
                            digest_to_check.push(digest.clone());
                        }
                        _ => {}
                    }
                }
            }

            if digest_to_check.is_empty() {
                continue;
            }

            let missing_blobs = cas_client
                .find_missing_blobs(with_re_metadata(
                    FindMissingBlobsRequest {
                        instance_name: self.instance_name.as_str().to_owned(),
                        blob_digests: digest_to_check.map(|b| tdigest_to(b.clone())),
                    },
                    metadata.clone(),
                    self.runtime_opts.use_fbcode_metadata,
                ))
                .await
                .context("Failed to request what blobs are not present on remote")?;
            let resp: FindMissingBlobsResponse = missing_blobs.into_inner();
            let mut find_missing_cache = self.find_missing_cache.lock().unwrap();
            for digest in &digest_to_check {
                find_missing_cache.put(digest.clone(), DigestRemoteState::ExistsOnRemote);
            }
            for digest in &resp.missing_blob_digests.map(|d| tdigest_from(d.clone())) {
                // If it's present in the MissingBlobsResponse, it's expired on the remote and
                // needs to be refetched.
                remote_ttl.insert(
                    digest.clone(),
                    DigestWithTtl {
                        digest: digest.clone(),
                        ttl: 0,
                    },
                );
                find_missing_cache.put(digest.clone(), DigestRemoteState::Missing);
            }
        }

        Ok(GetDigestsTtlResponse {
            digests_with_ttl: remote_ttl.values().cloned().collect::<Vec<DigestWithTtl>>(),
        })
    }

    pub async fn extend_digest_ttl(
        &self,
        _metadata: RemoteExecutionMetadata,
        _request: ExtendDigestsTtlRequest,
    ) -> anyhow::Result<TDigest> {
        // TODO(arr)
        Err(anyhow::anyhow!("Not implemented (RE extend_digest_ttl)"))
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

    let output_symlinks = action_result
        .output_symlinks
        .into_try_map(|output_symlink| {
            anyhow::Ok(TSymlink {
                name: output_symlink.path,
                target: output_symlink.target,
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
        output_symlinks,
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

async fn download_impl<Byt, BytRet, Cas>(
    instance_name: &InstanceName,
    request: DownloadRequest,
    max_total_batch_size: usize,
    cas_f: impl Fn(BatchReadBlobsRequest) -> Cas,
    bystream_fut: impl Fn(ReadRequest) -> Byt + Sync + Send + Copy,
) -> anyhow::Result<DownloadResponse>
where
    Byt: Future<Output = anyhow::Result<Pin<Box<BytRet>>>>,
    BytRet: Stream<Item = Result<ReadResponse, tonic::Status>>,
    Cas: Future<Output = anyhow::Result<BatchReadBlobsResponse>>,
{
    let bystream_fut = |digest: TDigest| async move {
        let hash = digest.hash;
        let size_in_bytes = digest.size_in_bytes;

        let resource_name = format!(
            "{}blobs/{}/{}",
            instance_name.as_resource_prefix(),
            hash,
            size_in_bytes
        );

        bystream_fut(ReadRequest {
            resource_name: resource_name.clone(),
            read_offset: 0,
            read_limit: 0,
        })
        .await
        .with_context(|| format!("Failed to read {} from Bytestream service", resource_name))
    };

    let inlined_digests = request.inlined_digests.unwrap_or_default();
    let file_digests = request.file_digests.unwrap_or_default();

    let mut curr_size = 0;
    let mut requests = vec![];
    let mut curr_digests = vec![];
    for digest in file_digests
        .iter()
        .map(|req| &req.named_digest.digest)
        .chain(inlined_digests.iter())
        .map(|d| tdigest_to(d.clone()))
        .filter(|d| d.size_bytes > 0)
    {
        if digest.size_bytes as usize >= max_total_batch_size {
            // digest is too big to download in a BatchReadBlobsRequest
            // need to use the bytstream api
            continue;
        }
        curr_size += digest.size_bytes;
        if curr_size >= max_total_batch_size as i64 {
            let read_blob_req = BatchReadBlobsRequest {
                instance_name: instance_name.as_str().to_owned(),
                digests: std::mem::take(&mut curr_digests),
                acceptable_compressors: vec![compressor::Value::Identity as i32],
            };
            requests.push(read_blob_req);
            curr_size = digest.size_bytes;
        }
        curr_digests.push(digest.clone());
    }

    if !curr_digests.is_empty() {
        let read_blob_req = BatchReadBlobsRequest {
            instance_name: instance_name.as_str().to_owned(),
            digests: std::mem::take(&mut curr_digests),
            acceptable_compressors: vec![compressor::Value::Identity as i32],
        };
        requests.push(read_blob_req);
    }

    let mut batched_blobs_response = HashMap::new();
    for read_blob_req in requests {
        let resp = cas_f(read_blob_req)
            .await
            .context("Failed to make BatchReadBlobs request")?;
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

    let mut inlined_blobs = vec![];
    for digest in inlined_digests {
        let data = if digest.size_in_bytes as usize >= max_total_batch_size {
            let mut accum = vec![];
            let mut responses = bystream_fut(digest.clone()).await?;
            while let Some(resp) = responses.next().await {
                let data = resp
                    .with_context(|| format!("Failed to fetch inline digest: {digest}"))?
                    .data;
                accum.extend_from_slice(&data);
            }
            accum
        } else {
            get(&digest)?
        };
        inlined_blobs.push(InlinedDigestWithStatus {
            digest,
            status: tstatus_ok(),
            blob: data,
        })
    }

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

            // If the data is small enough to be transferred in a batch
            // blob update, write it all at once to the file. Otherwise, it'll
            // be streamed in chunks as the remote responds.
            if req.named_digest.digest.size_in_bytes < max_total_batch_size as i64 {
                let data = get(&req.named_digest.digest)?;
                file.write_all(&data)
                    .await
                    .with_context(|| format!("Error writing: {}", req.named_digest.digest))?;
            } else {
                let mut responses = bystream_fut(req.named_digest.digest.clone()).await?;
                while let Some(resp) = responses.next().await {
                    let data = resp
                        .with_context(|| format!("Failed to fetch file: {:?}", file))?
                        .data;
                    file.write_all(&data).await.with_context(|| {
                        format!("Error writing chunk of: {}", req.named_digest.digest)
                    })?;
                }
            }
            file.flush().await.context("Error flushing")?;
            anyhow::Ok(())
        };
        fut.await.with_context(|| {
            format!(
                "Error downloading digest `{}` to `{}`",
                req.named_digest.digest, req.named_digest.name,
            )
        })
    });

    buck2_util::future::try_join_all(writes).await?;

    Ok(DownloadResponse {
        inlined_blobs: Some(inlined_blobs),
        directories: None,
        local_cache_stats: Default::default(),
    })
}

async fn upload_impl<Byt, Cas>(
    instance_name: &InstanceName,
    request: UploadRequest,
    max_total_batch_size: usize,
    max_concurrent_uploads: Option<usize>,
    prev_uploads: &DashMap<TDigest, OngoingUploadStatus>,
    cas_f: impl Fn(BatchUpdateBlobsRequest) -> Cas + Sync + Send + Copy,
    bystream_fut: impl Fn(Vec<WriteRequest>) -> Byt + Sync + Send + Copy,
) -> anyhow::Result<UploadResponse>
where
    Cas: Future<Output = anyhow::Result<BatchUpdateBlobsResponse>> + Send,
    Byt: Future<Output = anyhow::Result<WriteResponse>> + Send,
{
    // NOTE if we stop recording blob_hashes, we can drop out a lot of allocations.
    let mut upload_futures: Vec<BoxFuture<anyhow::Result<Vec<String>>>> = vec![];

    // For small file uploads the client should group them together and call `BatchUpdateBlobs`
    // https://github.com/bazelbuild/remote-apis/blob/main/build/bazel/remote/execution/v2/remote_execution.proto#L205
    let mut batched_blob_updates = BatchUploadReqAggregator::new(max_total_batch_size);

    // Create futures for any blobs that need uploading.
    for blob in request.inlined_blobs_with_digest.unwrap_or_default() {
        let hash = blob.digest.hash.clone();
        let size = blob.digest.size_in_bytes;

        if size < max_total_batch_size as i64 {
            batched_blob_updates.push(BatchUploadRequest::Blob(blob));
            continue;
        }

        let data = blob.blob;
        let client_uuid = uuid::Uuid::new_v4().to_string();
        let resource_name = format!(
            "{}uploads/{}/blobs/{}/{}",
            instance_name.as_resource_prefix(),
            client_uuid,
            hash,
            size
        );
        let fut = async move {
            // Number of complete (non-partial) messages
            let mut upload_segments = vec![];
            for (i, chunk) in data.chunks(max_total_batch_size).enumerate() {
                upload_segments.push(WriteRequest {
                    resource_name: resource_name.to_owned(),
                    write_offset: (i * max_total_batch_size) as i64,
                    finish_write: false,
                    data: chunk.to_owned(),
                });
            }
            upload_segments.last_mut().unwrap().finish_write = true;

            let resp = bystream_fut(upload_segments).await?;
            if resp.committed_size != size {
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
        let digest = file.digest.clone();
        let name = file.name.clone();
        if digest.size_in_bytes < max_total_batch_size as i64 {
            batched_blob_updates.push(BatchUploadRequest::File(file));
            continue;
        }
        let client_uuid = uuid::Uuid::new_v4().to_string();
        let resource_name = format!(
            "{}uploads/{}/blobs/{}/{}",
            instance_name.as_resource_prefix(),
            client_uuid,
            file.digest.hash,
            file.digest.size_in_bytes
        );

        enum UploadStatus {
            New(tokio::sync::watch::Sender<Result<(), ()>>),
            Ongoing(OngoingUploadStatus),
        }

        let upload_status = match prev_uploads.entry(digest.clone()) {
            dashmap::mapref::entry::Entry::Occupied(o) => UploadStatus::Ongoing(o.get().clone()),
            dashmap::mapref::entry::Entry::Vacant(v) => {
                let (tx, rx) = tokio::sync::watch::channel(Err(()));
                v.insert(OngoingUploadStatus::Active(rx));
                UploadStatus::New(tx)
            }
        };
        let fut = async move {
            match upload_status {
                UploadStatus::Ongoing(OngoingUploadStatus::Active(mut rx)) => {
                    // Another task was already uploading this artifact, wait for it complete and report result.
                    rx.changed().await?;
                    rx.borrow_and_update().as_ref().map_err(|_e| {
                        anyhow::anyhow!("Upload queued for previous action failed.")
                    })?;
                }
                UploadStatus::Ongoing(OngoingUploadStatus::Done) => {
                    // Another task has already completed the upload of this artifact, no need to do any work.
                }
                UploadStatus::Ongoing(OngoingUploadStatus::Error) => {
                    // Another task tried to perform the transmission, but failed.
                    anyhow::bail!("Upload queued for previous action failed.")
                }
                UploadStatus::New(tx) => {
                    let mut file = tokio::fs::File::open(&name)
                        .await
                        .with_context(|| format!("Opening `{name}` for reading failed"))?;
                    let mut data = vec![0; max_total_batch_size];

                    let mut write_offset = 0;
                    let mut upload_segments = Vec::new();
                    loop {
                        let length = file
                            .read(&mut data)
                            .await
                            .with_context(|| format!("Error reading from {name}"))?;
                        if length == 0 {
                            break;
                        }
                        upload_segments.push(WriteRequest {
                            resource_name: resource_name.to_owned(),
                            write_offset,
                            finish_write: false,
                            data: data[..length].to_owned(),
                        });
                        write_offset += length as i64;
                    }
                    upload_segments
                        .last_mut()
                        .with_context(|| format!("Read no segments from `{name} "))?
                        .finish_write = true;

                    let upload_ret = bystream_fut(upload_segments)
                        .await
                        .and_then(|resp| {
                            if resp.committed_size != digest.size_in_bytes {
                                Err(anyhow::anyhow!(
                                    "Failed to upload `{name}`: invalid committed_size from WriteResponse"
                                ))
                            }
                            else {
                                Ok(())
                            }
                        });

                    // Mark artifact as uploaded and notify other potentially waiting tasks.
                    if upload_ret.is_ok() {
                        prev_uploads.alter(&digest, |_, _| OngoingUploadStatus::Done);
                        let _ = tx.send(upload_ret.as_ref().map_err(|_| ()).cloned());
                    } else {
                        prev_uploads.alter(&digest, |_, _| OngoingUploadStatus::Error);
                        let _ = tx.send(Err(()));
                    }

                    // Only propage errors _after_ notifying other waiting tasks that this task is complete.
                    upload_ret?;
                }
            }

            Ok(vec![digest.hash])
        };
        upload_futures.push(Box::pin(fut));
    }

    // Create futures for any files small enough that they
    // should be uploaded in batches.
    let batched_blob_updates = batched_blob_updates.done();
    for batch in batched_blob_updates {
        let fut = async move {
            let mut re_request = BatchUpdateBlobsRequest {
                instance_name: instance_name.as_str().to_owned(),
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
                        let mut fin = tokio::fs::File::open(&file.name)
                            .await
                            .with_context(|| format!("Opening {} for writing failed", file.name))?;
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

            let response = cas_f(re_request).await?;
            let failures: Vec<String> = response
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

    let blob_hashes = if let Some(concurrency_limit) = max_concurrent_uploads {
        futures::stream::iter(upload_futures)
            .buffer_unordered(concurrency_limit)
            .try_collect::<Vec<Vec<String>>>()
            .await?
    } else {
        futures::future::try_join_all(upload_futures).await?
    };

    tracing::debug!("uploaded: {:?}", blob_hashes);
    Ok(UploadResponse {})
}

fn with_re_metadata<T>(
    t: T,
    metadata: RemoteExecutionMetadata,
    use_fbcode_metadata: bool,
) -> tonic::Request<T> {
    // This creates a new Tonic request with attached metadata for the RE
    // backend. There are two cases here we need to support:
    //
    //   - Servers that abide by the remote execution apis defined with Bazel,
    //     AKA the "OSS RE API", which this package implements
    //   - The internal RE solution used at Meta, which uses a different API,
    //     but is compatible with the OSS RE API to some extent.
    //
    // The second case is supported only through attaching some metadata to the
    // request, which the fbcode RE service understands; and the reason for all
    // of this is that it allows this OSS client package to be tested inside of
    // fbcode builds within Meta. So there doesn't need to be a separate CI
    // check.
    //
    // However, we don't need it for FOSS builds of Buck2. And in theory we
    // could test the OSS Bazel API in the upstream GitHub CI, but doing it this
    // way is only a little ugly, it's hidden, and it helps ensure the internal
    // Meta builds catch those issues earlier.

    let mut msg = tonic::Request::new(t);

    if use_fbcode_metadata {
        // This is pretty ugly, but the protobuf spec that defines this is
        // internal, so considering field numbers need to be stable anyway (=
        // low risk), and this is not used in prod (= low impact if this goes
        // wrong), we just inline it here. This is a small hack that lets us use
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

        let mut encoded = Vec::new();
        Metadata {
            platform: metadata.platform,
            use_case_id: Some(metadata.use_case_id),
        }
        .encode(&mut encoded)
        .expect("Encoding into a Vec cannot not fail");

        msg.metadata_mut()
            .insert_bin("re-metadata-bin", MetadataValue::from_bytes(&encoded));
    } else {
        let mut encoded = Vec::new();
        RequestMetadata {
            tool_details: Some(ToolDetails {
                tool_name: "buck2".to_owned(),
                // TODO(#503): Pull the BuckVersion::get_unique_id() from BuckDaemon
                tool_version: "0.1.0".to_owned(),
            }),
            action_id: "".to_owned(),
            tool_invocation_id: metadata
                .buck_info
                .map_or(String::new(), |buck_info| buck_info.build_id),
            correlated_invocations_id: "".to_owned(),
            action_mnemonic: "".to_owned(),
            target_id: "".to_owned(),
            configuration_id: "".to_owned(),
        }
        .encode(&mut encoded)
        .expect("Encoding into a Vec cannot not fail");

        msg.metadata_mut().insert_bin(
            "build.bazel.remote.execution.v2.requestmetadata-bin",
            MetadataValue::from_bytes(&encoded),
        );
    };
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
    use core::sync::atomic::Ordering;
    use std::sync::atomic::AtomicU16;

    use re_grpc_proto::build::bazel::remote::execution::v2::batch_read_blobs_response;
    use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_response;

    use super::*;

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
            &InstanceName(None),
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
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
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
    async fn test_download_large_named() -> anyhow::Result<()> {
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

        let blob_data = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];

        let digest2 = TDigest {
            hash: "xl".to_owned(),
            size_in_bytes: 18,
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
                    digest: Some(tdigest_to(digest1.clone())),
                    data: vec![1, 2, 3],
                    ..Default::default()
                },
            ],
        };

        let read_response1 = ReadResponse {
            data: blob_data[..10].to_vec(),
        };
        let read_response2 = ReadResponse {
            data: blob_data[10..].to_vec(),
        };

        download_impl(
            &InstanceName(None),
            req,
            10, // kept small to simulate a large file download
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                async move {
                    assert_eq!(req.digests.len(), 1);
                    assert_eq!(req.digests[0], tdigest_to(digest1));
                    Ok(res.clone())
                }
            },
            |req| {
                let read_response1 = read_response1.clone();
                let read_response2 = read_response2.clone();
                async move {
                    assert_eq!(req.resource_name, "blobs/xl/18");
                    anyhow::Ok(Box::pin(futures::stream::iter(vec![
                        Ok(read_response1),
                        Ok(read_response2),
                    ])))
                }
            },
        )
        .await?;

        assert_eq!(tokio::fs::read(&path1).await?, vec![1, 2, 3]);
        assert_eq!(tokio::fs::read(&path2).await?, blob_data);

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
            &InstanceName(None),
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
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
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
    async fn test_download_multiple_batches() -> anyhow::Result<()> {
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

        let digest3 = &TDigest {
            hash: "cc".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest4 = &TDigest {
            hash: "dd".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest5 = &TDigest {
            hash: "dd".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest6 = &TDigest {
            hash: "dd".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digests = vec![
            digest1.clone(),
            digest2.clone(),
            digest3.clone(),
            digest4.clone(),
            digest5.clone(),
            digest6.clone(),
        ];

        let req = DownloadRequest {
            inlined_digests: Some(digests.clone()),
            ..Default::default()
        };

        let counter = AtomicU16::new(0);

        let res = download_impl(
            &InstanceName(None),
            req,
            7,
            |req| {
                counter.fetch_add(1, Ordering::Relaxed);
                let res = BatchReadBlobsResponse {
                    responses: req.digests.map(|d| batch_read_blobs_response::Response {
                        digest: Some(d.clone()),
                        data: vec![0, 1, 2],
                        ..Default::default()
                    }),
                };
                async { Ok(res) }
            },
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
        )
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), digests.len());
        assert_eq!(counter.load(Ordering::Relaxed), 3);

        Ok(())
    }

    #[tokio::test]
    async fn test_download_large_inlined() -> anyhow::Result<()> {
        let digest1 = &TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest2 = &TDigest {
            hash: "xl".to_owned(),
            size_in_bytes: 18,
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
                    digest: Some(tdigest_to(digest1.clone())),
                    data: vec![1, 2, 3],
                    ..Default::default()
                },
            ],
        };

        let blob_data = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];

        let read_response1 = ReadResponse {
            data: blob_data[..10].to_vec(),
        };
        let read_response2 = ReadResponse {
            data: blob_data[10..].to_vec(),
        };

        let res = download_impl(
            &InstanceName(None),
            req,
            10, // intentionally small value to keep data in the test blobs small
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                async move {
                    assert_eq!(req.digests.len(), 1);
                    assert_eq!(req.digests[0], tdigest_to(digest1));
                    Ok(res)
                }
            },
            |req| {
                let read_response1 = read_response1.clone();
                let read_response2 = read_response2.clone();
                async move {
                    assert_eq!(req.resource_name, "blobs/xl/18");
                    anyhow::Ok(Box::pin(futures::stream::iter(vec![
                        Ok(read_response1),
                        Ok(read_response2),
                    ])))
                }
            },
        )
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), 2);

        assert_eq!(inlined_blobs[0].digest, *digest1);
        assert_eq!(inlined_blobs[0].blob, vec![1, 2, 3]);

        assert_eq!(inlined_blobs[1].digest, *digest2);
        assert_eq!(inlined_blobs[1].blob, blob_data);

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
            &InstanceName(None),
            req,
            100000,
            |req| {
                let res = res.clone();
                async move {
                    assert_eq!(req.digests.len(), 0);
                    Ok(res)
                }
            },
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
        )
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), 1);

        assert_eq!(inlined_blobs[0].digest, *digest1);
        assert!(inlined_blobs[0].blob.is_empty());

        Ok(())
    }

    #[tokio::test]
    async fn test_download_resource_name() -> anyhow::Result<()> {
        let digest1 = &TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 0,
            ..Default::default()
        };

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone()]),
            ..Default::default()
        };

        download_impl(
            &InstanceName(Some("instance".to_owned())),
            req,
            0,
            |_req| async { panic!("not called") },
            |req| async move {
                assert_eq!(req.resource_name, "instance/blobs/aa/0");
                anyhow::Ok(Box::pin(futures::stream::iter(vec![])))
            },
        )
        .await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_upload_named() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path1, "aaa").await?;

        let path2 = work.path().join("path2");
        let path2 = path2.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path2, "bbb").await?;

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

        let req = UploadRequest {
            files_with_digest: Some(vec![
                NamedDigest {
                    name: path1.to_owned(),
                    digest: digest1.clone(),
                    ..Default::default()
                },
                NamedDigest {
                    name: path2.to_owned(),
                    digest: digest2.clone(),
                    ..Default::default()
                },
            ]),
            ..Default::default()
        };

        let res = BatchUpdateBlobsResponse {
            responses: vec![
                // Reply out of order
                batch_update_blobs_response::Response {
                    digest: Some(tdigest_to(digest2.clone())),
                    status: Some(Status::default()),
                },
                batch_update_blobs_response::Response {
                    digest: Some(tdigest_to(digest1.clone())),
                    status: Some(Status::default()),
                },
            ],
        };

        upload_impl(
            &InstanceName(None),
            req,
            10000,
            None,
            &DashMap::new(),
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.requests.len(), 2);
                    assert_eq!(req.requests[0].digest, Some(tdigest_to(digest1)));
                    assert_eq!(req.requests[0].data, b"aaa");
                    assert_eq!(req.requests[1].digest, Some(tdigest_to(digest2)));
                    assert_eq!(req.requests[1].data, b"bbb");
                    Ok(res)
                }
            },
            |_req| async { panic!("A Bytestream upload should not be triggered") },
        )
        .await?;

        Ok(())
    }

    #[tokio::test]
    async fn test_upload_large_named() -> anyhow::Result<()> {
        let blob_data = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];

        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path1, "aaa").await?;

        let path2 = work.path().join("path2");
        let path2 = path2.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path2, &blob_data).await?;

        let digest1 = TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };

        let digest2 = TDigest {
            hash: "xl".to_owned(),
            size_in_bytes: 18,
            ..Default::default()
        };

        let req = UploadRequest {
            files_with_digest: Some(vec![
                NamedDigest {
                    name: path1.to_owned(),
                    digest: digest1.clone(),
                    ..Default::default()
                },
                NamedDigest {
                    name: path2.to_owned(),
                    digest: digest2.clone(),
                    ..Default::default()
                },
            ]),
            ..Default::default()
        };

        let res = BatchUpdateBlobsResponse {
            responses: vec![
                // Reply out of order
                batch_update_blobs_response::Response {
                    digest: Some(tdigest_to(digest2.clone())),
                    status: Some(Status::default()),
                },
                batch_update_blobs_response::Response {
                    digest: Some(tdigest_to(digest1.clone())),
                    status: Some(Status::default()),
                },
            ],
        };

        upload_impl(
            &InstanceName(None),
            req,
            10, // kept small to simulate a large file upload
            None,
            &DashMap::new(),
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                async move {
                    assert_eq!(req.requests.len(), 1);
                    assert_eq!(req.requests[0].digest, Some(tdigest_to(digest1)));
                    assert_eq!(req.requests[0].data, b"aaa");
                    Ok(res)
                }
            },
            |write_reqs| {
                let blob_data = blob_data.clone();
                async move {
                    assert_eq!(write_reqs.len(), 2);
                    assert_eq!(write_reqs[0].write_offset, 0);
                    assert!(!write_reqs[0].finish_write);
                    assert_eq!(write_reqs[0].data, blob_data[..10]);
                    assert_eq!(write_reqs[1].write_offset, 10);
                    assert!(write_reqs[1].finish_write);
                    assert_eq!(write_reqs[1].data, blob_data[10..]);
                    anyhow::Ok(WriteResponse { committed_size: 18 })
                }
            },
        )
        .await?;
        Ok(())
    }

    #[tokio::test]
    async fn test_upload_large_inlined() -> anyhow::Result<()> {
        let digest1 = TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };
        let blob_data1 = b"aaa".to_vec();

        let digest2 = TDigest {
            hash: "xl".to_owned(),
            size_in_bytes: 18,
            ..Default::default()
        };
        let blob_data2 = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];

        let req = UploadRequest {
            inlined_blobs_with_digest: Some(vec![
                InlinedBlobWithDigest {
                    blob: blob_data2.clone(),
                    digest: digest2.clone(),
                    ..Default::default()
                },
                InlinedBlobWithDigest {
                    blob: blob_data1.clone(),
                    digest: digest1.clone(),
                    ..Default::default()
                },
            ]),
            ..Default::default()
        };

        let res = BatchUpdateBlobsResponse {
            responses: vec![batch_update_blobs_response::Response {
                digest: Some(tdigest_to(digest2.clone())),
                status: Some(Status::default()),
            }],
        };

        upload_impl(
            &InstanceName(None),
            req,
            10, // kept small to simulate a large inlined upload
            None,
            &DashMap::new(),
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let blob_data1 = blob_data1.clone();
                async move {
                    assert_eq!(req.requests.len(), 1);
                    assert_eq!(req.requests[0].digest, Some(tdigest_to(digest1)));
                    assert_eq!(req.requests[0].data, blob_data1);
                    Ok(res)
                }
            },
            |write_reqs| {
                let blob_data2 = blob_data2.clone();
                async move {
                    assert_eq!(write_reqs.len(), 2);
                    assert_eq!(write_reqs[0].write_offset, 0);
                    assert!(!write_reqs[0].finish_write);
                    assert_eq!(write_reqs[0].data, blob_data2[..10]);
                    assert_eq!(write_reqs[1].write_offset, 10);
                    assert!(write_reqs[1].finish_write);
                    assert_eq!(write_reqs[1].data, blob_data2[10..]);
                    anyhow::Ok(WriteResponse { committed_size: 18 })
                }
            },
        )
        .await?;
        Ok(())
    }

    #[tokio::test]
    async fn test_upload_invalid_committed_size() -> anyhow::Result<()> {
        let blob_data = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];

        let work = tempfile::tempdir()?;

        let path2 = work.path().join("path2");
        let path2 = path2.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path2, &blob_data).await?;

        let digest2 = TDigest {
            hash: "xl".to_owned(),
            size_in_bytes: 18,
            ..Default::default()
        };

        let req = UploadRequest {
            files_with_digest: Some(vec![NamedDigest {
                name: path2.to_owned(),
                digest: digest2.clone(),
                ..Default::default()
            }]),
            ..Default::default()
        };

        let resp: Result<UploadResponse, anyhow::Error> = upload_impl(
            &InstanceName(None), // TODO
            req,
            10,
            None,
            &DashMap::new(),
            |_req| async move {
                panic!("This should not be called as there are no blobs to upload in batch");
            },
            |_write_reqs| async move {
                // Not the right size
                anyhow::Ok(WriteResponse { committed_size: 10 })
            },
        )
        .await;

        let err: anyhow::Error = resp.unwrap_err();
        // can't compare the full message because tempfile is used
        assert!(
            err.root_cause()
                .to_string()
                .contains("invalid committed_size")
        );

        Ok(())
    }

    #[tokio::test]
    async fn test_upload_exact() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path1, "aaabbb").await?;

        let digest1 = TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 6,
            ..Default::default()
        };

        let digest2 = TDigest {
            hash: "bb".to_owned(),
            size_in_bytes: 6,
            ..Default::default()
        };
        let blob_data2 = vec![1, 2, 3, 4, 5, 6];

        let req = UploadRequest {
            files_with_digest: Some(vec![NamedDigest {
                name: path1.to_owned(),
                digest: digest1.clone(),
                ..Default::default()
            }]),
            inlined_blobs_with_digest: Some(vec![InlinedBlobWithDigest {
                blob: blob_data2.clone(),
                digest: digest2.clone(),
                ..Default::default()
            }]),
            ..Default::default()
        };

        upload_impl(
            &InstanceName(None),
            req,
            3,
            None,
            &DashMap::new(),
            |_req| async move {
                panic!("Not called");
            },
            |write_reqs| async move {
                assert_eq!(write_reqs.len(), 2);
                assert!(write_reqs[1].finish_write);
                anyhow::Ok(WriteResponse { committed_size: 6 })
            },
        )
        .await?;
        Ok(())
    }

    #[tokio::test]
    async fn test_upload_empty() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path1, "").await?;

        let digest1 = TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 0,
            ..Default::default()
        };

        let req = UploadRequest {
            files_with_digest: Some(vec![NamedDigest {
                name: path1.to_owned(),
                digest: digest1.clone(),
                ..Default::default()
            }]),
            ..Default::default()
        };

        let res = upload_impl(
            &InstanceName(None),
            req,
            0,
            None,
            &DashMap::new(),
            |_req| async move {
                panic!("Not called");
            },
            |_write_reqs| async move {
                panic!("Not called");
            },
        )
        .await;

        assert!(res.is_err()); // Should not panic.

        Ok(())
    }

    #[tokio::test]
    async fn test_upload_resource_name() -> anyhow::Result<()> {
        let digest1 = TDigest {
            hash: "aa".to_owned(),
            size_in_bytes: 3,
            ..Default::default()
        };
        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;
        tokio::fs::write(path1, "aaa").await?;

        let req = UploadRequest {
            inlined_blobs_with_digest: Some(vec![InlinedBlobWithDigest {
                digest: digest1.clone(),
                blob: b"aaa".to_vec(),
                ..Default::default()
            }]),
            files_with_digest: Some(vec![NamedDigest {
                name: path1.to_owned(),
                digest: digest1.clone(),
                ..Default::default()
            }]),
            ..Default::default()
        };

        upload_impl(
            &InstanceName(Some("instance".to_owned())),
            req,
            1,
            None,
            &DashMap::new(),
            |_req| async move {
                panic!("Not called");
            },
            |write_reqs| async move {
                assert!(write_reqs[0].resource_name.starts_with("instance/uploads/"));
                assert!(write_reqs[0].resource_name.ends_with("/blobs/aa/3"));
                anyhow::Ok(WriteResponse { committed_size: 3 })
            },
        )
        .await?;

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
