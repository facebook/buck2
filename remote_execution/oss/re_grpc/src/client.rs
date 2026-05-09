/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::hash_map;
use std::env::VarError;
use std::io;
use std::io::Cursor;
use std::num::NonZeroUsize;
use std::pin::Pin;
use std::sync::Arc;
use std::sync::LazyLock;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;
use std::time::Instant;

use anyhow::Context;
use async_compression::tokio::bufread::BrotliDecoder;
use async_compression::tokio::bufread::BrotliEncoder;
use async_compression::tokio::bufread::DeflateDecoder;
use async_compression::tokio::bufread::DeflateEncoder;
use async_compression::tokio::bufread::ZstdDecoder;
use async_compression::tokio::bufread::ZstdEncoder;
use buck2_re_configuration::Buck2OssReConfiguration;
use buck2_re_configuration::HttpHeader;
use dupe::Dupe;
use futures::Stream;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use futures::stream::TryStreamExt;
use hyper_util::client::legacy::connect::HttpConnector;
use lru::LruCache;
use prost::Message;
use re_grpc_proto::build::bazel::remote::execution::v2::ActionResult;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchReadBlobsRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchReadBlobsResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchUpdateBlobsRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::BatchUpdateBlobsResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::Digest;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteOperationMetadata;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteRequest as GExecuteRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecuteResponse as GExecuteResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecutedActionMetadata;
use re_grpc_proto::build::bazel::remote::execution::v2::ExecutionPolicy;
use re_grpc_proto::build::bazel::remote::execution::v2::FindMissingBlobsRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::FindMissingBlobsResponse;
use re_grpc_proto::build::bazel::remote::execution::v2::GetActionResultRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::GetCapabilitiesRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::OutputDirectory;
use re_grpc_proto::build::bazel::remote::execution::v2::OutputFile;
use re_grpc_proto::build::bazel::remote::execution::v2::OutputSymlink;
use re_grpc_proto::build::bazel::remote::execution::v2::RequestMetadata;
use re_grpc_proto::build::bazel::remote::execution::v2::ResultsCachePolicy;
use re_grpc_proto::build::bazel::remote::execution::v2::ToolDetails;
use re_grpc_proto::build::bazel::remote::execution::v2::UpdateActionResultRequest;
use re_grpc_proto::build::bazel::remote::execution::v2::action_cache_client::ActionCacheClient;
use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_request::Request;
use re_grpc_proto::build::bazel::remote::execution::v2::capabilities_client::CapabilitiesClient;
use re_grpc_proto::build::bazel::remote::execution::v2::compressor;
use re_grpc_proto::build::bazel::remote::execution::v2::content_addressable_storage_client::ContentAddressableStorageClient;
use re_grpc_proto::build::bazel::remote::execution::v2::digest_function;
use re_grpc_proto::build::bazel::remote::execution::v2::execution_client::ExecutionClient;
use re_grpc_proto::build::bazel::remote::execution::v2::execution_stage;
use re_grpc_proto::google::bytestream::QueryWriteStatusRequest;
use re_grpc_proto::google::bytestream::ReadRequest;
use re_grpc_proto::google::bytestream::ReadResponse;
use re_grpc_proto::google::bytestream::WriteRequest;
use re_grpc_proto::google::bytestream::WriteResponse;
use re_grpc_proto::google::bytestream::byte_stream_client::ByteStreamClient;
use re_grpc_proto::google::longrunning::operation::Result as OpResult;
use re_grpc_proto::google::rpc::Code;
use re_grpc_proto::google::rpc::Status;
use regex::Regex;
use sha1::Sha1;
use sha2::Digest as _;
use sha2::Sha256;
use tokio::fs::OpenOptions;
use tokio::io::AsyncBufRead;
use tokio::io::AsyncRead;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::io::BufReader;
use tokio_util::io::StreamReader;
use tonic::codegen::InterceptedService;
use tonic::metadata;
use tonic::metadata::MetadataKey;
use tonic::metadata::MetadataValue;
use tonic::service::Interceptor;
use tonic::transport::Certificate;
use tonic::transport::Channel;
use tonic::transport::Endpoint;
use tonic::transport::Identity;
use tonic::transport::Uri;
use tonic::transport::channel::ClientTlsConfig;
use uuid::Uuid;

use crate::error::*;
use crate::metadata::*;
use crate::pool::ChannelPool;
use crate::pool::MakeChannel;
use crate::pool::PoolConfig;
use crate::pool::PooledChannel;
use crate::request::*;
use crate::response::*;
use crate::stats::CountingConnector;

const DEFAULT_MAX_TOTAL_BATCH_SIZE: usize = 4 * 1000 * 1000;
const DEFAULT_RETRIES: usize = 5;
const GRPC_RETRY_INITIAL_DELAY_MILLIS: u64 = 100;
const DEFAULT_RETRY_MAX_DELAY_MILLIS: u64 = 5000;
const GRPC_RETRY_JITTER: f64 = 0.1;
const DEFAULT_GRPC_KEEPALIVE_TIME_SECS: u64 = 30;
const DEFAULT_GRPC_KEEPALIVE_TIMEOUT_SECS: u64 = 10;
const DEFAULT_GRPC_KEEPALIVE_WHILE_IDLE: bool = true;

fn tdigest_to(tdigest: &TDigest) -> Digest {
    Digest {
        hash: tdigest.hash.clone(),
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

enum BlobHashVerifier {
    Sha1(Sha1),
    Sha256(Sha256),
    Blake3(blake3::Hasher),
}

impl BlobHashVerifier {
    fn name(&self) -> &'static str {
        match self {
            Self::Sha1(_) => "SHA1",
            Self::Sha256(_) => "SHA256",
            Self::Blake3(_) => "BLAKE3",
        }
    }

    fn update(&mut self, data: &[u8]) {
        match self {
            Self::Sha1(hasher) => hasher.update(data),
            Self::Sha256(hasher) => hasher.update(data),
            Self::Blake3(hasher) => {
                hasher.update(data);
            }
        }
    }

    fn finalize_hex(self) -> String {
        match self {
            Self::Sha1(hasher) => hex::encode(hasher.finalize()),
            Self::Sha256(hasher) => hex::encode(hasher.finalize()),
            Self::Blake3(hasher) => blake3::Hasher::finalize(&hasher).to_hex().to_string(),
        }
    }
}

struct BlobHashValidators {
    expected_hash: String,
    verifiers: Vec<BlobHashVerifier>,
}

impl BlobHashValidators {
    fn new(
        expected_hash: &str,
        selected_digest_function: Option<digest_function::Value>,
    ) -> anyhow::Result<Self> {
        anyhow::ensure!(
            !expected_hash.is_empty(),
            "Digest hash is empty and cannot be validated"
        );
        anyhow::ensure!(
            expected_hash.bytes().all(|b| b.is_ascii_hexdigit()),
            "Digest hash contains non-hex characters: `{expected_hash}`"
        );

        let expected_hash = expected_hash.to_ascii_lowercase();
        let verifiers = if let Some(digest_function) = selected_digest_function {
            match digest_function {
                digest_function::Value::Sha1 => {
                    anyhow::ensure!(
                        expected_hash.len() == 40,
                        "Digest hash length mismatch for configured SHA1: `{expected_hash}`"
                    );
                    vec![BlobHashVerifier::Sha1(Sha1::new())]
                }
                digest_function::Value::Sha256 => {
                    anyhow::ensure!(
                        expected_hash.len() == 64,
                        "Digest hash length mismatch for configured SHA256: `{expected_hash}`"
                    );
                    vec![BlobHashVerifier::Sha256(Sha256::new())]
                }
                digest_function::Value::Blake3 => {
                    anyhow::ensure!(
                        expected_hash.len() == 64,
                        "Digest hash length mismatch for configured BLAKE3: `{expected_hash}`"
                    );
                    vec![BlobHashVerifier::Blake3(blake3::Hasher::new())]
                }
                _ => {
                    anyhow::bail!(
                        "Configured digest function {:?} is not supported for download hash validation",
                        digest_function
                    )
                }
            }
        } else {
            match expected_hash.len() {
                40 => vec![BlobHashVerifier::Sha1(Sha1::new())],
                // Could be either SHA256 or BLAKE3. Validate against both.
                64 => vec![
                    BlobHashVerifier::Sha256(Sha256::new()),
                    BlobHashVerifier::Blake3(blake3::Hasher::new()),
                ],
                n => {
                    anyhow::bail!(
                        "Unsupported digest hash length `{n}` for `{expected_hash}`; cannot validate downloaded blob hash"
                    )
                }
            }
        };

        Ok(Self {
            expected_hash,
            verifiers,
        })
    }

    fn update(&mut self, data: &[u8]) {
        for verifier in &mut self.verifiers {
            verifier.update(data);
        }
    }

    fn finish(self, digest: &TDigest) -> anyhow::Result<()> {
        let mut tried = Vec::with_capacity(self.verifiers.len());
        for verifier in self.verifiers {
            let name = verifier.name();
            tried.push(name);
            if verifier.finalize_hex() == self.expected_hash {
                return Ok(());
            }
        }
        anyhow::bail!(
            "Downloaded blob hash mismatch for `{digest}` after validating with [{}]",
            tried.join(", ")
        );
    }
}

fn validate_downloaded_blob_size(digest: &TDigest, actual_size: usize) -> anyhow::Result<()> {
    let expected_size = usize::try_from(digest.size_in_bytes)
        .with_context(|| format!("Invalid negative digest size for `{digest}`"))?;
    anyhow::ensure!(
        actual_size == expected_size,
        "Downloaded blob size mismatch for `{digest}`: expected {expected_size} bytes, got {actual_size} bytes"
    );
    Ok(())
}

fn validate_downloaded_blob_hash(
    digest: &TDigest,
    data: &[u8],
    selected_digest_function: Option<digest_function::Value>,
) -> anyhow::Result<()> {
    let mut validators = BlobHashValidators::new(&digest.hash, selected_digest_function)?;
    validators.update(data);
    validators.finish(digest)
}

fn validate_downloaded_blob(
    digest: &TDigest,
    data: &[u8],
    selected_digest_function: Option<digest_function::Value>,
) -> anyhow::Result<()> {
    validate_downloaded_blob_size(digest, data.len())?;
    validate_downloaded_blob_hash(digest, data, selected_digest_function)
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

fn tcode_is_retryable(code: TCode) -> bool {
    matches!(
        code,
        TCode::CANCELLED
            | TCode::UNKNOWN
            | TCode::DEADLINE_EXCEEDED
            | TCode::ABORTED
            | TCode::INTERNAL
            | TCode::UNAVAILABLE
            | TCode::RESOURCE_EXHAUSTED
    )
}

fn tcode_from_grpc_code(code: tonic::Code) -> TCode {
    match code {
        tonic::Code::Ok => TCode::OK,
        tonic::Code::Cancelled => TCode::CANCELLED,
        tonic::Code::Unknown => TCode::UNKNOWN,
        tonic::Code::InvalidArgument => TCode::INVALID_ARGUMENT,
        tonic::Code::DeadlineExceeded => TCode::DEADLINE_EXCEEDED,
        tonic::Code::NotFound => TCode::NOT_FOUND,
        tonic::Code::AlreadyExists => TCode::ALREADY_EXISTS,
        tonic::Code::PermissionDenied => TCode::PERMISSION_DENIED,
        tonic::Code::ResourceExhausted => TCode::RESOURCE_EXHAUSTED,
        tonic::Code::FailedPrecondition => TCode::FAILED_PRECONDITION,
        tonic::Code::Aborted => TCode::ABORTED,
        tonic::Code::OutOfRange => TCode::OUT_OF_RANGE,
        tonic::Code::Unimplemented => TCode::UNIMPLEMENTED,
        tonic::Code::Internal => TCode::INTERNAL,
        tonic::Code::Unavailable => TCode::UNAVAILABLE,
        tonic::Code::DataLoss => TCode::DATA_LOSS,
        tonic::Code::Unauthenticated => TCode::UNAUTHENTICATED,
    }
}

fn error_tcode(err: &anyhow::Error) -> Option<TCode> {
    err.downcast_ref::<REClientError>()
        .map(|status| status.code)
        .or_else(|| {
            err.downcast_ref::<tonic::Status>()
                .map(|status| tcode_from_grpc_code(status.code()))
        })
}

fn is_retryable_grpc_error(err: &anyhow::Error) -> bool {
    error_tcode(err).is_some_and(tcode_is_retryable)
}

fn jittered_retry_delay(base_delay: Duration) -> Duration {
    let random_bytes = Uuid::new_v4().into_bytes();
    let random = u16::from_be_bytes([random_bytes[0], random_bytes[1]]) as f64 / u16::MAX as f64;
    let jitter_ratio = GRPC_RETRY_JITTER * ((2.0 * random) - 1.0);
    base_delay.mul_f64(1.0 + jitter_ratio)
}

async fn retry_grpc_request<T, Fut, F>(
    retries: usize,
    retry_max_delay: Duration,
    mut request: F,
) -> anyhow::Result<T>
where
    Fut: Future<Output = anyhow::Result<T>>,
    F: FnMut() -> Fut,
{
    let mut retry_attempt = 0usize;
    let mut next_delay = Duration::from_millis(GRPC_RETRY_INITIAL_DELAY_MILLIS);

    loop {
        match request().await {
            Ok(response) => return Ok(response),
            Err(err) => {
                if retry_attempt >= retries || !is_retryable_grpc_error(&err) {
                    return Err(normalize_re_error(err));
                }

                let delay = jittered_retry_delay(next_delay);
                tracing::debug!(
                    retry_attempt = retry_attempt + 1,
                    retries,
                    delay_ms = delay.as_millis(),
                    "Retrying transient gRPC failure"
                );
                tokio::time::sleep(delay).await;
                retry_attempt += 1;
                next_delay = std::cmp::min(next_delay.saturating_mul(2), retry_max_delay);
            }
        }
    }
}

enum BystreamWritePlan {
    Write(Vec<WriteRequest>),
    AlreadyCommitted(i64),
}

fn total_bystream_write_size(segments: &[WriteRequest]) -> i64 {
    segments
        .last()
        .map(|segment| segment.write_offset + segment.data.len() as i64)
        .unwrap_or(0)
}

fn trim_bystream_write_segments(
    segments: Vec<WriteRequest>,
    committed_size: i64,
) -> Vec<WriteRequest> {
    if committed_size <= 0 {
        return segments;
    }

    let mut resumed = Vec::with_capacity(segments.len());
    for mut segment in segments {
        let start = segment.write_offset;
        let end = start + segment.data.len() as i64;

        if end <= committed_size {
            continue;
        }

        if start < committed_size {
            let skip = (committed_size - start) as usize;
            segment.data = segment.data[skip..].to_vec();
            segment.write_offset = committed_size;
        }

        resumed.push(segment);
    }

    resumed
}

fn ttimestamp_to(ts: &TTimestamp) -> ::prost_types::Timestamp {
    ::prost_types::Timestamp {
        seconds: ts.seconds,
        nanos: ts.nanos,
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

fn tany_to(any: &TAny) -> ::prost_types::Any {
    ::prost_types::Any {
        type_url: any.type_url.clone(),
        value: any.value.clone(),
    }
}

fn tany_from(any: ::prost_types::Any) -> TAny {
    TAny {
        type_url: any.type_url,
        value: any.value,
        ..Default::default()
    }
}

async fn create_tls_config(settings: &GrpcTlsSettings) -> anyhow::Result<ClientTlsConfig> {
    let config = match settings.tls_ca_certs.as_ref() {
        Some(tls_ca_certs) => {
            let tls_ca_certs =
                substitute_env_vars(tls_ca_certs).context("Invalid `tls_ca_certs`")?;
            let data = tokio::fs::read(&tls_ca_certs)
                .await
                .with_context(|| format!("Error reading `{tls_ca_certs}`"))?;
            ClientTlsConfig::new().ca_certificate(Certificate::from_pem(data))
        }
        None => {
            // We set the `tls-webpki-roots` feature so we'll get that default.
            ClientTlsConfig::new().with_enabled_roots()
        }
    };

    let config = match settings.tls_client_cert.as_ref() {
        Some(tls_client_cert) => {
            let tls_client_cert =
                substitute_env_vars(tls_client_cert).context("Invalid `tls_client_cert`")?;
            let data = tokio::fs::read(&tls_client_cert)
                .await
                .with_context(|| format!("Error reading `{tls_client_cert}`"))?;
            config.identity(Identity::from_pem(&data, &data))
        }
        None => config,
    };

    Ok(config)
}

fn prepare_uri(uri: Uri) -> anyhow::Result<(Uri, bool)> {
    // Now do some awkward things with the protocol. Why do we do all this? The reason is
    // because we'd like our configuration to not be super confusing. We don't want to e.g.
    // allow setting the address to `https://foobar`; instead we infer TLS from the source
    // scheme and only accept schemes that are valid in GRPC naming.

    // This is the GRPC spec for naming: https://github.com/grpc/grpc/blob/master/doc/naming.md
    // Many people (including Bazel), use grpc:// and grpcs://, so we tolerate both.
    // We also accept http:// and https:// for convenience.

    let tls = match uri.scheme_str() {
        Some("grpc") => false,
        Some("grpcs") => true,
        Some("http") => false,
        Some("https") => true,
        Some("dns") | Some("ipv4") | Some("ipv6") | None => true,
        Some(scheme) => {
            return Err(anyhow::anyhow!(
                "Invalid URI scheme: `{}` for `{}` (expected one of grpc, grpcs, http, https, dns, ipv4, ipv6, or no scheme)",
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
        parts.path_and_query = Some(http::uri::PathAndQuery::from_static("/"));
    }

    Ok((Uri::from_parts(parts)?, tls))
}

/// Contains information queried from the Remote Execution Capabilities service.
pub struct RECapabilities {
    /// Largest size of a message before being uploaded using bytestream service.
    /// 0 indicates no limit beyond constraint of underlying transport (which is unknown).
    max_total_batch_size: usize,
    /// Compressors supported by the "compressed-blobs" bytestream resources.
    supported_compressors: Vec<Compressor>,
    /// Digest functions supported by the remote cache/execution capabilities.
    supported_digest_functions: Vec<digest_function::Value>,
}

/// Contains runtime options for the remote execution client as set under `buck2_re_client`
pub struct RERuntimeOpts {
    /// Use the Meta version of the request metadata
    use_fbcode_metadata: bool,
    /// Maximum number of concurrent upload requests.
    max_concurrent_uploads_per_action: Option<usize>,
    /// Time that digests are assumed to live in CAS after being touched.
    cas_ttl_secs: i64,
    /// Maximum number of digests per `FindMissingBlobs` RPC.
    find_missing_blobs_batch_size: usize,
    /// Number of retries to apply for transient gRPC errors.
    retries: usize,
    /// Maximum delay between retry attempts.
    retry_max_delay_ms: u64,
    /// Digest function selected from user config and capabilities for download hash validation.
    download_hash_digest_function: Option<digest_function::Value>,
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
            Some(instance_name) => format!("{instance_name}/"),
            None => "".to_owned(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub enum Compressor {
    Zstd,
    Deflate,
    Brotli,
}

impl Compressor {
    fn from_grpc(val: i32) -> Option<Self> {
        if val == compressor::Value::Zstd as i32 {
            Some(Self::Zstd)
        } else if val == compressor::Value::Deflate as i32 {
            Some(Self::Deflate)
        } else if val == compressor::Value::Brotli as i32 {
            Some(Self::Brotli)
        } else {
            None
        }
    }

    /// The compressor name used in compressed-blob resource paths
    fn name(&self) -> &str {
        match self {
            Self::Zstd => "zstd",
            Self::Deflate => "deflate",
            Self::Brotli => "brotli",
        }
    }
}

fn digest_function_from_grpc(val: i32) -> Option<digest_function::Value> {
    let value = digest_function::Value::try_from(val).ok()?;
    if value == digest_function::Value::Unknown {
        None
    } else {
        Some(value)
    }
}

fn parse_configured_digest_function(value: &str) -> Option<digest_function::Value> {
    match value.trim().to_ascii_uppercase().as_str() {
        "SHA1" => Some(digest_function::Value::Sha1),
        "SHA256" => Some(digest_function::Value::Sha256),
        // RE API only has BLAKE3 (not keyed); map config tokens to that capability.
        "BLAKE3" | "BLAKE3-KEYED" => Some(digest_function::Value::Blake3),
        _ => None,
    }
}

fn supports_hash_validation(digest_function: digest_function::Value) -> bool {
    matches!(
        digest_function,
        digest_function::Value::Sha1
            | digest_function::Value::Sha256
            | digest_function::Value::Blake3
    )
}

fn select_download_hash_digest_function(
    configured_digest_algorithms: &[String],
    supported_digest_functions: &[digest_function::Value],
) -> anyhow::Result<Option<digest_function::Value>> {
    let mut configured = vec![];
    for configured_algorithm in configured_digest_algorithms {
        match parse_configured_digest_function(configured_algorithm) {
            Some(digest_function) if supports_hash_validation(digest_function) => {
                configured.push(digest_function);
            }
            _ => {
                tracing::debug!(
                    "Ignoring unsupported digest_algorithms entry for download validation: `{}`",
                    configured_algorithm
                );
            }
        }
    }
    let mut configured_dedup = vec![];
    for digest_function in configured {
        if !configured_dedup.contains(&digest_function) {
            configured_dedup.push(digest_function);
        }
    }
    let configured = configured_dedup;

    let mut supported = supported_digest_functions
        .iter()
        .copied()
        .filter(|digest_function| supports_hash_validation(*digest_function))
        .collect::<Vec<_>>();
    supported.sort_unstable();
    supported.dedup();

    if !configured.is_empty() {
        if supported.is_empty() {
            return Ok(configured.first().copied());
        }
        for configured_digest_function in &configured {
            if supported.contains(configured_digest_function) {
                return Ok(Some(*configured_digest_function));
            }
        }
        return Err(anyhow::anyhow!(
            "Configured digest_algorithms are incompatible with RE server capabilities. configured={:?}, server={:?}",
            configured,
            supported
        ));
    }

    if supported.len() == 1 {
        Ok(supported.first().copied())
    } else {
        Ok(None)
    }
}

pub struct REClientBuilder;

impl REClientBuilder {
    pub async fn build_and_connect(opts: &Buck2OssReConfiguration) -> anyhow::Result<REClient> {
        // Channels infer TLS from the address scheme, and read the TLS files the
        // first time one needs them.
        let tls_config = Arc::new(tokio::sync::OnceCell::new());
        let channel_settings = GrpcChannelSettings::from_options(opts);
        let connector = |address: &str| {
            GrpcChannelConnector::new(
                address.to_owned(),
                channel_settings.clone(),
                tls_config.clone(),
            )
        };

        // Connect a single channel for fetching capabilities, so that an
        // unreachable remote fails early. Other channels are created on-demand
        // through the connection pool.
        let engine_address = opts.engine_address.as_ref().context("No engine address")?;
        let capabilities_channel = connector(engine_address)
            .connect()
            .await
            .context("Error creating Capabilities channel")?;

        let interceptor = InjectHeadersInterceptor::new(&opts.http_headers)?;

        let mut capabilities_client =
            CapabilitiesClient::with_interceptor(capabilities_channel, interceptor.dupe());

        if let Some(max_decoding_message_size) = opts.max_decoding_message_size {
            capabilities_client =
                capabilities_client.max_decoding_message_size(max_decoding_message_size);
        }

        let instance_name = InstanceName(opts.instance_name.clone());
        let retries = opts.retries.unwrap_or(DEFAULT_RETRIES);
        let retry_max_delay_ms = opts
            .retry_max_delay_ms
            .unwrap_or(DEFAULT_RETRY_MAX_DELAY_MILLIS);

        let capabilities = if opts.capabilities.unwrap_or(true) {
            Self::fetch_rbe_capabilities(
                &mut capabilities_client,
                &instance_name,
                opts.max_total_batch_size,
                retries,
                retry_max_delay_ms,
            )
            .await?
        } else {
            RECapabilities {
                max_total_batch_size: DEFAULT_MAX_TOTAL_BATCH_SIZE,
                supported_compressors: Vec::new(),
                supported_digest_functions: Vec::new(),
            }
        };

        let download_hash_digest_function = select_download_hash_digest_function(
            &opts.digest_algorithms,
            &capabilities.supported_digest_functions,
        )?;

        let max_decoding_msg_size = opts
            .max_decoding_message_size
            .unwrap_or(capabilities.max_total_batch_size * 2);

        if max_decoding_msg_size < capabilities.max_total_batch_size {
            return Err(anyhow::anyhow!(
                "Attribute `max_decoding_message_size` must always be equal or higher to `max_total_batch_size`"
            ));
        }

        // Choose a ByteStream compressor
        let bystream_compressor = if capabilities
            .supported_compressors
            .contains(&Compressor::Zstd)
        {
            Some(Compressor::Zstd)
        } else if capabilities
            .supported_compressors
            .contains(&Compressor::Brotli)
        {
            Some(Compressor::Brotli)
        } else if capabilities
            .supported_compressors
            .contains(&Compressor::Deflate)
        {
            Some(Compressor::Deflate)
        } else {
            None
        };

        // Extract addresses
        let cas_address = opts.cas_address.clone().context("No CAS address")?;
        let action_cache_address = opts
            .action_cache_address
            .clone()
            .context("No action cache address")?;

        // The pool makes the channels to each address with the same settings.
        let mut make_channels: HashMap<String, MakeChannel> = HashMap::new();
        for address in [&cas_address, engine_address, &action_cache_address] {
            if let hash_map::Entry::Vacant(entry) = make_channels.entry(address.clone()) {
                entry.insert(connector(address).make_channel().await?);
            }
        }

        // Create connection pool
        let min_connections = opts.min_connections.unwrap_or(1).max(1);
        let max_connections = opts.max_connections.unwrap_or(100).max(min_connections);
        let pool_config = PoolConfig {
            min_connections,
            max_connections,
            max_concurrency_per_connection: opts.max_concurrency_per_connection.unwrap_or(100),
        };
        let pool = ChannelPool::new(
            pool_config,
            Box::new(move |address| {
                make_channels
                    .get(address)
                    .cloned()
                    .with_context(|| format!("No channel settings for `{address}`"))
            }),
        );

        Ok(REClient::new(
            RERuntimeOpts {
                use_fbcode_metadata: opts.use_fbcode_metadata,
                max_concurrent_uploads_per_action: opts.max_concurrent_uploads_per_action,
                // NOTE: This is an arbitrary number because RBE does not return information
                // on the TTL of the remote blob.
                cas_ttl_secs: opts.cas_ttl_secs.unwrap_or(3 * 60 * 60),
                find_missing_blobs_batch_size: opts.find_missing_blobs_batch_size.unwrap_or(100),
                retries,
                retry_max_delay_ms,
                download_hash_digest_function,
            },
            capabilities,
            instance_name,
            bystream_compressor,
            pool,
            max_decoding_msg_size,
            interceptor,
            cas_address,
            engine_address.clone(),
            action_cache_address,
        ))
    }

    async fn fetch_rbe_capabilities(
        client: &mut CapabilitiesClient<InterceptedService<Channel, InjectHeadersInterceptor>>,
        instance_name: &InstanceName,
        max_total_batch_size: Option<usize>,
        retries: usize,
        retry_max_delay_ms: u64,
    ) -> anyhow::Result<RECapabilities> {
        // TODO use more of the capabilities of the remote build executor

        let resp = retry_grpc_request(retries, Duration::from_millis(retry_max_delay_ms), || {
            let mut client = client.clone();
            let request = GetCapabilitiesRequest {
                instance_name: instance_name.as_str().to_owned(),
            };
            async move { Ok(client.get_capabilities(request).await?.into_inner()) }
        })
        .await
        .context("Failed to query capabilities of remote")?;

        let supported_compressors = if let Some(cache_cap) = &resp.cache_capabilities {
            cache_cap
                .supported_compressors
                .iter()
                .copied()
                .filter_map(Compressor::from_grpc)
                .collect()
        } else {
            Vec::new()
        };

        let mut supported_digest_functions = vec![];
        if let Some(cache_cap) = &resp.cache_capabilities {
            supported_digest_functions.extend(
                cache_cap
                    .digest_functions
                    .iter()
                    .copied()
                    .filter_map(digest_function_from_grpc),
            );
        }
        if supported_digest_functions.is_empty() {
            if let Some(exec_cap) = &resp.execution_capabilities {
                if exec_cap.digest_functions.is_empty() {
                    if let Some(digest_function) =
                        digest_function_from_grpc(exec_cap.digest_function)
                    {
                        supported_digest_functions.push(digest_function);
                    }
                } else {
                    supported_digest_functions.extend(
                        exec_cap
                            .digest_functions
                            .iter()
                            .copied()
                            .filter_map(digest_function_from_grpc),
                    );
                }
            }
        }
        supported_digest_functions.sort_unstable();
        supported_digest_functions.dedup();

        let max_total_batch_size_from_capabilities: Option<usize> =
            resp.cache_capabilities.as_ref().and_then(|cache_cap| {
                let size = cache_cap.max_batch_total_size_bytes as usize;
                // A value of 0 means no limit is set
                if size != 0 { Some(size) } else { None }
            });

        let max_total_batch_size =
            match (max_total_batch_size_from_capabilities, max_total_batch_size) {
                (Some(cap), Some(config)) => std::cmp::min(cap, config),
                (Some(cap), None) => cap,
                (None, Some(config)) => config,
                (None, None) => DEFAULT_MAX_TOTAL_BATCH_SIZE,
            };

        Ok(RECapabilities {
            max_total_batch_size,
            supported_compressors,
            supported_digest_functions,
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
                    .with_context(|| format!("Invalid key in header: `{key}: {value}`"))?;

                let value = MetadataValue::try_from(&value)
                    .with_context(|| format!("Invalid value in header: `{key}: {value}`"))?;

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

type GrpcService = InterceptedService<PooledChannel, InjectHeadersInterceptor>;

#[derive(Clone)]
struct GrpcTlsSettings {
    tls_ca_certs: Option<String>,
    tls_client_cert: Option<String>,
}

#[derive(Clone)]
struct GrpcChannelSettings {
    tls: GrpcTlsSettings,
    grpc_keepalive_time_secs: u64,
    grpc_keepalive_timeout_secs: u64,
    grpc_keepalive_while_idle: bool,
    tcp_keepalive_secs: Option<u64>,
}

impl GrpcChannelSettings {
    fn from_options(opts: &Buck2OssReConfiguration) -> Self {
        Self {
            tls: GrpcTlsSettings {
                tls_ca_certs: opts.tls_ca_certs.clone(),
                tls_client_cert: opts.tls_client_cert.clone(),
            },
            grpc_keepalive_time_secs: opts
                .grpc_keepalive_time_secs
                .unwrap_or(DEFAULT_GRPC_KEEPALIVE_TIME_SECS),
            grpc_keepalive_timeout_secs: opts
                .grpc_keepalive_timeout_secs
                .unwrap_or(DEFAULT_GRPC_KEEPALIVE_TIMEOUT_SECS),
            grpc_keepalive_while_idle: opts
                .grpc_keepalive_while_idle
                .unwrap_or(DEFAULT_GRPC_KEEPALIVE_WHILE_IDLE),
            tcp_keepalive_secs: opts.tcp_keepalive_secs,
        }
    }
}

#[derive(Clone)]
struct GrpcChannelConnector {
    address: String,
    settings: GrpcChannelSettings,
    tls_config: Arc<tokio::sync::OnceCell<ClientTlsConfig>>,
}

impl GrpcChannelConnector {
    fn new(
        address: String,
        settings: GrpcChannelSettings,
        tls_config: Arc<tokio::sync::OnceCell<ClientTlsConfig>>,
    ) -> Self {
        Self {
            address,
            settings,
            tls_config,
        }
    }

    /// The endpoint, and the address it was made from with variables substituted.
    async fn endpoint(&self) -> anyhow::Result<(Endpoint, String)> {
        let address = substitute_env_vars(&self.address).context("Invalid address")?;
        let uri = address.parse().context("Invalid address")?;
        let (uri, tls) = prepare_uri(uri).context("Invalid URI")?;

        let mut endpoint = Channel::builder(uri);
        if tls {
            let tls_config = self
                .tls_config
                .get_or_try_init(|| async {
                    create_tls_config(&self.settings.tls)
                        .await
                        .context("Invalid TLS config")
                })
                .await?
                .clone();
            endpoint = endpoint.tls_config(tls_config)?;
        }

        endpoint = endpoint
            .http2_keep_alive_interval(Duration::from_secs(self.settings.grpc_keepalive_time_secs))
            .keep_alive_timeout(Duration::from_secs(
                self.settings.grpc_keepalive_timeout_secs,
            ))
            .keep_alive_while_idle(self.settings.grpc_keepalive_while_idle);

        Ok((endpoint, address))
    }

    fn http_connector(&self) -> CountingConnector<HttpConnector> {
        // Since we are creating the HttpConnector ourselves, any TCP settings
        // need to be set here instead of on the endpoint.
        let mut http = HttpConnector::new();
        http.enforce_http(false);
        if let Some(tcp_keepalive_secs) = self.settings.tcp_keepalive_secs {
            http.set_keepalive(Some(Duration::from_secs(tcp_keepalive_secs)));
        }
        CountingConnector::new(http)
    }

    async fn connect(&self) -> anyhow::Result<Channel> {
        let (endpoint, address) = self.endpoint().await?;
        endpoint
            .connect_with_connector(self.http_connector())
            .await
            .with_context(|| format!("Error connecting to `{address}`"))
    }

    /// Makes channels for the connection pool, which connect when first used.
    async fn make_channel(&self) -> anyhow::Result<MakeChannel> {
        let (endpoint, _) = self.endpoint().await?;
        let connector = self.clone();
        Ok(Arc::new(move || {
            endpoint.connect_with_connector_lazy(connector.http_connector())
        }))
    }
}

#[derive(Debug, Copy, Clone)]
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

    pub fn get(&mut self, digest: &TDigest) -> Option<DigestRemoteState> {
        self.clear_if_ttl_expires();
        self.cache.get(digest).copied()
    }

    pub fn put(&mut self, digest: TDigest, state: DigestRemoteState) {
        self.clear_if_ttl_expires();
        self.cache.put(digest, state);
    }
}

pub struct REClient {
    runtime_opts: RERuntimeOpts,
    pool: ChannelPool,
    capabilities: RECapabilities,
    instance_name: InstanceName,
    // buck2 calls find_missing for same blobs
    find_missing_cache: Mutex<FindMissingCache>,
    bystream_compressor: Option<Compressor>,
    max_decoding_msg_size: usize,
    interceptor: InjectHeadersInterceptor,
    cas_address: String,
    engine_address: String,
    action_cache_address: String,
    query_write_status_supported: AtomicBool,
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

        // As an optimization, we can silently skip uploading empty blobs
        if size_in_bytes == 0 {
            return;
        }

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

/// Returns true if an `io::Error` indicates a transport-level disruption that
/// a fresh connection can recover from.
fn is_transient_io_error(err: &std::io::Error) -> bool {
    matches!(
        err.kind(),
        std::io::ErrorKind::BrokenPipe
            | std::io::ErrorKind::ConnectionReset
            | std::io::ErrorKind::ConnectionAborted
            | std::io::ErrorKind::UnexpectedEof
            | std::io::ErrorKind::TimedOut
    )
}

/// Returns true if an error is a transient connection/transport error worth
/// retrying. Walks the error chain checking for:
///   - `tonic::Status` with codes the gRPC retry policy treats as transient
///     (Unavailable, ResourceExhausted, Aborted)
///   - `io::Error` of a kind that indicates a transport-level disruption
///     (BrokenPipe, ConnectionReset/Aborted, UnexpectedEof, TimedOut)
///   - `h2::Error` wrapping such an `io::Error`. `h2::Error` does not
///     implement `source()`, so the inner `io::Error` never appears as its
///     own link in the chain; tonic also maps these errors to
///     `Code::Unknown`. Body errors on an established connection (for
///     example a TCP reset during a `BatchReadBlobs` response) take this
///     path and need the explicit downcast.
///
/// `tonic::transport::Error` is not matched directly — its transient subset
/// surfaces as an `io::Error` somewhere in the chain, which we catch above.
/// Non-transient transport errors (TLS handshake, invalid URI) do not have
/// an `io::Error` source, so they correctly do not retry.
fn is_retryable(err: &anyhow::Error) -> bool {
    for cause in err.chain() {
        if let Some(status) = cause.downcast_ref::<tonic::Status>() {
            match status.code() {
                tonic::Code::Unavailable
                | tonic::Code::ResourceExhausted
                | tonic::Code::Aborted => return true,
                _ => {}
            }
        }
        if let Some(io_err) = cause.downcast_ref::<std::io::Error>() {
            if is_transient_io_error(io_err) {
                return true;
            }
        }
        if let Some(h2_err) = cause.downcast_ref::<h2::Error>() {
            if let Some(io_err) = h2_err.get_io() {
                if is_transient_io_error(io_err) {
                    return true;
                }
            }
        }
    }
    false
}

/// Attaches an RE classification to gRPC errors, preserving the original chain
/// and any existing `REClientError` classification.
fn normalize_re_error(err: anyhow::Error) -> anyhow::Error {
    if err.downcast_ref::<REClientError>().is_some() {
        return err;
    }
    let Some(status) = err.chain().find_map(|cause| {
        cause.downcast_ref::<tonic::Status>().or_else(|| {
            // `io::Error::source()` skips the wrapped error itself.
            cause
                .downcast_ref::<std::io::Error>()?
                .get_ref()?
                .downcast_ref::<tonic::Status>()
        })
    }) else {
        return err;
    };
    let re_error = REClientError {
        code: TCode(status.code() as i32),
        message: status.message().to_owned(),
        group: TCodeReasonGroup::UNKNOWN,
    };
    err.context(re_error)
}

/// Retry an RPC on transient connection errors and classify its final error.
///
/// On retryable failure, the closure is called again from scratch — acquiring
/// a fresh connection from the pool, rebuilding the request, etc. Up to 5
/// attempts with exponential backoff (100ms, 200ms, 400ms, 800ms — capped at
/// 5s) plus 0–50ms of jitter to avoid synchronized retry storms across
/// concurrent in-flight requests.
async fn retry<F, Fut, T>(f: F) -> anyhow::Result<T>
where
    F: Fn() -> Fut,
    Fut: Future<Output = anyhow::Result<T>>,
{
    use rand::RngExt;

    const MAX_ATTEMPTS: u32 = 5;
    const INITIAL_DELAY: Duration = Duration::from_millis(100);
    const MAX_DELAY: Duration = Duration::from_secs(5);

    let mut delay = INITIAL_DELAY;
    for attempt in 1..=MAX_ATTEMPTS {
        match f().await {
            Ok(v) => return Ok(v),
            Err(e) if is_retryable(&e) && attempt < MAX_ATTEMPTS => {
                let jitter = Duration::from_millis(rand::rng().random_range(0..50));
                let sleep_for = delay + jitter;
                tracing::warn!(
                    "Transient error (attempt {}/{}), retrying in {:?}: {:#}",
                    attempt,
                    MAX_ATTEMPTS,
                    sleep_for,
                    e
                );
                tokio::time::sleep(sleep_for).await;
                delay = (delay * 2).min(MAX_DELAY);
            }
            Err(e) => return Err(normalize_re_error(e)),
        }
    }
    unreachable!()
}

impl REClient {
    fn new(
        runtime_opts: RERuntimeOpts,
        capabilities: RECapabilities,
        instance_name: InstanceName,
        bystream_compressor: Option<Compressor>,
        pool: ChannelPool,
        max_decoding_msg_size: usize,
        interceptor: InjectHeadersInterceptor,
        cas_address: String,
        engine_address: String,
        action_cache_address: String,
    ) -> Self {
        let cas_ttl_secs = runtime_opts.cas_ttl_secs;
        REClient {
            runtime_opts,
            pool,
            capabilities,
            instance_name,
            find_missing_cache: Mutex::new(FindMissingCache {
                cache: LruCache::new(NonZeroUsize::new(500_000).unwrap()),
                ttl: Duration::from_secs(cas_ttl_secs.max(0) as u64),
                last_check: Instant::now(),
            }),
            bystream_compressor,
            max_decoding_msg_size,
            interceptor,
            cas_address,
            engine_address,
            action_cache_address,
            query_write_status_supported: AtomicBool::new(true),
        }
    }

    async fn bystream_write_plan(
        &self,
        bytestream_client: &mut ByteStreamClient<GrpcService>,
        metadata: RemoteExecutionMetadata,
        segments: Vec<WriteRequest>,
    ) -> anyhow::Result<BystreamWritePlan> {
        if segments.is_empty() || !self.query_write_status_supported.load(Ordering::Relaxed) {
            return Ok(BystreamWritePlan::Write(segments));
        }

        let resource_name = segments[0].resource_name.clone();
        let total_size = total_bystream_write_size(&segments);

        match bytestream_client
            .query_write_status(with_re_metadata(
                QueryWriteStatusRequest {
                    resource_name: resource_name.clone(),
                },
                &metadata,
                self.runtime_opts.use_fbcode_metadata,
            ))
            .await
        {
            Ok(resp) => {
                let status = resp.into_inner();
                if status.complete || status.committed_size >= total_size {
                    return Ok(BystreamWritePlan::AlreadyCommitted(total_size));
                }

                Ok(BystreamWritePlan::Write(trim_bystream_write_segments(
                    segments,
                    status.committed_size,
                )))
            }
            Err(status) if status.code() == tonic::Code::Unimplemented => {
                self.query_write_status_supported
                    .store(false, Ordering::Relaxed);
                tracing::debug!(
                    resource_name = %resource_name,
                    "Bytestream QueryWriteStatus is not supported by server; disabling resume probes"
                );
                Ok(BystreamWritePlan::Write(segments))
            }
            Err(status) => {
                tracing::debug!(
                    resource_name = %resource_name,
                    code = ?status.code(),
                    "Bytestream QueryWriteStatus failed; retrying write from offset 0"
                );
                Ok(BystreamWritePlan::Write(segments))
            }
        }
    }

    pub fn get_action_result<'a>(
        &'a self,
        metadata: &RemoteExecutionMetadata,
        request: ActionResultRequest,
    ) -> impl Future<Output = anyhow::Result<ActionResultResponse>> + Send + use<'a> {
        let action_digest = tdigest_to(&request.digest);
        let metadata = metadata.clone();
        async move {
            let res = retry_grpc_request(
                self.runtime_opts.retries,
                Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                || {
                    let metadata = metadata.clone();
                    let action_digest = action_digest.clone();
                    async move {
                        let mut client = self.action_cache_client().await?;
                        client
                            .get_action_result(with_re_metadata(
                                GetActionResultRequest {
                                    instance_name: self.instance_name.as_str().to_owned(),
                                    action_digest: Some(action_digest),
                                    ..Default::default()
                                },
                                &metadata,
                                self.runtime_opts.use_fbcode_metadata,
                            ))
                            .await
                            .map_err(anyhow::Error::from)
                    }
                },
            )
            .await?;

            Ok(ActionResultResponse {
                action_result: convert_action_result(
                    res.into_inner(),
                    self.runtime_opts.cas_ttl_secs,
                )?,
                ttl: self.runtime_opts.cas_ttl_secs,
            })
        }
    }

    pub fn write_action_result<'a>(
        &'a self,
        metadata: &RemoteExecutionMetadata,
        request: &WriteActionResultRequest,
    ) -> impl Future<Output = anyhow::Result<WriteActionResultResponse>> + Send + use<'a> {
        let action_result = convert_t_action_result2(&request.action_result);
        let action_digest = tdigest_to(&request.action_digest);
        let metadata = metadata.clone();
        async move {
            let action_result = action_result?;
            let res = retry_grpc_request(
                self.runtime_opts.retries,
                Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                || {
                    let metadata = metadata.clone();
                    let action_digest = action_digest.clone();
                    let action_result = action_result.clone();
                    async move {
                        let mut client = self.action_cache_client().await?;
                        client
                            .update_action_result(with_re_metadata(
                                UpdateActionResultRequest {
                                    instance_name: self.instance_name.as_str().to_owned(),
                                    action_digest: Some(action_digest),
                                    action_result: Some(action_result),
                                    results_cache_policy: None,
                                    ..Default::default()
                                },
                                &metadata,
                                self.runtime_opts.use_fbcode_metadata,
                            ))
                            .await
                            .map_err(anyhow::Error::from)
                    }
                },
            )
            .await?;

            Ok(WriteActionResultResponse {
                actual_action_result: convert_action_result(
                    res.into_inner(),
                    self.runtime_opts.cas_ttl_secs,
                )?,
                ttl_seconds: self.runtime_opts.cas_ttl_secs,
            })
        }
    }

    pub async fn execute_with_progress(
        &self,
        metadata: &RemoteExecutionMetadata,
        mut execute_request: ExecuteRequest,
    ) -> anyhow::Result<BoxStream<'static, anyhow::Result<ExecuteWithProgressResponse>>> {
        // TODO(aloiscochard): Map those properly in the request
        // use crate::proto::build::bazel::remote::execution::v2::ExecutionPolicy;

        let action_digest = tdigest_to(&execute_request.action_digest);
        let priority = execute_request
            .execution_policy
            .map(|ep| ep.priority)
            .unwrap_or_default();

        let grpc_request = GExecuteRequest {
            instance_name: self.instance_name.as_str().to_owned(),
            skip_cache_lookup: execute_request.skip_cache_lookup,
            execution_policy: Some(ExecutionPolicy { priority }),
            results_cache_policy: Some(ResultsCachePolicy { priority: 0 }),
            action_digest: Some(action_digest.clone()),
            ..Default::default()
        };

        let stream = retry_grpc_request(
            self.runtime_opts.retries,
            Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
            || {
                let metadata = metadata.clone();
                let request = grpc_request.clone();
                async move {
                    let mut client = self.execution_client().await?;
                    Ok(client
                        .execute(with_re_metadata(
                            request,
                            &metadata,
                            self.runtime_opts.use_fbcode_metadata,
                        ))
                        .await?
                        .into_inner())
                }
            },
        )
        .await?;

        let cas_ttl_secs = self.runtime_opts.cas_ttl_secs;

        let stream = futures::stream::try_unfold(stream, move |mut stream| async move {
            let msg = match stream
                .try_next()
                .await
                .context("RE channel error")
                .map_err(normalize_re_error)?
            {
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

                        let action_result = convert_action_result(action_result, cas_ttl_secs)?;

                        let execute_response = ExecuteResponse {
                            action_result,
                            action_result_digest: TDigest::default(),
                            action_result_ttl: cas_ttl_secs,
                            status: TStatus {
                                code: TCode::OK,
                                message: execute_response_grpc.message,
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

                let stage = match execution_stage::Value::try_from(meta.stage) {
                    Ok(execution_stage::Value::Unknown) => Stage::UNKNOWN,
                    Ok(execution_stage::Value::CacheCheck) => Stage::CACHE_CHECK,
                    Ok(execution_stage::Value::Queued) => Stage::QUEUED,
                    Ok(execution_stage::Value::Executing) => Stage::EXECUTING,
                    Ok(execution_stage::Value::Completed) => Stage::COMPLETED,
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
                    execute_response: Some(response),
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

    pub fn upload<'a>(
        &'a self,
        metadata: &RemoteExecutionMetadata,
        request: UploadRequest,
    ) -> impl Future<Output = anyhow::Result<UploadResponse>> + Send + use<'a> {
        let metadata = metadata.clone();
        async move {
            upload_impl(
                &self.instance_name,
                request,
                self.bystream_compressor,
                self.capabilities.max_total_batch_size,
                self.runtime_opts.max_concurrent_uploads_per_action,
                |re_request| {
                    let metadata = metadata.clone();
                    async move {
                        retry_grpc_request(
                            self.runtime_opts.retries,
                            Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                            || {
                                let metadata = metadata.clone();
                                let re_request = re_request.clone();
                                async move {
                                    let mut cas_client = self.cas_client().await?;
                                    Ok(cas_client
                                        .batch_update_blobs(with_re_metadata(
                                            re_request,
                                            &metadata,
                                            self.runtime_opts.use_fbcode_metadata,
                                        ))
                                        .await?
                                        .into_inner())
                                }
                            },
                        )
                        .await
                    }
                },
                |segments| {
                    let metadata = metadata.clone();
                    async move {
                        retry_grpc_request(
                            self.runtime_opts.retries,
                            Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                            || {
                                let metadata = metadata.clone();
                                let segments = segments.clone();
                                async move {
                                    let mut bytestream_client = self.bytestream_client().await?;
                                    let segments = match self
                                        .bystream_write_plan(
                                            &mut bytestream_client,
                                            metadata.clone(),
                                            segments,
                                        )
                                        .await?
                                    {
                                        BystreamWritePlan::Write(segments) => segments,
                                        BystreamWritePlan::AlreadyCommitted(committed_size) => {
                                            return Ok(WriteResponse { committed_size });
                                        }
                                    };
                                    let requests = futures::stream::iter(segments);
                                    Ok(bytestream_client
                                        .write(with_re_metadata(
                                            requests,
                                            &metadata,
                                            self.runtime_opts.use_fbcode_metadata,
                                        ))
                                        .await?
                                        .into_inner())
                                }
                            },
                        )
                        .await
                    }
                },
            )
            .await
        }
    }

    pub fn upload_blob_with_digest<'a>(
        &'a self,
        blob: Vec<u8>,
        digest: TDigest,
        metadata: &RemoteExecutionMetadata,
    ) -> impl Future<Output = anyhow::Result<TDigest>> + Send + use<'a> {
        let blob = InlinedBlobWithDigest {
            digest: digest.clone(),
            blob,
            ..Default::default()
        };
        let upload = self.upload(
            metadata,
            UploadRequest {
                inlined_blobs_with_digest: Some(vec![blob]),
                files_with_digest: None,
                directories: None,
                upload_only_missing: false,
                ..Default::default()
            },
        );
        async move {
            upload.await?;
            Ok(digest)
        }
    }

    pub fn download<'a>(
        &'a self,
        metadata: &RemoteExecutionMetadata,
        request: DownloadRequest,
    ) -> impl Future<Output = anyhow::Result<DownloadResponse>> + Send + use<'a> {
        let metadata = metadata.clone();
        async move {
            download_impl(
                &self.instance_name,
                request,
                self.bystream_compressor,
                self.capabilities.max_total_batch_size,
                self.runtime_opts.download_hash_digest_function,
                |re_request| {
                    let metadata = metadata.clone();
                    async move {
                        retry_grpc_request(
                            self.runtime_opts.retries,
                            Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                            || {
                                let metadata = metadata.clone();
                                let re_request = re_request.clone();
                                async move {
                                    let mut client = self.cas_client().await?;
                                    Ok(client
                                        .batch_read_blobs(with_re_metadata(
                                            re_request,
                                            &metadata,
                                            self.runtime_opts.use_fbcode_metadata,
                                        ))
                                        .await?
                                        .into_inner())
                                }
                            },
                        )
                        .await
                    }
                },
                |read_request| {
                    let metadata = metadata.clone();
                    async move {
                        let response = retry_grpc_request(
                            self.runtime_opts.retries,
                            Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                            || {
                                let metadata = metadata.clone();
                                let read_request = read_request.clone();
                                async move {
                                    let mut client = self.bytestream_client().await?;
                                    Ok(client
                                        .read(with_re_metadata(
                                            read_request,
                                            &metadata,
                                            self.runtime_opts.use_fbcode_metadata,
                                        ))
                                        .await?
                                        .into_inner())
                                }
                            },
                        )
                        .await?;
                        Ok(Box::pin(response.into_stream()))
                    }
                },
            )
            .await
        }
    }

    pub async fn get_digests_ttl(
        &self,
        metadata: &RemoteExecutionMetadata,
        request: GetDigestsTtlRequest,
    ) -> anyhow::Result<GetDigestsTtlResponse> {
        let mut remote_results: HashMap<TDigest, DigestRemoteState> = HashMap::new();
        let mut digests_to_check: Vec<TDigest> = Vec::new();

        let batch_size = self.runtime_opts.find_missing_blobs_batch_size;
        let mut digest_iter = request.digests.iter();
        while digest_iter.len() > 0 {
            // Sort our blobs based on what action we need to take
            {
                let mut find_missing_cache = self.find_missing_cache.lock().unwrap();
                for digest in digest_iter.by_ref() {
                    if let Some(rs) = find_missing_cache.get(digest) {
                        // We have our final result already cached
                        remote_results.insert(digest.clone(), rs);
                    } else {
                        // We can check this blob
                        digests_to_check.push(digest.clone());
                    }
                    if digests_to_check.len() >= batch_size {
                        break;
                    }
                }
            }

            // Send a request and notify others of the result
            if !digests_to_check.is_empty() {
                tracing::debug!(num_digests = digests_to_check.len(), "FindMissingBlobs");
                let blobs_to_check = digests_to_check.clone();
                let missing_blobs = retry_grpc_request(
                    self.runtime_opts.retries,
                    Duration::from_millis(self.runtime_opts.retry_max_delay_ms),
                    || {
                        let metadata = metadata.clone();
                        let blobs_to_check = blobs_to_check.clone();
                        async move {
                            let mut cas_client = self.cas_client().await?;
                            cas_client
                                .find_missing_blobs(with_re_metadata(
                                    FindMissingBlobsRequest {
                                        instance_name: self.instance_name.as_str().to_owned(),
                                        blob_digests: blobs_to_check
                                            .iter()
                                            .map(tdigest_to)
                                            .collect(),
                                        ..Default::default()
                                    },
                                    &metadata,
                                    self.runtime_opts.use_fbcode_metadata,
                                ))
                                .await
                                .map_err(anyhow::Error::from)
                        }
                    },
                )
                .await
                .context("Failed to request what blobs are not present on remote")?;
                let resp: FindMissingBlobsResponse = missing_blobs.into_inner();

                // Build the set of missing digests first, so we don't
                // prematurely cache them as ExistsOnRemote.
                let missing: HashSet<TDigest> = resp
                    .missing_blob_digests
                    .iter()
                    .map(|d| tdigest_from(d.clone()))
                    .collect();

                let mut find_missing_cache = self.find_missing_cache.lock().unwrap();
                for digest in &digests_to_check {
                    if missing.contains(digest) {
                        // Missing is a transient state: the blob may be uploaded
                        // or produced by an RE action at any time.  Do NOT cache
                        // it -- a stale "Missing" entry would prevent future
                        // FindMissingBlobs RPCs, and a premature "ExistsOnRemote"
                        // entry would cause concurrent actions to skip uploading
                        // a blob that hasn't been uploaded yet.
                        remote_results.insert(digest.clone(), DigestRemoteState::Missing);
                    } else {
                        remote_results.insert(digest.clone(), DigestRemoteState::ExistsOnRemote);
                        find_missing_cache.put(digest.clone(), DigestRemoteState::ExistsOnRemote);
                    }
                }
                digests_to_check.clear();
            }
        }

        Ok(GetDigestsTtlResponse {
            digests_with_ttl: remote_results
                .iter()
                .map(|(digest, rs)| match rs {
                    DigestRemoteState::Missing => DigestWithTtl {
                        digest: digest.clone(),
                        ttl: 0,
                    },
                    DigestRemoteState::ExistsOnRemote => DigestWithTtl {
                        digest: digest.clone(),
                        ttl: self.runtime_opts.cas_ttl_secs,
                    },
                })
                .collect::<Vec<DigestWithTtl>>(),
        })
    }

    pub async fn extend_digest_ttl(
        &self,
        _metadata: &RemoteExecutionMetadata,
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

    async fn cas_client(&self) -> anyhow::Result<ContentAddressableStorageClient<GrpcService>> {
        let channel = self.pool.get(&self.cas_address).await?;
        Ok(
            ContentAddressableStorageClient::new(InterceptedService::new(
                channel,
                self.interceptor.dupe(),
            ))
            .max_decoding_message_size(self.max_decoding_msg_size),
        )
    }

    async fn bytestream_client(&self) -> anyhow::Result<ByteStreamClient<GrpcService>> {
        let channel = self.pool.get(&self.cas_address).await?;
        Ok(
            ByteStreamClient::new(InterceptedService::new(channel, self.interceptor.dupe()))
                .max_decoding_message_size(self.max_decoding_msg_size),
        )
    }

    async fn execution_client(&self) -> anyhow::Result<ExecutionClient<GrpcService>> {
        let channel = self.pool.get(&self.engine_address).await?;
        Ok(ExecutionClient::new(InterceptedService::new(
            channel,
            self.interceptor.dupe(),
        )))
    }

    async fn action_cache_client(&self) -> anyhow::Result<ActionCacheClient<GrpcService>> {
        let channel = self.pool.get(&self.action_cache_address).await?;
        Ok(ActionCacheClient::new(InterceptedService::new(
            channel,
            self.interceptor.dupe(),
        )))
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

    pub fn release_temporary_memory(&self) -> anyhow::Result<()> {
        Ok(())
    }
}

fn convert_action_result(
    action_result: ActionResult,
    cas_ttl_secs: i64,
) -> anyhow::Result<TActionResult2> {
    let execution_metadata = action_result
        .execution_metadata
        .with_context(|| "The execution metadata are not defined.")?;

    let output_files = action_result
        .output_files
        .into_iter()
        .map(|output_file| {
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
                ttl: cas_ttl_secs,
                _dot_dot_default: (),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let output_symlinks = action_result
        .output_symlinks
        .into_iter()
        .map(|output_symlink| {
            anyhow::Ok(TSymlink {
                name: output_symlink.path,
                target: output_symlink.target,
                _dot_dot_default: (),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let output_directories = action_result
        .output_directories
        .into_iter()
        .map(|output_directory| {
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
        })
        .collect::<Result<Vec<_>, _>>()?;

    let action_result = TActionResult2 {
        output_files,
        output_symlinks,
        output_directories,
        exit_code: action_result.exit_code,
        stdout_raw: Some(action_result.stdout_raw),
        stdout_digest: action_result.stdout_digest.map(tdigest_from),
        stderr_raw: Some(action_result.stderr_raw),
        stderr_digest: action_result.stderr_digest.map(tdigest_from),

        execution_metadata: Box::new(TExecutedActionMetadata {
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
            auxiliary_metadata: execution_metadata
                .auxiliary_metadata
                .into_iter()
                .map(tany_from)
                .collect::<Vec<_>>(),
            input_analyzing_start_timestamp: Default::default(),
            input_analyzing_completed_timestamp: Default::default(),
            execution_dir: "".to_owned(),
            execution_attempts: 0,
            last_queued_timestamp: Default::default(),
            ..Default::default()
        }),
        ..Default::default()
    };

    Ok(action_result)
}

fn convert_t_action_result2(t_action_result: &TActionResult2) -> anyhow::Result<ActionResult> {
    let t_execution_metadata = &t_action_result.execution_metadata;
    let virtual_execution_duration = prost_types::Duration::try_from(
        t_execution_metadata
            .execution_completed_timestamp
            .saturating_duration_since(&t_execution_metadata.execution_start_timestamp),
    )?;
    let execution_metadata = Some(ExecutedActionMetadata {
        worker: t_execution_metadata.worker.clone(),
        queued_timestamp: Some(ttimestamp_to(&t_execution_metadata.queued_timestamp)),
        worker_start_timestamp: Some(ttimestamp_to(&t_execution_metadata.worker_start_timestamp)),
        worker_completed_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.worker_completed_timestamp,
        )),
        input_fetch_start_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.input_fetch_start_timestamp,
        )),
        input_fetch_completed_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.input_fetch_completed_timestamp,
        )),
        execution_start_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.execution_start_timestamp,
        )),
        execution_completed_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.execution_completed_timestamp,
        )),
        virtual_execution_duration: Some(virtual_execution_duration),
        output_upload_start_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.output_upload_start_timestamp,
        )),
        output_upload_completed_timestamp: Some(ttimestamp_to(
            &t_execution_metadata.output_upload_completed_timestamp,
        )),
        auxiliary_metadata: t_execution_metadata
            .auxiliary_metadata
            .iter()
            .map(tany_to)
            .collect::<Vec<_>>(),
    });

    let output_files = t_action_result
        .output_files
        .iter()
        .map(|output_file| OutputFile {
            path: output_file.name.clone(),
            digest: Some(tdigest_to(&output_file.digest.digest)),
            is_executable: output_file.executable,
            contents: Vec::new(),
            node_properties: None,
        })
        .collect::<Vec<_>>();

    let output_symlinks = t_action_result
        .output_symlinks
        .iter()
        .map(|output_symlink| OutputSymlink {
            path: output_symlink.name.clone(),
            target: output_symlink.target.clone(),
            node_properties: None,
        })
        .collect::<Vec<_>>();

    let output_directories = t_action_result
        .output_directories
        .iter()
        .map(|output_directory| {
            let digest = tdigest_to(&output_directory.tree_digest);
            OutputDirectory {
                path: output_directory.path.clone(),
                tree_digest: Some(digest.clone()),
                is_topologically_sorted: false,
                root_directory_digest: None,
            }
        })
        .collect::<Vec<_>>();

    let action_result = ActionResult {
        output_files,
        output_symlinks,
        output_directories,
        exit_code: t_action_result.exit_code,
        stdout_raw: Vec::new(),
        stdout_digest: t_action_result.stdout_digest.as_ref().map(tdigest_to),
        stderr_raw: Vec::new(),
        stderr_digest: t_action_result.stderr_digest.as_ref().map(tdigest_to),
        execution_metadata,
        ..Default::default()
    };

    Ok(action_result)
}

async fn download_impl<Byt, BytRet, Cas>(
    instance_name: &InstanceName,
    request: DownloadRequest,
    bystream_compressor: Option<Compressor>,
    max_total_batch_size: usize,
    download_hash_digest_function: Option<digest_function::Value>,
    cas_f: impl Fn(BatchReadBlobsRequest) -> Cas,
    bystream_fut: impl Fn(ReadRequest) -> Byt + Sync + Send + Copy,
) -> anyhow::Result<DownloadResponse>
where
    Byt: Future<Output = anyhow::Result<Pin<Box<BytRet>>>>,
    BytRet: Stream<Item = Result<ReadResponse, tonic::Status>> + Send,
    Cas: Future<Output = anyhow::Result<BatchReadBlobsResponse>>,
{
    fn resource_name(
        instance_name: &InstanceName,
        compressor: Option<Compressor>,
        digest: &TDigest,
    ) -> String {
        if let Some(compressor) = compressor {
            format!(
                "{}compressed-blobs/{}/{}/{}",
                instance_name.as_resource_prefix(),
                compressor.name(),
                digest.hash,
                digest.size_in_bytes,
            )
        } else {
            format!(
                "{}blobs/{}/{}",
                instance_name.as_resource_prefix(),
                digest.hash,
                digest.size_in_bytes,
            )
        }
    }

    let bystream_fut = |digest: TDigest| async move {
        let resource_name = resource_name(instance_name, bystream_compressor, &digest);

        bystream_fut(ReadRequest {
            resource_name: resource_name.clone(),
            read_offset: 0,
            read_limit: 0,
        })
        .await
        // adapt the tokio Stream of ReadResponse into a StreamReader
        .map(|p| {
            let blob_reader = StreamReader::new(
                p.map(|r| r.map(|rr| Cursor::new(rr.data)).map_err(io::Error::other)),
            );
            let reader: Pin<Box<dyn AsyncRead + Unpin + Send>> = match bystream_compressor {
                None => Pin::new(Box::new(blob_reader)),
                Some(Compressor::Zstd) => {
                    let mut decoder = ZstdDecoder::new(blob_reader);
                    decoder.multiple_members(true);
                    Pin::new(Box::new(decoder))
                }
                Some(Compressor::Deflate) => {
                    let mut decoder = DeflateDecoder::new(blob_reader);
                    decoder.multiple_members(true);
                    Pin::new(Box::new(decoder))
                }
                Some(Compressor::Brotli) => {
                    let mut decoder = BrotliDecoder::new(blob_reader);
                    decoder.multiple_members(true);
                    Pin::new(Box::new(decoder))
                }
            };

            reader
        })
        .with_context(|| format!("Failed to read {resource_name} from Bytestream service"))
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
        .map(tdigest_to)
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
                ..Default::default()
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
            ..Default::default()
        };
        requests.push(read_blob_req);
    }

    let mut batched_blobs_response = HashMap::new();
    for read_blob_req in requests {
        let resp = retry(|| async {
            cas_f(read_blob_req.clone())
                .await
                .context("Failed to make BatchReadBlobs request")
        })
        .await?;
        for r in resp.responses.into_iter() {
            let digest = tdigest_from(r.digest.context("Response digest not found.")?);
            check_status(r.status.unwrap_or_default())?;
            batched_blobs_response.insert(digest, r.data);
        }
    }

    let get = |digest: &TDigest| -> anyhow::Result<Vec<u8>> {
        if digest.size_in_bytes == 0 {
            validate_downloaded_blob(digest, &[], download_hash_digest_function)?;
            return Ok(Vec::new());
        }

        let data = batched_blobs_response
            .get(digest)
            .with_context(|| format!("Did not receive digest data for `{digest}`"))?
            .clone();
        validate_downloaded_blob(digest, &data, download_hash_digest_function)?;
        Ok(data)
    };

    let mut inlined_blobs = vec![];
    for digest in inlined_digests {
        let data = if digest.size_in_bytes as usize >= max_total_batch_size {
            let mut accum = vec![];
            let mut reader = bystream_fut(digest.clone()).await?;
            tokio::io::copy(&mut reader, &mut accum).await?;
            validate_downloaded_blob(&digest, &accum, download_hash_digest_function)?;
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
        opts.read(true).write(true).create(true).truncate(true);
        #[cfg(unix)]
        {
            if req.is_executable {
                opts.mode(0o755);
            } else {
                opts.mode(0o644);
            }
        }

        retry(|| async {
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
                let mut reader = bystream_fut(req.named_digest.digest.clone()).await?;
                let mut hash_validators = BlobHashValidators::new(
                    &req.named_digest.digest.hash,
                    download_hash_digest_function,
                )?;
                let mut copied_bytes = 0usize;
                let mut buffer = vec![0u8; 64 * 1024];
                loop {
                    let read_bytes = reader.read(&mut buffer).await.with_context(|| {
                        format!("Error reading chunk of: {}", req.named_digest.digest)
                    })?;
                    if read_bytes == 0 {
                        break;
                    }
                    copied_bytes = copied_bytes.checked_add(read_bytes).with_context(|| {
                        format!(
                            "Downloaded blob is too large to validate on this platform: {}",
                            req.named_digest.digest
                        )
                    })?;
                    hash_validators.update(&buffer[..read_bytes]);
                    file.write_all(&buffer[..read_bytes])
                        .await
                        .with_context(|| {
                            format!("Error writing chunk of: {}", req.named_digest.digest)
                        })?;
                }
                validate_downloaded_blob_size(&req.named_digest.digest, copied_bytes)?;
                hash_validators.finish(&req.named_digest.digest)?;
            }
            file.flush().await.context("Error flushing")?;
            anyhow::Ok(())
        })
        .await
        .with_context(|| {
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
    bystream_compressor: Option<Compressor>,
    max_total_batch_size: usize,
    max_concurrent_uploads: Option<usize>,
    cas_f: impl Fn(BatchUpdateBlobsRequest) -> Cas + Sync + Send + Copy,
    bystream_fut: impl Fn(Vec<WriteRequest>) -> Byt + Sync + Send + Copy,
) -> anyhow::Result<UploadResponse>
where
    Cas: Future<Output = anyhow::Result<BatchUpdateBlobsResponse>> + Send,
    Byt: Future<Output = anyhow::Result<WriteResponse>> + Send,
{
    fn resource_name(
        instance_name: &InstanceName,
        client_uuid: &str,
        compressor: Option<Compressor>,
        digest: &TDigest,
    ) -> String {
        if let Some(compressor) = compressor {
            format!(
                "{}uploads/{}/compressed-blobs/{}/{}/{}",
                instance_name.as_resource_prefix(),
                client_uuid,
                compressor.name(),
                digest.hash,
                digest.size_in_bytes,
            )
        } else {
            format!(
                "{}uploads/{}/blobs/{}/{}",
                instance_name.as_resource_prefix(),
                client_uuid,
                digest.hash,
                digest.size_in_bytes,
            )
        }
    }

    // NOTE if we stop recording blob_hashes, we can drop out a lot of allocations.
    let mut upload_futures: Vec<BoxFuture<anyhow::Result<Vec<String>>>> = vec![];

    // For small file uploads the client should group them together and call `BatchUpdateBlobs`
    // https://github.com/bazelbuild/remote-apis/blob/main/build/bazel/remote/execution/v2/remote_execution.proto#L205
    let mut batched_blob_updates = BatchUploadReqAggregator::new(max_total_batch_size);

    // Adapt the given bystream_fut to take in an AsyncBufRead
    let bystream_fut = |resource_name: String, reader: Box<dyn AsyncBufRead + Unpin + Send>| async move {
        let mut reader: Pin<Box<dyn AsyncRead + Unpin + Send>> = match bystream_compressor {
            None => Pin::new(Box::new(reader)),
            Some(Compressor::Zstd) => Pin::new(Box::new(ZstdEncoder::new(reader))),
            Some(Compressor::Deflate) => Pin::new(Box::new(DeflateEncoder::new(reader))),
            Some(Compressor::Brotli) => Pin::new(Box::new(BrotliEncoder::new(reader))),
        };

        let mut current_offset = 0;
        let mut upload_segments = Vec::new();
        let mut buf = vec![0; max_total_batch_size];
        loop {
            let n_read = reader
                .read(&mut buf)
                .await
                .with_context(|| format!("Failed reading upload source for `{resource_name}`"))?;
            if n_read == 0 {
                break;
            }
            upload_segments.push(WriteRequest {
                resource_name: resource_name.clone(),
                write_offset: current_offset,
                finish_write: false,
                data: buf[0..n_read].to_vec(),
            });
            current_offset += n_read as i64;
        }
        if let Some(last_segment) = upload_segments.last_mut() {
            last_segment.finish_write = true;
        }

        if upload_segments.is_empty() {
            // As an optimization, we can silently skip uploading empty blobs
            return Ok(());
        }

        let response = bystream_fut(upload_segments).await?;
        if response.committed_size != current_offset && response.committed_size != -1 {
            return Err(anyhow::anyhow!(
                "Failed to upload `{resource_name}`: invalid committed_size from WriteResponse"
            ));
        }

        Ok(())
    };

    // Create futures for any blobs that need uploading.
    for blob in request.inlined_blobs_with_digest.unwrap_or_default() {
        let hash = blob.digest.hash.clone();
        let size = blob.digest.size_in_bytes;

        if size < max_total_batch_size as i64 {
            batched_blob_updates.push(BatchUploadRequest::Blob(blob));
            continue;
        }

        let data = prost::bytes::Bytes::from(blob.blob);
        let client_uuid = uuid::Uuid::new_v4().to_string();
        let resource_name = resource_name(
            instance_name,
            &client_uuid,
            bystream_compressor,
            &blob.digest,
        );
        let fut = async move {
            retry(|| async {
                bystream_fut(resource_name.clone(), Box::new(Cursor::new(data.clone()))).await?;
                Ok(vec![hash.clone()])
            })
            .await
        };
        upload_futures.push(Box::pin(fut));
    }

    // Create futures for any files that needs uploading.
    for file in request.files_with_digest.unwrap_or_default() {
        let hash = file.digest.hash.clone();
        let size = file.digest.size_in_bytes;
        let name = file.name.clone();
        if size < max_total_batch_size as i64 {
            batched_blob_updates.push(BatchUploadRequest::File(file));
            continue;
        }
        let client_uuid = uuid::Uuid::new_v4().to_string();
        let resource_name = resource_name(
            instance_name,
            &client_uuid,
            bystream_compressor,
            &file.digest,
        );

        let fut = async move {
            retry(|| async {
                let file = tokio::fs::File::open(&name)
                    .await
                    .with_context(|| format!("Opening `{name}` for reading failed"))?;

                bystream_fut(resource_name.clone(), Box::new(BufReader::new(file))).await?;
                Ok(vec![hash.clone()])
            })
            .await
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
                ..Default::default()
            };
            for blob in batch {
                match blob {
                    BatchUploadRequest::Blob(blob) => {
                        re_request.requests.push(Request {
                            digest: Some(tdigest_to(&blob.digest)),
                            data: blob.blob.clone(),
                            compressor: compressor::Value::Identity as i32,
                        });
                    }
                    BatchUploadRequest::File(file) => {
                        // These should be small files, so no need to use a buffered reader.
                        let mut fin = tokio::fs::File::open(&file.name)
                            .await
                            .with_context(|| format!("Opening {} for reading failed", file.name))?;
                        let mut data = vec![];
                        fin.read_to_end(&mut data).await?;

                        re_request.requests.push(Request {
                            digest: Some(tdigest_to(&file.digest)),
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

            let response = retry(|| async { cas_f(re_request.clone()).await }).await?;
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
    metadata: &RemoteExecutionMetadata,
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
        // This is defined in `fbcode/remote_execution/re_cas_common/grpc/proto/metadata.proto`.
        #[derive(prost::Message)]
        struct Metadata {
            #[prost(message, optional, tag = "15")]
            platform: Option<crate::grpc::Platform>,
            #[prost(string, optional, tag = "18")]
            use_case_id: Option<String>,
        }

        let mut encoded = Vec::new();
        Metadata {
            platform: metadata.platform.clone(),
            use_case_id: Some(metadata.use_case_id.clone()),
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
                .as_ref()
                .map_or(String::new(), |buck_info| buck_info.build_id.clone()),
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
    static ENV_REGEX: LazyLock<Regex> =
        LazyLock::new(|| Regex::new("\\$[a-zA-Z_][a-zA-Z_0-9]*").unwrap());

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
mod goaway_tests;

#[cfg(test)]
mod tests {
    use core::sync::atomic::Ordering;
    use std::sync::atomic::AtomicU16;

    use re_grpc_proto::build::bazel::remote::execution::v2::batch_read_blobs_response;
    use re_grpc_proto::build::bazel::remote::execution::v2::batch_update_blobs_response;

    use super::*;

    #[test]
    fn test_select_download_hash_digest_function() -> anyhow::Result<()> {
        assert_eq!(
            select_download_hash_digest_function(
                &["BLAKE3".to_owned()],
                &[
                    digest_function::Value::Sha256,
                    digest_function::Value::Blake3
                ],
            )?,
            Some(digest_function::Value::Blake3)
        );
        assert_eq!(
            select_download_hash_digest_function(
                &[],
                &[
                    digest_function::Value::Sha256,
                    digest_function::Value::Blake3
                ],
            )?,
            None
        );
        assert_eq!(
            select_download_hash_digest_function(&[], &[digest_function::Value::Sha256],)?,
            Some(digest_function::Value::Sha256)
        );
        assert!(
            select_download_hash_digest_function(
                &["SHA256".to_owned()],
                &[digest_function::Value::Blake3],
            )
            .is_err()
        );
        assert_eq!(
            select_download_hash_digest_function(
                &["BLAKE3".to_owned(), "SHA256".to_owned()],
                &[
                    digest_function::Value::Sha256,
                    digest_function::Value::Blake3
                ],
            )?,
            Some(digest_function::Value::Blake3)
        );
        assert_eq!(
            select_download_hash_digest_function(
                &["SHA384".to_owned()],
                &[digest_function::Value::Sha384],
            )?,
            None
        );
        Ok(())
    }

    fn digest_for_test_data(data: &[u8]) -> TDigest {
        TDigest {
            hash: hex::encode(Sha256::digest(data)),
            size_in_bytes: data.len() as i64,
            ..Default::default()
        }
    }

    #[tokio::test]
    async fn test_retry_unauthenticated() {
        let attempts = AtomicU16::new(0);
        let err = retry(|| async {
            attempts.fetch_add(1, Ordering::Relaxed);
            Err::<(), _>(
                anyhow::Error::new(tonic::Status::unauthenticated("no creds"))
                    .context("FindMissingBlobs failed"),
            )
        })
        .await
        .unwrap_err();

        assert_eq!(attempts.load(Ordering::Relaxed), 1);
        let re_error = err.downcast_ref::<REClientError>().unwrap();
        assert_eq!(re_error.code, TCode::UNAUTHENTICATED);
        assert_eq!(re_error.group, TCodeReasonGroup::UNKNOWN);
        assert_eq!(re_error.message, "no creds");
        assert!(err.downcast_ref::<tonic::Status>().is_some());
        assert!(format!("{err:#}").contains("FindMissingBlobs failed"));
    }

    #[tokio::test]
    async fn test_retry_exhausted() {
        let attempts = AtomicU16::new(0);
        let err = retry(|| async {
            attempts.fetch_add(1, Ordering::Relaxed);
            Err::<(), _>(anyhow::Error::new(tonic::Status::unavailable("offline")))
        })
        .await
        .unwrap_err();

        assert_eq!(attempts.load(Ordering::Relaxed), 5);
        assert_eq!(
            err.downcast_ref::<REClientError>().unwrap().code,
            TCode::UNAVAILABLE
        );
        assert!(err.downcast_ref::<tonic::Status>().is_some());
    }

    #[test]
    fn test_normalize_re_error_io_wrapped() {
        let io = io::Error::other(tonic::Status::unauthenticated("no creds"));
        let err = normalize_re_error(anyhow::Error::new(io).context("stream error"));
        assert_eq!(
            err.downcast_ref::<REClientError>().unwrap().code,
            TCode::UNAUTHENTICATED
        );
        assert!(err.downcast_ref::<io::Error>().is_some());
        assert!(format!("{err:#}").contains("stream error"));
    }

    #[test]
    fn test_normalize_re_error_preserves_classification() {
        let err = anyhow::Error::new(tonic::Status::unauthenticated("no creds"))
            .context(REClientError {
                message: "denied".to_owned(),
                code: TCode::PERMISSION_DENIED,
                group: TCodeReasonGroup::USER_BAD_CERTS,
            })
            .context("upload failed")
            .context("remote action failed");
        let diagnostic = format!("{err:#}");
        let err = normalize_re_error(err);
        let re_error = err.downcast_ref::<REClientError>().unwrap();
        assert_eq!(re_error.code, TCode::PERMISSION_DENIED);
        assert_eq!(re_error.group, TCodeReasonGroup::USER_BAD_CERTS);
        assert_eq!(re_error.message, "denied");
        assert_eq!(format!("{err:#}"), diagnostic);
        assert!(err.downcast_ref::<tonic::Status>().is_some());
    }

    #[test]
    fn test_normalize_re_error_without_status() {
        let err = normalize_re_error(anyhow::anyhow!("plain error with no grpc status"));
        assert!(err.downcast_ref::<REClientError>().is_none());
        assert_eq!(err.to_string(), "plain error with no grpc status");
    }

    #[tokio::test]
    async fn test_download_named() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path1 = work.path().join("path1");
        let path1 = path1.to_str().context("tempdir is not utf8")?;

        let path2 = work.path().join("path2");
        let path2 = path2.to_str().context("tempdir is not utf8")?;

        let blob1 = vec![1, 2, 3];
        let blob2 = vec![4, 5, 6];
        let digest1 = digest_for_test_data(&blob1);
        let digest2 = digest_for_test_data(&blob2);

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
                    digest: Some(tdigest_to(&digest2)),
                    data: blob2.clone(),
                    ..Default::default()
                },
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(&digest1)),
                    data: blob1.clone(),
                    ..Default::default()
                },
            ],
        };

        download_impl(
            &InstanceName(None),
            req,
            None,
            10000,
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.digests.len(), 2);
                    assert_eq!(req.digests[0], tdigest_to(&digest1));
                    assert_eq!(req.digests[1], tdigest_to(&digest2));
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

        let blob1 = vec![1, 2, 3];
        let digest1 = digest_for_test_data(&blob1);

        let blob_data = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];

        let digest2 = digest_for_test_data(&blob_data);

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
                    digest: Some(tdigest_to(&digest1)),
                    data: blob1.clone(),
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
            None,
            10, // kept small to simulate a large file download
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                async move {
                    assert_eq!(req.digests.len(), 1);
                    assert_eq!(req.digests[0], tdigest_to(&digest1));
                    Ok(res.clone())
                }
            },
            |req| {
                let read_response1 = read_response1.clone();
                let read_response2 = read_response2.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.resource_name, format!("blobs/{}/18", digest2.hash));
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
        let blob1 = vec![1, 2, 3];
        let blob2 = vec![4, 5, 6];
        let digest1 = &digest_for_test_data(&blob1);
        let digest2 = &digest_for_test_data(&blob2);

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone(), digest2.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse {
            responses: vec![
                // Reply out of order
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(&digest2)),
                    data: blob2.clone(),
                    ..Default::default()
                },
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(&digest1)),
                    data: blob1.clone(),
                    ..Default::default()
                },
            ],
        };

        let res = download_impl(
            &InstanceName(None),
            req,
            None,
            100000,
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.digests.len(), 2);
                    assert_eq!(req.digests[0], tdigest_to(&digest1));
                    assert_eq!(req.digests[1], tdigest_to(&digest2));
                    Ok(res)
                }
            },
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
        )
        .await?;

        let inlined_blobs = res.inlined_blobs.unwrap();

        assert_eq!(inlined_blobs.len(), 2);

        assert_eq!(inlined_blobs[0].digest, *digest1);
        assert_eq!(inlined_blobs[0].blob, blob1);

        assert_eq!(inlined_blobs[1].digest, *digest2);
        assert_eq!(inlined_blobs[1].blob, blob2);

        Ok(())
    }

    #[tokio::test]
    async fn test_download_multiple_batches() -> anyhow::Result<()> {
        let blob_data = vec![0, 1, 2];
        let digest1 = &digest_for_test_data(&blob_data);
        let digest2 = &digest_for_test_data(&blob_data);
        let digest3 = &digest_for_test_data(&blob_data);
        let digest4 = &digest_for_test_data(&blob_data);
        let digest5 = &digest_for_test_data(&blob_data);
        let digest6 = &digest_for_test_data(&blob_data);

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
            None,
            7,
            None,
            |req| {
                counter.fetch_add(1, Ordering::Relaxed);
                let res = BatchReadBlobsResponse {
                    responses: req
                        .digests
                        .iter()
                        .map(|d| batch_read_blobs_response::Response {
                            digest: Some(d.clone()),
                            data: blob_data.clone(),
                            ..Default::default()
                        })
                        .collect::<Vec<_>>(),
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
        let blob1 = vec![1, 2, 3];
        let digest1 = &digest_for_test_data(&blob1);
        let blob_data = vec![
            1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
        ];
        let digest2 = &digest_for_test_data(&blob_data);

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone(), digest2.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse {
            responses: vec![
                // Reply out of order
                batch_read_blobs_response::Response {
                    digest: Some(tdigest_to(&digest1)),
                    data: blob1.clone(),
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

        let res = download_impl(
            &InstanceName(None),
            req,
            None,
            10, // intentionally small value to keep data in the test blobs small
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                async move {
                    assert_eq!(req.digests.len(), 1);
                    assert_eq!(req.digests[0], tdigest_to(&digest1));
                    Ok(res)
                }
            },
            |req| {
                let read_response1 = read_response1.clone();
                let read_response2 = read_response2.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.resource_name, format!("blobs/{}/18", digest2.hash));
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
        assert_eq!(inlined_blobs[0].blob, blob1);

        assert_eq!(inlined_blobs[1].digest, *digest2);
        assert_eq!(inlined_blobs[1].blob, blob_data);

        Ok(())
    }

    #[tokio::test]
    async fn test_download_empty() -> anyhow::Result<()> {
        let digest1 = &digest_for_test_data(&[]);

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse { responses: vec![] };

        let res = download_impl(
            &InstanceName(None),
            req,
            None,
            100000,
            None,
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
    async fn test_download_inlined_size_mismatch_fails() -> anyhow::Result<()> {
        let digest1 = digest_for_test_data(&[1, 2, 3]);

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse {
            responses: vec![batch_read_blobs_response::Response {
                digest: Some(tdigest_to(&digest1)),
                data: vec![1, 2],
                ..Default::default()
            }],
        };

        let err = match download_impl(
            &InstanceName(None),
            req,
            None,
            100000,
            None,
            |_req| {
                let res = res.clone();
                async move { Ok(res) }
            },
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
        )
        .await
        {
            Ok(_) => anyhow::bail!("expected size mismatch error"),
            Err(err) => err,
        };

        assert!(
            err.chain()
                .any(|e| e.to_string().contains("Downloaded blob size mismatch"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_download_named_stream_size_mismatch_fails() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path = work.path().join("path");
        let path = path.to_str().context("tempdir is not utf8")?;

        let digest = digest_for_test_data(&[1; 18]);
        let req = DownloadRequest {
            file_digests: Some(vec![NamedDigestWithPermissions {
                named_digest: NamedDigest {
                    name: path.to_owned(),
                    digest: digest.clone(),
                    ..Default::default()
                },
                is_executable: false,
                ..Default::default()
            }]),
            ..Default::default()
        };

        let read_response = ReadResponse { data: vec![1; 17] };
        let err = match download_impl(
            &InstanceName(None),
            req,
            None,
            10,
            None,
            |_req| async { Ok(BatchReadBlobsResponse { responses: vec![] }) },
            |req| {
                let read_response = read_response.clone();
                let digest = digest.clone();
                async move {
                    assert_eq!(req.resource_name, format!("blobs/{}/18", digest.hash));
                    anyhow::Ok(Box::pin(futures::stream::iter(vec![Ok(read_response)])))
                }
            },
        )
        .await
        {
            Ok(_) => anyhow::bail!("expected size mismatch error"),
            Err(err) => err,
        };

        assert!(
            err.chain()
                .any(|e| e.to_string().contains("Downloaded blob size mismatch"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_download_inlined_hash_mismatch_fails() -> anyhow::Result<()> {
        let digest = digest_for_test_data(&[1, 2, 3]);
        let req = DownloadRequest {
            inlined_digests: Some(vec![digest.clone()]),
            ..Default::default()
        };

        let res = BatchReadBlobsResponse {
            responses: vec![batch_read_blobs_response::Response {
                digest: Some(tdigest_to(&digest)),
                data: vec![4, 5, 6],
                ..Default::default()
            }],
        };

        let err = match download_impl(
            &InstanceName(None),
            req,
            None,
            100000,
            None,
            |_req| {
                let res = res.clone();
                async move { Ok(res) }
            },
            |_digest| async move { anyhow::Ok(Box::pin(futures::stream::iter(vec![]))) },
        )
        .await
        {
            Ok(_) => anyhow::bail!("expected hash mismatch error"),
            Err(err) => err,
        };

        assert!(
            err.chain()
                .any(|e| e.to_string().contains("Downloaded blob hash mismatch"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_download_named_stream_hash_mismatch_fails() -> anyhow::Result<()> {
        let work = tempfile::tempdir()?;

        let path = work.path().join("path");
        let path = path.to_str().context("tempdir is not utf8")?;

        let expected_data = vec![1u8; 18];
        let actual_data = vec![2u8; 18];
        let digest = digest_for_test_data(&expected_data);
        let req = DownloadRequest {
            file_digests: Some(vec![NamedDigestWithPermissions {
                named_digest: NamedDigest {
                    name: path.to_owned(),
                    digest: digest.clone(),
                    ..Default::default()
                },
                is_executable: false,
                ..Default::default()
            }]),
            ..Default::default()
        };

        let read_response = ReadResponse {
            data: actual_data.clone(),
        };
        let err = match download_impl(
            &InstanceName(None),
            req,
            None,
            10,
            None,
            |_req| async { Ok(BatchReadBlobsResponse { responses: vec![] }) },
            |req| {
                let read_response = read_response.clone();
                let digest = digest.clone();
                async move {
                    assert_eq!(req.resource_name, format!("blobs/{}/18", digest.hash));
                    anyhow::Ok(Box::pin(futures::stream::iter(vec![Ok(read_response)])))
                }
            },
        )
        .await
        {
            Ok(_) => anyhow::bail!("expected hash mismatch error"),
            Err(err) => err,
        };

        assert!(
            err.chain()
                .any(|e| e.to_string().contains("Downloaded blob hash mismatch"))
        );
        Ok(())
    }

    #[tokio::test]
    async fn test_download_resource_name() -> anyhow::Result<()> {
        let digest1 = &digest_for_test_data(&[]);

        let req = DownloadRequest {
            inlined_digests: Some(vec![digest1.clone()]),
            ..Default::default()
        };

        download_impl(
            &InstanceName(Some("instance".to_owned())),
            req,
            None,
            0,
            None,
            |_req| async { panic!("not called") },
            |req| async move {
                assert_eq!(
                    req.resource_name,
                    format!("instance/blobs/{}/0", digest1.hash)
                );
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
                    digest: Some(tdigest_to(&digest2)),
                    status: Some(Status::default()),
                },
                batch_update_blobs_response::Response {
                    digest: Some(tdigest_to(&digest1)),
                    status: Some(Status::default()),
                },
            ],
        };

        upload_impl(
            &InstanceName(None),
            req,
            None,
            10000,
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let digest2 = digest2.clone();
                async move {
                    assert_eq!(req.requests.len(), 2);
                    assert_eq!(req.requests[0].digest, Some(tdigest_to(&digest1)));
                    assert_eq!(req.requests[0].data, b"aaa");
                    assert_eq!(req.requests[1].digest, Some(tdigest_to(&digest2)));
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
                    digest: Some(tdigest_to(&digest2)),
                    status: Some(Status::default()),
                },
                batch_update_blobs_response::Response {
                    digest: Some(tdigest_to(&digest1)),
                    status: Some(Status::default()),
                },
            ],
        };

        upload_impl(
            &InstanceName(None),
            req,
            None,
            10, // kept small to simulate a large file upload
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                async move {
                    assert_eq!(req.requests.len(), 1);
                    assert_eq!(req.requests[0].digest, Some(tdigest_to(&digest1)));
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
                digest: Some(tdigest_to(&digest2)),
                status: Some(Status::default()),
            }],
        };

        upload_impl(
            &InstanceName(None),
            req,
            None,
            10, // kept small to simulate a large inlined upload
            None,
            |req| {
                let res = res.clone();
                let digest1 = digest1.clone();
                let blob_data1 = blob_data1.clone();
                async move {
                    assert_eq!(req.requests.len(), 1);
                    assert_eq!(req.requests[0].digest, Some(tdigest_to(&digest1)));
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
            None,
            10,
            None,
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
            None,
            3,
            None,
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

        for compressor in [
            None,
            Some(Compressor::Deflate),
            Some(Compressor::Brotli),
            Some(Compressor::Zstd),
        ] {
            assert!(
                upload_impl(
                    &InstanceName(None),
                    UploadRequest {
                        files_with_digest: Some(vec![NamedDigest {
                            name: path1.to_owned(),
                            digest: digest1.clone(),
                            ..Default::default()
                        }]),
                        ..Default::default()
                    },
                    compressor,
                    0, // max_total_batch_size=0 forces bytestream API
                    None,
                    |_req| async move {
                        panic!("Not called");
                    },
                    |_write_reqs| async move {
                        panic!("Not called");
                    },
                )
                .await
                .is_ok()
            );

            assert!(
                upload_impl(
                    &InstanceName(None),
                    UploadRequest {
                        files_with_digest: Some(vec![NamedDigest {
                            name: path1.to_owned(),
                            digest: digest1.clone(),
                            ..Default::default()
                        }]),
                        ..Default::default()
                    },
                    compressor,
                    1024, // forces the batch API
                    None,
                    |_req| async move {
                        panic!("Not called");
                    },
                    |_write_reqs| async move {
                        panic!("Not called");
                    },
                )
                .await
                .is_ok()
            );
        }
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
            None,
            1,
            None,
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

    #[tokio::test]
    async fn test_upload_resource_name_compressed() -> anyhow::Result<()> {
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
            Some(Compressor::Zstd),
            1,
            None,
            |_req| async move {
                panic!("Not called");
            },
            |write_reqs| async move {
                assert!(write_reqs[0].resource_name.starts_with("instance/uploads/"));
                assert!(
                    write_reqs[0]
                        .resource_name
                        .ends_with("/compressed-blobs/zstd/aa/3")
                );
                anyhow::Ok(WriteResponse { committed_size: -1 })
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

    #[test]
    fn prepare_uri_adds_root_path_to_bare_authority() -> anyhow::Result<()> {
        let (uri, tls) = prepare_uri("remote.buildbuddy.io".parse()?)?;
        assert_eq!(uri.to_string(), "https://remote.buildbuddy.io/");
        assert!(tls);

        let (uri, tls) = prepare_uri("grpc://localhost:8980".parse()?)?;
        assert_eq!(uri.to_string(), "http://localhost:8980/");
        assert!(!tls);

        let (uri, tls) = prepare_uri("grpcs://remote.buildbuddy.io/cache".parse()?)?;
        assert_eq!(uri.to_string(), "https://remote.buildbuddy.io/cache");
        assert!(tls);

        Ok(())
    }

    #[test]
    fn test_trim_bystream_write_segments_partial() {
        let resource_name = "uploads/uuid/blobs/hash/18".to_owned();
        let segments = vec![
            WriteRequest {
                resource_name: resource_name.clone(),
                write_offset: 0,
                finish_write: false,
                data: vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            },
            WriteRequest {
                resource_name,
                write_offset: 10,
                finish_write: true,
                data: vec![11, 12, 13, 14, 15, 16, 17, 18],
            },
        ];

        assert_eq!(18, total_bystream_write_size(&segments));

        let resumed = trim_bystream_write_segments(segments, 12);
        assert_eq!(1, resumed.len());
        assert_eq!(12, resumed[0].write_offset);
        assert_eq!(vec![13, 14, 15, 16, 17, 18], resumed[0].data);
        assert!(resumed[0].finish_write);
    }

    #[test]
    fn test_trim_bystream_write_segments_no_trim() {
        let segments = vec![WriteRequest {
            resource_name: "uploads/uuid/blobs/hash/3".to_owned(),
            write_offset: 0,
            finish_write: true,
            data: vec![1, 2, 3],
        }];

        let resumed = trim_bystream_write_segments(segments.clone(), 0);
        assert_eq!(segments, resumed);
    }
}

#[tokio::test]
async fn test_upload_compressed() -> anyhow::Result<()> {
    let blob_data = vec![1; 10 * 1024 * 1024];
    let digest1 = TDigest {
        hash: "aa".to_owned(),
        size_in_bytes: blob_data.len() as i64,
        ..Default::default()
    };

    let req = UploadRequest {
        inlined_blobs_with_digest: Some(vec![InlinedBlobWithDigest {
            digest: digest1.clone(),
            blob: blob_data.clone(),
            ..Default::default()
        }]),
        ..Default::default()
    };

    let blob_data_ref = &blob_data;
    upload_impl(
        &InstanceName(Some("instance".to_owned())),
        req,
        Some(Compressor::Zstd),
        1,
        None,
        |_req| async move {
            panic!("Not called");
        },
        {
            |write_reqs| async move {
                let compressed_data: Vec<u8> =
                    write_reqs.iter().flat_map(|wr| wr.data.clone()).collect();
                let mut data = vec![];
                ZstdDecoder::new(Cursor::new(compressed_data))
                    .read_to_end(&mut data)
                    .await
                    .unwrap();
                assert_eq!(&data, blob_data_ref);
                anyhow::Ok(WriteResponse { committed_size: -1 })
            }
        },
    )
    .await?;

    Ok(())
}

#[tokio::test]
async fn test_download_compressed() -> anyhow::Result<()> {
    let blob_data = vec![1; 1024];

    let mut compressed_data = vec![];
    ZstdEncoder::new(Cursor::new(blob_data.clone()))
        .read_to_end(&mut compressed_data)
        .await
        .unwrap();
    let compressed_data_ref = &compressed_data;

    let d_resp = download_impl(
        &InstanceName(None),
        DownloadRequest {
            inlined_digests: Some(vec![TDigest {
                hash: hex::encode(Sha256::digest(&blob_data)),
                size_in_bytes: blob_data.len() as i64,
                ..Default::default()
            }]),
            file_digests: None,
            ..Default::default()
        },
        Some(Compressor::Zstd),
        10,
        None,
        |_req| async { panic!("not called") },
        |_req| async move {
            Ok(Box::pin(futures::stream::iter(
                compressed_data_ref
                    .chunks(10)
                    .map(|d| Result::Ok(ReadResponse { data: d.to_vec() })),
            )))
        },
    )
    .await?;

    assert_eq!(
        d_resp.inlined_blobs.as_ref().unwrap()[0].blob.len(),
        blob_data.len()
    );
    assert_eq!(d_resp.inlined_blobs.unwrap()[0].blob, blob_data);
    Ok(())
}
