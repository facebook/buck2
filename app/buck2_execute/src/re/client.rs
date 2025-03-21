/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;

use allocative::Allocative;
use anyhow::Context;
use buck2_core::buck2_env;
use buck2_core::execution_types::executor_config::MetaInternalExtraParams;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_error::buck2_error;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::BuckErrorContext;
#[cfg(fbcode_build)]
use buck2_re_configuration::CASdMode;
use buck2_re_configuration::RemoteExecutionStaticMetadataImpl;
use chrono::DateTime;
use chrono::Utc;
use dupe::Dupe;
use either::Either;
use futures::stream::BoxStream;
use futures::FutureExt;
use futures::StreamExt;
use gazebo::prelude::*;
use itertools::Itertools;
use prost::Message;
use remote_execution as RE;
use remote_execution::ActionResultRequest;
use remote_execution::ActionResultResponse;
use remote_execution::BuckInfo;
use remote_execution::DownloadRequest;
use remote_execution::ExecuteRequest;
use remote_execution::ExecuteResponse;
use remote_execution::ExecuteWithProgressResponse;
use remote_execution::ExtendDigestsTtlRequest;
use remote_execution::GetDigestsTtlRequest;
use remote_execution::InlinedBlobWithDigest;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;
use remote_execution::REClient;
use remote_execution::REClientBuilder;
use remote_execution::RemoteExecutionMetadata;
#[cfg(fbcode_build)]
use remote_execution::RemoteFetchPolicy;
use remote_execution::Stage;
use remote_execution::TActionResult2;
use remote_execution::TCode;
use remote_execution::TDependency;
use remote_execution::TDigest;
use remote_execution::TExecutionPolicy;
use remote_execution::THostResourceRequirements;
use remote_execution::THostRuntimeRequirements;
use remote_execution::TLocalCacheStats;
use remote_execution::UploadRequest;
use remote_execution::WriteActionResultRequest;
use remote_execution::WriteActionResultResponse;
use tokio::sync::Semaphore;

use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::directory::ActionImmutableDirectory;
use crate::execute::action_digest::ActionDigest;
use crate::execute::blobs::ActionBlobs;
use crate::execute::executor_stage_async;
use crate::execute::manager::CommandExecutionManager;
use crate::knobs::ExecutorGlobalKnobs;
use crate::materialize::materializer::Materializer;
use crate::re::action_identity::ReActionIdentity;
use crate::re::convert::platform_to_proto;
use crate::re::error::test_re_error;
use crate::re::error::with_error_handler;
use crate::re::error::RemoteExecutionError;
use crate::re::manager::RemoteExecutionConfig;
use crate::re::metadata::RemoteExecutionMetadataExt;
use crate::re::stats::LocalCacheRemoteExecutionClientStats;
use crate::re::stats::LocalCacheStats;
use crate::re::stats::OpStats;
use crate::re::stats::RemoteExecutionClientOpStats;
use crate::re::stats::RemoteExecutionClientStats;
use crate::re::uploader::UploadStats;
use crate::re::uploader::Uploader;

pub enum CancellationReason {
    NotSpecified,
    ReQueueTimeout,
}

#[derive(Default)]
pub struct Cancelled {
    pub reason: Option<CancellationReason>,
}

#[derive(Clone, Dupe, Allocative)]
pub struct RemoteExecutionClient {
    data: Arc<RemoteExecutionClientData>,
}

// The large one is the actual default case
#[allow(clippy::large_enum_variant)]
pub enum ExecuteResponseOrCancelled {
    Response(ExecuteResponse),
    Cancelled(Cancelled),
}

#[derive(Allocative)]
struct RemoteExecutionClientData {
    client: RemoteExecutionClientImpl,
    uploads: OpStats,
    downloads: OpStats,
    action_cache: OpStats,
    executes: OpStats,
    materializes: OpStats,
    write_action_results: OpStats,
    get_digest_expirations: OpStats,
    extend_digest_ttl: OpStats,
    local_cache: LocalCacheStats,
    persistent_cache_mode: Option<String>,
}

#[cfg(fbcode_build)]
fn map_casd_mode_into_remote_fetch_policy(casd_mode: &CASdMode) -> RemoteFetchPolicy {
    match casd_mode {
        CASdMode::LocalWithoutSync => RemoteFetchPolicy::LOCAL_FETCH_WITHOUT_SYNC,
        CASdMode::Remote => RemoteFetchPolicy::REMOTE_FETCH,
        CASdMode::LocalWithSync => RemoteFetchPolicy::LOCAL_FETCH_WITH_SYNC,
    }
}

impl RemoteExecutionClient {
    pub async fn new(re_config: &RemoteExecutionConfig) -> buck2_error::Result<Self> {
        if buck2_env!("BUCK2_TEST_FAIL_CONNECT", bool, applicability = testing)? {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Injected RE Connection error"
            ));
        }

        let client = RemoteExecutionClientImpl::new(re_config).await?;

        let persistent_cache_mode = client.persistent_cache_mode.clone();
        Ok(Self {
            data: Arc::new(RemoteExecutionClientData {
                client,
                uploads: OpStats::default(),
                downloads: OpStats::default(),
                action_cache: OpStats::default(),
                executes: OpStats::default(),
                materializes: OpStats::default(),
                write_action_results: OpStats::default(),
                get_digest_expirations: OpStats::default(),
                extend_digest_ttl: OpStats::default(),
                local_cache: Default::default(),
                persistent_cache_mode,
            }),
        })
    }

    pub async fn new_retry(re_config: &RemoteExecutionConfig) -> buck2_error::Result<Self> {
        // Loop happens times-1 times at most
        for i in 1..re_config.connection_retries {
            match Self::new(re_config).await {
                Ok(v) => return Ok(v),
                Err(e) => {
                    let e: buck2_error::Error = e.into();
                    if e.find_typed_context::<RemoteExecutionError>().is_none() {
                        // If we cannot connect to RE due to some non-RE error, we should not retry
                        // And should just return the error immediately as it's unlikely to be flakey
                        return Err(e.into());
                    }

                    tracing::warn!(
                        "Failed to connect to RE, retrying after sleeping {} seconds: {:#?}",
                        i,
                        e
                    );
                    tokio::time::sleep(Duration::from_secs(i as u64)).await;
                }
            }
        }
        Self::new(re_config).await
    }

    pub async fn action_cache(
        &self,
        action_digest: ActionDigest,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<Option<ActionResultResponse>> {
        self.data
            .action_cache
            .op(self.data.client.action_cache(action_digest, use_case))
            .await
    }

    pub async fn upload(
        &self,
        fs: &ProjectRoot,
        materializer: &Arc<dyn Materializer>,
        blobs: &ActionBlobs,
        dir_path: &ProjectRelativePath,
        input_dir: &ActionImmutableDirectory,
        use_case: RemoteExecutorUseCase,
        identity: Option<&ReActionIdentity<'_>>,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<UploadStats> {
        self.data
            .uploads
            .op(self.data.client.upload(
                fs,
                materializer,
                blobs,
                dir_path,
                input_dir,
                use_case,
                identity,
                digest_config,
            ))
            .await
    }

    pub async fn upload_files_and_directories(
        &self,
        files_with_digest: Vec<NamedDigest>,
        directories: Vec<remote_execution::Path>,
        inlined_blobs_with_digest: Vec<InlinedBlobWithDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<()> {
        self.data
            .uploads
            .op(self.data.client.upload_files_and_directories(
                files_with_digest,
                directories,
                inlined_blobs_with_digest,
                use_case,
            ))
            .await
    }

    pub async fn execute<'a>(
        &self,
        action_digest: ActionDigest,
        platform: &RE::Platform,
        dependencies: impl IntoIterator<Item = &'a RemoteExecutorDependency>,
        use_case: RemoteExecutorUseCase,
        identity: &ReActionIdentity<'_>,
        manager: &mut CommandExecutionManager,
        skip_cache_read: bool,
        skip_cache_write: bool,
        re_max_queue_time: Option<Duration>,
        re_resource_units: Option<i64>,
        knobs: &ExecutorGlobalKnobs,
        meta_internal_extra_params: &MetaInternalExtraParams,
    ) -> buck2_error::Result<ExecuteResponseOrCancelled> {
        self.data
            .executes
            .op(self.data.client.execute(
                action_digest,
                platform,
                dependencies,
                use_case,
                identity,
                manager,
                skip_cache_read,
                skip_cache_write,
                re_max_queue_time,
                re_resource_units,
                knobs,
                meta_internal_extra_params,
            ))
            .await
    }

    pub async fn materialize_files(
        &self,
        files: Vec<NamedDigestWithPermissions>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<()> {
        let stat = self
            .data
            .materializes
            .op(self.data.client.materialize_files(files, use_case))
            .await?;
        self.data.local_cache.update(&stat);
        Ok(())
    }

    pub async fn download_typed_blobs<T: Message + Default>(
        &self,
        identity: Option<&ReActionIdentity<'_>>,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<Vec<T>> {
        self.data
            .downloads
            .op(self
                .data
                .client
                .download_typed_blobs(identity, digests, use_case))
            .await
            .map(|r| {
                self.data.local_cache.update(&r.1);
                r.0
            })
    }

    pub async fn download_blob(
        &self,
        digest: &TDigest,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<Vec<u8>> {
        self.data
            .downloads
            .op(self.data.client.download_blob(digest, use_case))
            .await
            .map(|r| {
                self.data.local_cache.update(&r.1);
                r.0
            })
    }

    pub async fn upload_blob(
        &self,
        blob: InlinedBlobWithDigest,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<TDigest> {
        self.data
            .uploads
            .op(self.data.client.upload_blob(blob, use_case))
            .await
    }

    pub async fn get_digest_expirations(
        &self,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<Vec<(TDigest, DateTime<Utc>)>> {
        self.data
            .get_digest_expirations
            .op(self.data.client.get_digest_expirations(digests, use_case))
            .await
    }

    pub async fn extend_digest_ttl(
        &self,
        digests: Vec<TDigest>,
        ttl: Duration,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<()> {
        self.data
            .extend_digest_ttl
            .op(self.data.client.extend_digest_ttl(digests, ttl, use_case))
            .await
    }

    pub async fn write_action_result(
        &self,
        digest: ActionDigest,
        result: TActionResult2,
        use_case: RemoteExecutorUseCase,
        platform: &RE::Platform,
    ) -> buck2_error::Result<WriteActionResultResponse> {
        self.data
            .write_action_results
            .op(self
                .data
                .client
                .write_action_result(digest, result, use_case, platform))
            .await
    }

    pub fn get_session_id(&self) -> &str {
        self.data.client.get_session_id()
    }

    pub fn get_persistent_cache_mode(&self) -> Option<String> {
        self.data.persistent_cache_mode.clone()
    }

    pub fn get_experiment_name(&self) -> buck2_error::Result<Option<String>> {
        self.data
            .client
            .client()
            .get_experiment_name()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))
    }

    pub fn fill_network_stats(&self, stats: &mut RemoteExecutionClientStats) {
        stats.uploads = RemoteExecutionClientOpStats::from(&self.data.uploads);
        stats.downloads = RemoteExecutionClientOpStats::from(&self.data.downloads);
        stats.executes = RemoteExecutionClientOpStats::from(&self.data.executes);
        stats.action_cache = RemoteExecutionClientOpStats::from(&self.data.action_cache);
        stats.write_action_results =
            RemoteExecutionClientOpStats::from(&self.data.write_action_results);
        stats.materializes = RemoteExecutionClientOpStats::from(&self.data.materializes);
        stats.get_digest_expirations =
            RemoteExecutionClientOpStats::from(&self.data.get_digest_expirations);
        stats.local_cache = LocalCacheRemoteExecutionClientStats::from(&self.data.local_cache);
    }
}

#[derive(Allocative)]
struct RemoteExecutionClientImpl {
    #[allocative(skip)]
    client: Option<REClient>,
    skip_remote_cache: bool,
    /// How many simultaneous requests to RE
    #[allocative(skip)]
    cas_semaphore: Arc<Semaphore>,
    /// How many files we can be downloading concurrently.
    #[allocative(skip)]
    download_files_semapore: Arc<Semaphore>,
    /// How many files to kick off downloading concurrently for one request. This should be smaller
    /// than the files semaphore to ensure we can actually *acquire* that semaphore.
    download_chunk_size: usize,
    /// Preserve file symlinks as symlinks when uploading action result.
    respect_file_symlinks: bool,
    persistent_cache_mode: Option<String>,
}

fn re_platform(x: &RE::Platform) -> remote_execution::TPlatform {
    remote_execution::TPlatform {
        properties: x.properties.map(|x| remote_execution::TProperty {
            name: x.name.clone(),
            value: x.value.clone(),
            ..Default::default()
        }),
        ..Default::default()
    }
}

impl RemoteExecutionClientImpl {
    async fn new(re_config: &RemoteExecutionConfig) -> buck2_error::Result<Self> {
        let op_name = "REClientBuilder";
        tracing::info!("Creating a new RE client");

        let res: buck2_error::Result<Self> = try {
            let download_concurrency =
                buck2_env!("BUCK2_RE_DOWNLOAD_CONCURRENCY", type=usize, default=256)?;

            // Split things up into smaller chunks.
            let download_chunk_size = std::cmp::max(download_concurrency / 8, 1);
            let static_metadata = &re_config.static_metadata;

            #[allow(unused_mut)]
            let mut persistent_cache_mode = None;
            #[cfg(fbcode_build)]
            let client = {
                use buck2_core::fs::fs_util;
                use remote_execution::create_default_config;
                use remote_execution::CASDaemonClientCfg;
                use remote_execution::CopyPolicy;
                use remote_execution::CurlReactorConfig;
                use remote_execution::EmbeddedCASDaemonClientCfg;
                use remote_execution::RichClientMode;
                use remote_execution::TTLExtendingConfig;
                use remote_execution::ThreadConfig;

                let mut re_client_config = create_default_config();

                // gRPC settings
                re_client_config.action_cache_client_config.connection_count =
                    static_metadata.action_cache_connection_count;
                re_client_config.action_cache_client_config.address =
                    static_metadata.action_cache_address.clone();

                let mut embedded_cas_daemon_config = EmbeddedCASDaemonClientCfg {
                    connection_count: static_metadata.cas_connection_count,
                    address: static_metadata.cas_address.clone(),
                    name: "buck2".to_owned(),
                    ..Default::default()
                };

                if re_config.is_paranoid_mode {
                    // Dedupe is not compatible with us downloading blobs and moving them.
                    embedded_cas_daemon_config.disable_download_dedup = true;
                }

                // buck2 makes find_missing calls for the same blobs
                // so having a 50Mb cache to amortize that
                embedded_cas_daemon_config
                    .cache_config
                    .find_missing_cache_size_byte = 50 << 20;
                // a small cache maps inodes to digests
                // useful for both uploads and downloads
                embedded_cas_daemon_config.cache_config.digest_cache_size = 100000;

                // If a shared CAS cache directory is configured, we
                // want to tell the RE client to rely on an external
                // CAS daemon to manage the cache.
                if let Some(external_casd_address) = &static_metadata.shared_casd_address {
                    use buck2_re_configuration::CASdAddress;
                    use buck2_re_configuration::CopyPolicy as Buck2CopyPolicy;
                    use remote_execution::RemoteCASdAddress;
                    use remote_execution::RemoteCacheConfig;
                    use remote_execution::RemoteCacheSyncConfig;

                    let policies: buck2_error::Result<(RemoteFetchPolicy, RemoteFetchPolicy)> =
                        if let Some(legacy_mode) = &static_metadata.legacy_shared_casd_mode {
                            let upper_legacy_mode = legacy_mode.to_uppercase();
                            match upper_legacy_mode.trim() {
                                "BIG_FILES" => Ok((
                                    RemoteFetchPolicy::LOCAL_FETCH_WITHOUT_SYNC,
                                    RemoteFetchPolicy::REMOTE_FETCH,
                                )),
                                "ALL_FILES" => Ok((
                                    RemoteFetchPolicy::REMOTE_FETCH,
                                    RemoteFetchPolicy::REMOTE_FETCH,
                                )),
                                "ALL_FILES_LOCAL_WITHOUT_SYNC" => Ok((
                                    RemoteFetchPolicy::LOCAL_FETCH_WITHOUT_SYNC,
                                    RemoteFetchPolicy::LOCAL_FETCH_WITHOUT_SYNC,
                                )),
                                unknown => {
                                    return Err(buck2_error!(
                                        buck2_error::ErrorTag::Input,
                                        "Unknown RemoteCacheManagerMode: {}",
                                        unknown
                                    ));
                                }
                            }
                        } else {
                            Ok((
                                static_metadata
                                    .shared_casd_mode_small_files
                                    .as_ref()
                                    .map(map_casd_mode_into_remote_fetch_policy)
                                    .unwrap_or(RemoteFetchPolicy::LOCAL_FETCH_WITHOUT_SYNC),
                                static_metadata
                                    .shared_casd_mode_large_files
                                    .as_ref()
                                    .map(map_casd_mode_into_remote_fetch_policy)
                                    .unwrap_or(RemoteFetchPolicy::LOCAL_FETCH_WITHOUT_SYNC),
                            ))
                        };
                    let (small_files_policy, large_files_policy) = policies?;

                    let remote_cache_config = {
                        let mut remote_cache_config = RemoteCacheConfig {
                            small_files: small_files_policy,
                            large_files: large_files_policy,
                            address: match external_casd_address {
                                CASdAddress::Uds(path) => RemoteCASdAddress::uds_path(path.clone()),
                                CASdAddress::Tcp(port) => RemoteCASdAddress::tcp_port(*port as i32),
                            },
                            sync_files_config: Some(RemoteCacheSyncConfig {
                                max_batch_size: static_metadata
                                    .shared_casd_cache_sync_max_batch_size
                                    .unwrap_or(100)
                                    as i32,
                                max_delay_ms: static_metadata
                                    .shared_casd_cache_sync_max_delay_ms
                                    .unwrap_or(1000)
                                    as i32,
                                wal_buckets: static_metadata
                                    .shared_casd_cache_sync_wal_files_count
                                    .unwrap_or(8)
                                    as i32,
                                wal_max_file_size: static_metadata
                                    .shared_casd_cache_sync_wal_file_max_size
                                    .unwrap_or(200 << 20)
                                    as i64,
                                ..Default::default()
                            }),
                            sync_copy_policy: match static_metadata
                                .shared_casd_copy_policy
                                .clone()
                                .unwrap_or(Buck2CopyPolicy::Copy)
                            {
                                Buck2CopyPolicy::Copy => CopyPolicy::FULL_COPY,
                                Buck2CopyPolicy::Reflink => CopyPolicy::SOFT_COPY,
                            },
                            ..Default::default()
                        };
                        if let Some(tls) = static_metadata.shared_casd_use_tls {
                            remote_cache_config.use_tls = tls;
                        }
                        remote_cache_config
                    };
                    persistent_cache_mode = Some(format!(
                        "small=>{:?}, large=>{:?}",
                        small_files_policy, large_files_policy
                    ));

                    embedded_cas_daemon_config.remote_cache_config = Some(remote_cache_config);
                    embedded_cas_daemon_config.cache_config.writable_cache = false;
                    embedded_cas_daemon_config
                        .cache_config
                        .downloads_cache_config
                        .dir_path = static_metadata.shared_casd_cache_path.clone();
                }

                // prevents downloading the same trees (dirs)
                embedded_cas_daemon_config
                    .rich_client_config
                    .get_tree_cache_size = 50 << 20; // 50 Mb

                // disabling zippy rich client until we have limits in place
                embedded_cas_daemon_config
                    .rich_client_config
                    .zdb_client_mode = if static_metadata.use_zippy_rich_client {
                    RichClientMode::HYBRID
                } else {
                    RichClientMode::DISABLED
                };
                embedded_cas_daemon_config.rich_client_config.disable_p2p =
                    !static_metadata.use_p2p;
                embedded_cas_daemon_config
                    .rich_client_config
                    .enable_rich_client = static_metadata.use_manifold_rich_client;

                if let Some(channels) = static_metadata.rich_client_channels_per_blob {
                    embedded_cas_daemon_config
                        .rich_client_config
                        .number_of_parallel_channels = channels;
                }

                if let Some(attempt_timeout) = static_metadata.rich_client_attempt_timeout_ms {
                    embedded_cas_daemon_config
                        .rich_client_config
                        .attempt_timeout_ms = attempt_timeout;
                }

                if let Some(retries_count) = static_metadata.rich_client_retries_count {
                    embedded_cas_daemon_config
                        .rich_client_config
                        .number_of_retries = retries_count;
                }

                embedded_cas_daemon_config.force_enable_deduplicate_find_missing =
                    static_metadata.force_enable_deduplicate_find_missing;

                // Will either choose the SOFT_COPY (on some linux fs like btrfs/extfs etc, on Mac if using APFS) or FULL_COPY otherwise
                embedded_cas_daemon_config.copy_policy = CopyPolicy::BEST_AVAILABLE;
                embedded_cas_daemon_config.meterialization_mount_path = Some(
                    re_config
                        .buck_out_path
                        .to_str()
                        .buck_error_context("invalid meterialization_mount_path")?
                        .to_owned(),
                );

                if static_metadata.cas_thread_count_ratio == 0.0 {
                    embedded_cas_daemon_config.thread_count =
                        ThreadConfig::fixed_thread_count(static_metadata.cas_thread_count);
                } else {
                    embedded_cas_daemon_config.thread_count =
                        ThreadConfig::thread_count_ratio(static_metadata.cas_thread_count_ratio);
                }

                // make sure that outputs are writable
                // otherwise actions that are modifying outputs will fail due to a permission error
                embedded_cas_daemon_config.writable_outputs = true;

                let minimal_blob_ttl_threshold =
                    static_metadata.minimal_blob_ttl_seconds.unwrap_or(3600);
                let remaining_ttl_fraction_refresh_threshold = static_metadata
                    .remaining_ttl_fraction_refresh_threshold
                    .unwrap_or(0.1)
                    as f64;
                let remaining_ttl_random_extra_threshold = static_metadata
                    .remaining_ttl_random_extra_threshold
                    .unwrap_or(0.25)
                    as f64;
                embedded_cas_daemon_config.ttl_extending_config = Some(TTLExtendingConfig {
                    blocking_ttl_extending_seconds_threshold: minimal_blob_ttl_threshold,
                    remaining_ttl_fraction_refresh_threshold,
                    remaining_ttl_random_extra_threshold,
                    ..Default::default()
                });
                embedded_cas_daemon_config.action_cache_ttl_extending_config =
                    Some(TTLExtendingConfig {
                        blocking_ttl_extending_seconds_threshold: minimal_blob_ttl_threshold,
                        remaining_ttl_fraction_refresh_threshold,
                        remaining_ttl_random_extra_threshold,
                        ..Default::default()
                    });

                re_client_config.cas_client_config =
                    CASDaemonClientCfg::embedded_config(embedded_cas_daemon_config);
                if let Some(logs_dir_path) = &re_config.logs_dir_path {
                    // make sure that the log dir exists as glog is expecting that :(
                    fs_util::create_dir_all(logs_dir_path)?;
                    re_client_config.log_file_location = Some(
                        logs_dir_path
                            .to_str()
                            .buck_error_context("Invalid log_file_location")?
                            .to_owned(),
                    );
                    // keep last 10 sessions (similar to a number of buck builds)
                    re_client_config.log_rollup_window_size = 10;
                }

                re_client_config.check_file_existence_when_no_cache_enabled = false;
                re_client_config.check_parent_directories_exist = false;
                if static_metadata.curl_reactor_max_number_of_retries.is_some()
                    || static_metadata.curl_reactor_connection_timeout_ms.is_some()
                    || static_metadata.curl_reactor_request_timeout_ms.is_some()
                {
                    let mut curl_config = CurlReactorConfig {
                        ..Default::default()
                    };

                    if let Some(max_number_of_retries) =
                        static_metadata.curl_reactor_max_number_of_retries
                    {
                        curl_config.max_number_of_retries = max_number_of_retries;
                    }

                    if let Some(request_timeout_ms) =
                        static_metadata.curl_reactor_request_timeout_ms
                    {
                        curl_config.request_timeout_ms = request_timeout_ms;
                    }

                    if let Some(connection_timeout_ms) =
                        static_metadata.curl_reactor_connection_timeout_ms
                    {
                        curl_config.connection_timeout_ms = connection_timeout_ms;
                    }

                    re_client_config.curl_reactor_config = Some(curl_config);
                }

                re_client_config.features_config_path = static_metadata
                    .features_config_path
                    .as_deref()
                    .unwrap_or(
                        if static_metadata.use_zippy_rich_client && cfg!(target_os = "linux") {
                            "remote_execution/features/client_buck2"
                        } else {
                            "remote_execution/features/client_buck2_alternative"
                        },
                    )
                    .to_owned();

                re_client_config.disable_fallocate = static_metadata.disable_fallocate;

                re_client_config
                    .thrift_execution_client_config
                    .concurrency_limit = static_metadata.execution_concurrency_limit;

                if let Some(engine_tier) = static_metadata.engine_tier.to_owned() {
                    re_client_config.thrift_execution_client_config.tier = engine_tier;
                }

                if static_metadata.engine_host.is_some() || static_metadata.engine_port.is_some() {
                    if static_metadata.engine_host.is_some()
                        && static_metadata.engine_port.is_some()
                    {
                        re_client_config.thrift_execution_client_config.host_port =
                            Some(remote_execution::HostPort {
                                host: static_metadata.engine_host.clone().unwrap(),
                                port: static_metadata.engine_port.unwrap(),
                                ..Default::default()
                            });
                    } else {
                        return Err(buck2_error!(
                            buck2_error::ErrorTag::Input,
                            "Both engine_host and engine_port must be set if either is set"
                        ));
                    }
                }

                // TODO(ndmitchell): For now, we just drop RE log messages, but ideally we'd put them in our log stream.
                let logger = slog::Logger::root(slog::Discard, slog::o!());
                // TODO T179215751: If RE client fails we don't get the RE session ID and we can't find the RE logs.
                // Better to generate the RE session ID ourselves and pass it to the RE client.
                with_error_handler(
                    op_name,
                    "<none>",
                    REClientBuilder::new(re_config.fb)
                        .with_config(re_client_config)
                        .with_logger(logger)
                        .build_and_connect()
                        .await,
                )
                .await?
            };

            #[cfg(not(fbcode_build))]
            let client = {
                with_error_handler(
                    op_name,
                    "<none>",
                    REClientBuilder::build_and_connect(&static_metadata.0).await,
                )
                .await?
            };

            let respect_file_symlinks = {
                #[cfg(fbcode_build)]
                {
                    static_metadata.respect_file_symlinks
                }
                #[cfg(not(fbcode_build))]
                {
                    false
                }
            };

            Self {
                client: Some(client),
                skip_remote_cache: re_config.skip_remote_cache,
                cas_semaphore: Arc::new(Semaphore::new(static_metadata.cas_semaphore_size())),
                download_files_semapore: Arc::new(Semaphore::new(download_concurrency)),
                download_chunk_size,
                respect_file_symlinks,
                persistent_cache_mode,
            }
        };

        res
    }

    fn client(&self) -> &REClient {
        self.client
            .as_ref()
            .expect("REClient is always present unless dropped")
    }

    fn get_session_id(&self) -> &str {
        self.client().get_session_id()
    }

    async fn action_cache(
        &self,
        action_digest: ActionDigest,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<Option<ActionResultResponse>> {
        let res = with_error_handler(
            "action_cache",
            self.get_session_id(),
            self.client()
                .get_action_cache_client()
                .get_action_result(
                    use_case.metadata(None),
                    ActionResultRequest {
                        digest: action_digest.to_re(),
                        ..Default::default()
                    },
                )
                .await,
        )
        .await;

        match res {
            Ok(r) => Ok(Some(r)),
            Err(e) => {
                let e: buck2_error::Error = e.into();
                match e.find_typed_context::<RemoteExecutionError>() {
                    Some(e) if e.code == TCode::NOT_FOUND => Ok(None),
                    _ => Err(e.into()),
                }
            }
        }
    }

    async fn upload(
        &self,
        fs: &ProjectRoot,
        materializer: &Arc<dyn Materializer>,
        blobs: &ActionBlobs,
        dir_path: &ProjectRelativePath,
        input_dir: &ActionImmutableDirectory,
        use_case: RemoteExecutorUseCase,
        identity: Option<&ReActionIdentity<'_>>,
        digest_config: DigestConfig,
    ) -> buck2_error::Result<UploadStats> {
        // Actually upload to CAS
        let _cas = self.cas_semaphore.acquire().await;

        with_error_handler(
            "upload",
            self.get_session_id(),
            Uploader::upload(
                fs,
                self.client().get_cas_client(),
                materializer,
                dir_path,
                input_dir,
                blobs,
                use_case,
                identity,
                digest_config,
            )
            .await,
        )
        .await
    }

    async fn upload_files_and_directories(
        &self,
        files_with_digest: Vec<NamedDigest>,
        directories: Vec<remote_execution::Path>,
        inlined_blobs_with_digest: Vec<InlinedBlobWithDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<()> {
        with_error_handler(
            "upload_files_and_directories",
            self.get_session_id(),
            self.client()
                .get_cas_client()
                .upload(
                    use_case.metadata(None),
                    UploadRequest {
                        files_with_digest: Some(files_with_digest),
                        inlined_blobs_with_digest: Some(inlined_blobs_with_digest),
                        directories: Some(directories),
                        upload_only_missing: true,
                        ..Default::default()
                    },
                )
                .await,
        )
        .await?;
        Ok(())
    }

    async fn execute_impl(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ExecuteRequest,
        action_digest: &ActionDigest,
        manager: &mut CommandExecutionManager,
        re_max_queue_time: Option<Duration>,
        platform: &remote_execution::Platform,
        knobs: &ExecutorGlobalKnobs,
    ) -> anyhow::Result<ExecuteResponseOrCancelled> {
        use buck2_data::re_stage;
        use buck2_data::ReAfterAction;
        use buck2_data::ReBeforeAction;
        use buck2_data::ReExecute;
        use buck2_data::ReQueue;
        use buck2_data::ReUnknown;
        use buck2_data::ReWorkerDownload;
        use buck2_data::ReWorkerUpload;

        let action_key = if knobs.log_action_keys {
            metadata
                .action_history_info
                .as_ref()
                .map(|h| h.action_key.clone())
        } else {
            None
        };

        let re_use_case = metadata.use_case_id.clone();

        #[allow(clippy::large_enum_variant)]
        enum ResponseOrStateChange {
            Present(ExecuteWithProgressResponse),
            Cancelled(Cancelled),
        }

        /// Wait for either the ExecuteResponse to show up, or a stage change, within a span
        /// on the CommandExecutionManager.
        async fn wait_for_response_or_stage_change(
            receiver: &mut BoxStream<'static, anyhow::Result<ExecuteWithProgressResponse>>,
            previous_stage: Stage,
            report_stage: re_stage::Stage,
            manager: &mut CommandExecutionManager,
            re_fallback_on_estimated_queue_time_exceeds_duration: Option<Duration>,
            re_cancel_on_estimated_queue_time_exceeds_s: Option<u32>,
        ) -> anyhow::Result<ResponseOrStateChange> {
            executor_stage_async(
                buck2_data::ReStage {
                    stage: Some(report_stage),
                },
                async move {
                    loop {
                        let next = futures::future::select(
                            manager.inner.liveliness_observer.while_alive(),
                            receiver.next(),
                        );

                        let event = match next.await {
                            futures::future::Either::Left((_dead, _)) => {
                                return Ok(ResponseOrStateChange::Cancelled(Cancelled {
                                    ..Default::default()
                                }));
                            }
                            futures::future::Either::Right((event, _)) => match event {
                                Some(event) => event,
                                None => {
                                    return Err(anyhow::anyhow!(
                                        "RE execution did not yield a ExecuteResponse"
                                    ));
                                }
                            },
                        };

                        let event = event.context("Error was returned on the stream by RE")?;

                        if event.execute_response.is_some() || event.stage != previous_stage {
                            return Ok(ResponseOrStateChange::Present(event));
                        }

                        if let Some(info) = event.metadata.task_info {
                            let est = u64::try_from(info.estimated_queue_time_ms)
                                .context("estimated_queue_time_ms from RE is negative")?;
                            let anticipated_queue_duration = Duration::from_millis(est);

                            if let Some(re_queue_threshold) =
                                re_cancel_on_estimated_queue_time_exceeds_s
                            {
                                if anticipated_queue_duration.as_secs() >= re_queue_threshold.into()
                                {
                                    return Ok(ResponseOrStateChange::Cancelled(Cancelled {
                                        reason: Some(CancellationReason::ReQueueTimeout),
                                    }));
                                }
                            }

                            if let Some(re_acceptable_anticipated_queue_duration) =
                                re_fallback_on_estimated_queue_time_exceeds_duration
                            {
                                if anticipated_queue_duration
                                    > re_acceptable_anticipated_queue_duration
                                {
                                    manager.on_result_delayed();
                                }
                            }
                        }
                    }
                },
            )
            .await
        }

        fn re_stage_from_exe_stage(
            stage: Stage,
            action_digest: String,
            platform: &remote_execution::Platform,
            action_key: &Option<String>,
            use_case: String,
        ) -> re_stage::Stage {
            match stage {
                Stage::QUEUED => re_stage::Stage::Queue(ReQueue {
                    action_digest,
                    use_case,
                }),
                Stage::MATERIALIZING_INPUT => re_stage::Stage::WorkerDownload(ReWorkerDownload {
                    action_digest,
                    use_case,
                }),
                Stage::BEFORE_ACTION => {
                    re_stage::Stage::BeforeActionExecution(ReBeforeAction { action_digest })
                }
                Stage::EXECUTING => re_stage::Stage::Execute(ReExecute {
                    action_digest,
                    platform: Some(platform_to_proto(platform)),
                    action_key: action_key.clone(),
                    use_case,
                }),
                Stage::AFTER_ACTION => {
                    re_stage::Stage::AfterActionExecution(ReAfterAction { action_digest })
                }
                Stage::UPLOADING_OUTPUT => re_stage::Stage::WorkerUpload(ReWorkerUpload {
                    action_digest,
                    use_case,
                }),
                _ => {
                    tracing::debug!(
                        "Received unexpected RE stage {:#?} for action: {}",
                        stage,
                        action_digest
                    );
                    re_stage::Stage::Unknown(ReUnknown { action_digest })
                }
            }
        }

        // There are four possible outcomes, three of them failures:
        // 1. We get Err, which means we failed to communicate to RE.
        // 2. We get Ok(error.code != TCode::Ok), which means we have an RE-level failure
        // 3. We get Ok(action_result.exit_code != 0), which means the command failed
        // 4. We get Ok(0 everywhere), which means it worked

        // Obtain a stream of events from RE. If this fails then that is case #1 above so we
        // bail.
        let mut receiver = self
            .client()
            .get_execution_client()
            .execute_with_progress(metadata, request)
            // boxed() to segment the future
            .boxed()
            .await
            .map_err(anyhow::Error::from)
            .context("Failed to start remote execution")?;

        // Now we wait until the ExecuteResponse shows up, and produce events accordingly. If
        // this doesn't give us an ExecuteResponse then this is case #1 again so we also fail.
        let action_digest_str = action_digest.to_string();
        let mut exe_stage = Stage::QUEUED;

        loop {
            let progress_response = wait_for_response_or_stage_change(
                &mut receiver,
                exe_stage,
                re_stage_from_exe_stage(
                    exe_stage,
                    action_digest_str.clone(),
                    platform,
                    &action_key,
                    re_use_case.clone(),
                ),
                manager,
                re_max_queue_time,
                knobs.re_cancel_on_estimated_queue_time_exceeds_s,
            )
            .await?;

            let progress_response = match progress_response {
                ResponseOrStateChange::Present(r) => r,
                ResponseOrStateChange::Cancelled(c) => {
                    return Ok(ExecuteResponseOrCancelled::Cancelled(c));
                }
            };

            // Return the result if we're done
            if let Some(execute_response) = progress_response.execute_response {
                return Ok(ExecuteResponseOrCancelled::Response(execute_response));
            }

            // Change the stage
            exe_stage = progress_response.stage;
        }
    }

    pub async fn execute<'a>(
        &self,
        action_digest: ActionDigest,
        platform: &RE::Platform,
        dependencies: impl IntoIterator<Item = &'a RemoteExecutorDependency>,
        use_case: RemoteExecutorUseCase,
        identity: &ReActionIdentity<'_>,
        manager: &mut CommandExecutionManager,
        skip_cache_read: bool,
        skip_cache_write: bool,
        re_max_queue_time: Option<Duration>,
        re_resource_units: Option<i64>,
        knobs: &ExecutorGlobalKnobs,
        meta_internal_extra_params: &MetaInternalExtraParams,
    ) -> buck2_error::Result<ExecuteResponseOrCancelled> {
        let metadata = RemoteExecutionMetadata {
            platform: Some(re_platform(platform)),
            do_not_cache: skip_cache_write,
            buck_info: Some(BuckInfo {
                version: buck2_build_info::revision()
                    .map(|s| s.to_owned())
                    .unwrap_or_default(),
                build_id: identity.trace_id.to_string(),
                ..Default::default()
            }),
            respect_file_symlinks: Some(self.respect_file_symlinks),
            ..use_case.metadata(Some(identity))
        };

        let request = ExecuteRequest {
            skip_cache_lookup: self.skip_remote_cache || skip_cache_read,
            execution_policy: Some(TExecutionPolicy {
                affinity_keys: vec![identity.affinity_key.clone()],
                priority: meta_internal_extra_params
                    .remote_execution_policy
                    .priority
                    .unwrap_or_default(),
                region_preference: meta_internal_extra_params
                    .remote_execution_policy
                    .region_preference
                    .clone()
                    .unwrap_or_default(),
                setup_preference_key: meta_internal_extra_params
                    .remote_execution_policy
                    .setup_preference_key
                    .clone()
                    .unwrap_or_default(),
                ..Default::default()
            }),
            action_digest: action_digest.to_re(),
            host_runtime_requirements: THostRuntimeRequirements {
                platform: re_platform(platform),
                host_resource_requirements: THostResourceRequirements {
                    input_files_bytes: identity.paths.input_files_bytes() as i64,
                    resource_units: re_resource_units.unwrap_or_default(),
                    ..Default::default()
                },
                dependencies: dependencies
                    .into_iter()
                    .map(|dep| TDependency {
                        smc_tier: dep.smc_tier.clone(),
                        id: dep.id.clone(),
                        ..Default::default()
                    })
                    .collect(),
                ..Default::default()
            },
            ..Default::default()
        };
        let re_action = format!("Execute with digest {}", &action_digest);
        with_error_handler(
            re_action.as_str(),
            self.get_session_id(),
            self.execute_impl(
                metadata,
                request,
                &action_digest,
                manager,
                re_max_queue_time,
                platform,
                knobs,
            )
            .await,
        )
        .await
    }

    /// Fetches a list of digests from the CAS and casts them to Tree objects.
    /// If fetching or decoding fails for one or more digests, returns an Err.
    async fn download_typed_blobs<T: Message + Default>(
        &self,
        identity: Option<&ReActionIdentity<'_>>,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<(Vec<T>, TLocalCacheStats)> {
        if digests.is_empty() {
            return Ok((Vec::new(), TLocalCacheStats::default()));
        }
        let expected_blobs = digests.len();
        let response = with_error_handler(
            "download_typed_blobs",
            self.get_session_id(),
            self.client()
                .get_cas_client()
                .download(
                    use_case.metadata(identity),
                    DownloadRequest {
                        inlined_digests: Some(digests),
                        ..Default::default()
                    },
                )
                .await,
        )
        .await?;

        let mut blobs: Vec<T> = Vec::with_capacity(expected_blobs);
        if let Some(ds) = response.inlined_blobs {
            for d in ds {
                blobs.push(
                    Message::decode(d.blob.as_slice()).with_buck_error_context(|| {
                        format!("Failed to Protobuf decode tree at `{}`", d.digest)
                    })?,
                );
            }
        }

        // This shouldn't happen, but we can't just assume the CAS won't ever break
        if blobs.len() != expected_blobs {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Tier0,
                "CAS client returned fewer blobs than expected."
            ));
        }

        Ok((blobs, response.local_cache_stats))
    }

    pub async fn download_blob(
        &self,
        digest: &TDigest,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<(Vec<u8>, TLocalCacheStats)> {
        let re_action = format!("download_blob for digest {}", digest);
        let response = with_error_handler(
            re_action.as_str(),
            self.get_session_id(),
            self.client()
                .get_cas_client()
                .download(
                    use_case.metadata(None),
                    DownloadRequest {
                        inlined_digests: Some(vec![digest.clone()]),
                        ..Default::default()
                    },
                )
                // boxed() to segment the future
                .boxed()
                .await,
        )
        .await?;

        response
            .inlined_blobs
            .into_iter()
            .flat_map(|blobs| blobs.into_iter())
            .next()
            .map(|blob| (blob.blob, response.local_cache_stats))
            .with_buck_error_context(|| format!("No digest was returned in request for {}", digest))
    }

    pub async fn upload_blob(
        &self,
        blob: InlinedBlobWithDigest,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<TDigest> {
        with_error_handler(
            "upload_blob",
            self.get_session_id(),
            self.client()
                .upload_blob_with_digest(blob.blob, blob.digest, use_case.metadata(None))
                .await,
        )
        .await
    }

    async fn materialize_files(
        &self,
        files: Vec<NamedDigestWithPermissions>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<TLocalCacheStats> {
        if buck2_env!(
            "BUCK2_TEST_FAIL_RE_DOWNLOADS",
            bool,
            applicability = testing
        )? {
            return Err(test_re_error("Injected error", TCode::NOT_FOUND));
        }

        let use_case = &use_case;

        let futs = chunks(files, self.download_chunk_size).map(|chunk| async move {
            let _permit = self
                .download_files_semapore
                .acquire_many(
                    chunk
                        .len()
                        .try_into()
                        .buck_error_context("chunk is too large")?,
                )
                .await
                .buck_error_context("Failed to acquire download_files_semapore")?;

            let response = with_error_handler(
                "materialize_files",
                self.get_session_id(),
                self.client()
                    .get_cas_client()
                    .download(
                        use_case.metadata(None),
                        DownloadRequest {
                            file_digests: Some(chunk),
                            ..Default::default()
                        },
                    )
                    .await,
            )
            .await?;

            buck2_error::Ok(response.local_cache_stats)
        });

        let stat = buck2_util::future::try_join_all(futs).await?.iter().fold(
            TLocalCacheStats::default(),
            |acc, x| TLocalCacheStats {
                hits_files: acc.hits_files + x.hits_files,
                hits_bytes: acc.hits_bytes + x.hits_bytes,
                misses_files: acc.misses_files + x.misses_files,
                misses_bytes: acc.misses_bytes + x.misses_bytes,
                ..Default::default()
            },
        );

        Ok(stat)
    }

    async fn get_digest_expirations(
        &self,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<Vec<(TDigest, DateTime<Utc>)>> {
        let now = Utc::now();

        let ttls = with_error_handler(
            "get_digest_expirations",
            self.get_session_id(),
            self.client()
                .get_cas_client()
                .get_digests_ttl(
                    use_case.metadata(None),
                    GetDigestsTtlRequest {
                        digests,
                        ..Default::default()
                    },
                )
                .await,
        )
        .await?
        .digests_with_ttl;

        Ok(ttls
            .into_iter()
            .map(|t| (t.digest, now + chrono::Duration::seconds(t.ttl)))
            .collect())
    }

    async fn extend_digest_ttl(
        &self,
        digests: Vec<TDigest>,
        ttl: Duration,
        use_case: RemoteExecutorUseCase,
    ) -> buck2_error::Result<()> {
        let use_case = &use_case;
        // TODO(arr): use batch API from RE when it becomes available
        with_error_handler(
            "extend_digest_ttl",
            self.get_session_id(),
            self.client()
                .get_cas_client()
                .extend_digest_ttl(
                    use_case.metadata(None),
                    ExtendDigestsTtlRequest {
                        digests,
                        ttl: ttl.as_secs() as i64,
                        ..Default::default()
                    },
                )
                .await,
        )
        .await?;
        Ok(())
    }

    async fn write_action_result(
        &self,
        digest: ActionDigest,
        result: TActionResult2,
        use_case: RemoteExecutorUseCase,
        platform: &RE::Platform,
    ) -> buck2_error::Result<WriteActionResultResponse> {
        with_error_handler(
            "write_action_result",
            self.get_session_id(),
            self.client()
                .get_action_cache_client()
                .write_action_result(
                    RemoteExecutionMetadata {
                        platform: Some(re_platform(platform)),
                        ..use_case.metadata(None)
                    },
                    WriteActionResultRequest {
                        action_digest: digest.to_re(),
                        action_result: result,
                        ..Default::default()
                    },
                )
                .await,
        )
        .await
    }
}

/// Drop the REClient on a blocking thread. The REClient destructor does a blocking wait on async
/// calls (it tells the server to cancel its calls, but it waits for an ack), so we shouldn't drop
/// it on a runtime thread.
impl Drop for RemoteExecutionClientImpl {
    fn drop(&mut self) {
        tracing::info!("Dropping RE client");
        let client = self.client.take();

        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn_blocking(move || {
                drop(client);
                tracing::info!("Dropped RE client");
            });
        }
    }
}

fn chunks<T>(v: Vec<T>, chunk_size: usize) -> impl Iterator<Item = Vec<T>> {
    if !v.is_empty() && v.len() <= chunk_size {
        return Either::Left(std::iter::once(v));
    }

    let chunks = v
        .into_iter()
        .chunks(chunk_size)
        .into_iter()
        .map(|c| c.collect::<Vec<_>>())
        .collect::<Vec<_>>();

    Either::Right(chunks.into_iter())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunks_skips() {
        assert_eq!(chunks(Vec::<usize>::new(), 1).next(), None);
    }

    #[test]
    fn test_chunks_reuses() {
        let v = vec![1, 2, 3];
        let addr = v.as_ptr() as usize;
        let mut it = chunks(v, 3);
        let first = it.next().unwrap();
        assert_eq!(first.as_ptr() as usize, addr);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_chunks_splits() {
        let v = vec![1, 2, 3];
        let mut it = chunks(v, 2);
        assert_eq!(it.next(), Some(vec![1, 2]));
        assert_eq!(it.next(), Some(vec![3]));
        assert_eq!(it.next(), None);
    }
}
