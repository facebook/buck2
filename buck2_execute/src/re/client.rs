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

use anyhow::Context;
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectRelativePath;
use either::Either;
use fbinit::FacebookInit;
use futures::stream::BoxStream;
use futures::StreamExt;
use gazebo::prelude::*;
use itertools::Itertools;
use prost::Message;
use remote_execution as RE;
use remote_execution::create_default_config;
use remote_execution::ActionHistoryInfo;
use remote_execution::ActionResultRequest;
use remote_execution::ActionResultResponse;
use remote_execution::CASDaemonClientCfg;
use remote_execution::CopyPolicy;
use remote_execution::DownloadRequest;
use remote_execution::EmbeddedCASDaemonClientCfg;
use remote_execution::ExecuteRequest;
use remote_execution::ExecuteResponse;
use remote_execution::ExecuteWithProgressResponse;
use remote_execution::HostResourceRequirements;
use remote_execution::InlinedBlobWithDigest;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;
use remote_execution::NetworkStatisticsResponse;
use remote_execution::REClient;
use remote_execution::REClientBuilder;
use remote_execution::RemoteExecutionMetadata;
use remote_execution::RichClientMode;
use remote_execution::Stage;
use remote_execution::TActionResult2;
use remote_execution::TDigest;
use remote_execution::TExecutionPolicy;
use remote_execution::TResultsCachePolicy;
use remote_execution::UploadRequest;
use remote_execution::WriteActionResultRequest;
use tokio::sync::Semaphore;
use tracing::warn;

use crate::digest::FileDigestToReExt;
use crate::directory::ActionImmutableDirectory;
use crate::execute::action_digest::ActionDigest;
use crate::execute::blobs::ActionBlobs;
use crate::execute::manager::CommandExecutionManager;
use crate::materialize::materializer::Materializer;
use crate::re::action_identity::ReActionIdentity;
use crate::re::knobs::ReExecutorGlobalKnobs;
use crate::re::metadata::RemoteExecutionMetadataExt;
use crate::re::uploader::Uploader;

static BUCK2_RE_CLIENT_CFG_SECTION: &str = "buck2_re_client";

/// Metadata that doesn't change between executions
#[derive(Clone, Debug, Default)]
pub struct RemoteExecutionStaticMetadata {
    pub cas_address: Option<String>,
    pub cas_connection_count: i32,
    pub action_cache_address: Option<String>,
    pub action_cache_connection_count: i32,
    pub engine_address: Option<String>,
    pub engine_connection_count: i32,

    pub verbose_logging: bool,

    pub use_manifold_rich_client: bool,
    pub use_zippy_rich_client: bool,
    pub use_p2p: bool,

    pub cas_thread_count: i32,

    pub rich_client_channels_per_blob: Option<i32>,
    pub rich_client_attempt_timeout_ms: Option<i32>,
    pub rich_client_retries_count: Option<i32>,
    pub force_enable_deduplicate_find_missing: Option<bool>,
}

impl RemoteExecutionStaticMetadata {
    pub fn from_legacy_config(legacy_config: &LegacyBuckConfig) -> anyhow::Result<Self> {
        Ok(Self {
            cas_address: legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_address")?,
            cas_connection_count: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_connection_count")?
                .unwrap_or(16),
            action_cache_address: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "action_cache_address")?,
            action_cache_connection_count: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "action_cache_connection_count")?
                .unwrap_or(4),
            engine_address: legacy_config.parse(BUCK2_RE_CLIENT_CFG_SECTION, "engine_address")?,
            engine_connection_count: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "engine_connection_count")?
                .unwrap_or(4),
            verbose_logging: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "verbose_logging")?
                .unwrap_or(false),
            cas_thread_count: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "cas_thread_count")?
                .unwrap_or(4),
            use_manifold_rich_client: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "use_manifold_rich_client")?
                .unwrap_or(true),
            use_zippy_rich_client: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "use_zippy_rich_client")?
                .unwrap_or(false),
            use_p2p: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "use_p2p")?
                .unwrap_or(false),
            rich_client_channels_per_blob: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "rich_client_channels_per_blob")?,
            rich_client_attempt_timeout_ms: legacy_config.parse(
                BUCK2_RE_CLIENT_CFG_SECTION,
                "rich_client_attempt_timeout_ms",
            )?,
            rich_client_retries_count: legacy_config
                .parse(BUCK2_RE_CLIENT_CFG_SECTION, "rich_client_retries_count")?,
            force_enable_deduplicate_find_missing: legacy_config.parse(
                BUCK2_RE_CLIENT_CFG_SECTION,
                "force_enable_deduplicate_find_missing",
            )?,
        })
    }
}

#[derive(Clone, Dupe)]
pub struct RemoteExecutionClient {
    data: Arc<RemoteExecutionClientData>,
}

struct RemoteExecutionClientData {
    client: RemoteExecutionClientImpl,
    /// Network stats will increase throughout the lifetime of the RE client, so we're not
    /// guaranteed that they'll start at zero (if we reuse the client). Accordingly, we track the
    /// initial value so that we can provide a delta.
    initial_network_stats: NetworkStatisticsResponse,
}

impl RemoteExecutionClient {
    pub async fn new(
        fb: FacebookInit,
        skip_remote_cache: bool,
        static_metadata: Arc<RemoteExecutionStaticMetadata>,
        logs_dir_path: Option<&str>,
        buck_out_path: &str,
    ) -> anyhow::Result<Self> {
        let client = RemoteExecutionClientImpl::new(
            fb,
            skip_remote_cache,
            static_metadata,
            logs_dir_path,
            buck_out_path,
        )
        .await?;

        let initial_network_stats = client
            .client()
            .get_network_stats()
            .context("Error getting initial network stats")?;

        Ok(Self {
            data: Arc::new(RemoteExecutionClientData {
                client,
                initial_network_stats,
            }),
        })
    }

    pub async fn new_retry(
        fb: FacebookInit,
        skip_remote_cache: bool,
        times: usize, // 0 is treated as 1
        static_metadata: Arc<RemoteExecutionStaticMetadata>,
        logs_dir_path: Option<&str>,
        buck_out_path: &str,
    ) -> anyhow::Result<Self> {
        // Loop happens times-1 times at most
        for i in 1..times {
            match Self::new(
                fb,
                skip_remote_cache,
                static_metadata.dupe(),
                logs_dir_path,
                buck_out_path,
            )
            .await
            {
                Ok(v) => return Ok(v),
                Err(e) => {
                    warn!(
                        "Failed to connect to RE, retrying after sleeping {} seconds: {:#?}",
                        i, e
                    );
                    tokio::time::sleep(Duration::from_secs(i as u64)).await;
                }
            }
        }
        Self::new(
            fb,
            skip_remote_cache,
            static_metadata,
            logs_dir_path,
            buck_out_path,
        )
        .await
    }

    fn decorate_error(&self, source: anyhow::Error) -> anyhow::Error {
        source.context(format!(
            "Remote Execution Error ({})",
            self.get_session_id()
        ))
    }

    pub async fn action_cache(
        &self,
        action_digest: ActionDigest,
        use_case: RemoteExecutorUseCase,
    ) -> Option<ActionResultResponse> {
        self.data.client.action_cache(action_digest, use_case).await
    }

    pub async fn upload(
        &self,
        materializer: &Arc<dyn Materializer>,
        blobs: &ActionBlobs,
        dir_path: &ProjectRelativePath,
        input_dir: &ActionImmutableDirectory,
        use_case: RemoteExecutorUseCase,
        knobs: &ReExecutorGlobalKnobs,
    ) -> anyhow::Result<()> {
        self.data
            .client
            .upload(materializer, blobs, dir_path, input_dir, use_case, knobs)
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub async fn upload_files_and_directories(
        &self,
        files_with_digest: Vec<NamedDigest>,
        directories: Vec<remote_execution::Path>,
        inlined_blobs_with_digest: Vec<InlinedBlobWithDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.data
            .client
            .upload_files_and_directories(
                files_with_digest,
                directories,
                inlined_blobs_with_digest,
                use_case,
            )
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub async fn execute(
        &self,
        action_digest: ActionDigest,
        platform: &RE::Platform,
        use_case: RemoteExecutorUseCase,
        identity: &ReActionIdentity<'_, '_>,
        manager: &mut CommandExecutionManager,
    ) -> anyhow::Result<ExecuteResponse> {
        self.data
            .client
            .execute(action_digest, platform, use_case, identity, manager)
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub async fn materialize_files(
        &self,
        files: Vec<NamedDigestWithPermissions>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.data.client.materialize_files(files, use_case).await
    }

    pub async fn download_trees(
        &self,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<RE::Tree>> {
        self.data
            .client
            .download_trees(digests, use_case)
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub async fn download_blob(
        &self,
        digest: &TDigest,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<u8>> {
        self.data
            .client
            .download_blob(digest, use_case)
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub async fn upload_blob(
        &self,
        blob: Vec<u8>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<TDigest> {
        self.data
            .client
            .upload_blob(blob, use_case)
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub async fn write_action_result(
        &self,
        digest: TDigest,
        result: TActionResult2,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.data
            .client
            .write_action_result(digest, result, use_case)
            .await
            .map_err(|e| self.decorate_error(e))
    }

    pub fn get_session_id(&self) -> &str {
        self.data.client.client().get_session_id()
    }

    pub fn get_experiment_name(&self) -> anyhow::Result<Option<String>> {
        self.data.client.client().get_experiment_name()
    }

    pub fn get_network_stats(&self) -> anyhow::Result<NetworkStatisticsResponse> {
        let updated = self
            .data
            .client
            .client()
            .get_network_stats()
            .context("Error getting updated network stats")?;

        Ok(NetworkStatisticsResponse {
            uploaded: updated.uploaded - self.data.initial_network_stats.uploaded,
            downloaded: updated.downloaded - self.data.initial_network_stats.downloaded,
            ..Default::default()
        })
    }
}

struct RemoteExecutionClientImpl {
    client: Option<REClient>,
    skip_remote_cache: bool,
    /// How many simultaneous requests to RE
    cas_semaphore: Arc<Semaphore>,
    /// How many files we can be downloading concurrently.
    download_files_semapore: Arc<Semaphore>,
    /// How many files to kick off downloading concurrently for one request. This should be smaller
    /// than the files semaphore to ensure we can actually *acquire* that semaphore.
    download_chunk_size: usize,
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
    async fn new(
        fb: FacebookInit,
        skip_remote_cache: bool,
        static_metadata: Arc<RemoteExecutionStaticMetadata>,
        maybe_logs_dir_path: Option<&str>,
        buck_out_path: &str,
    ) -> anyhow::Result<Self> {
        let res: anyhow::Result<Self> = try {
            static DOWNLOAD_CONCURRENCY: EnvHelper<usize> =
                EnvHelper::new("BUCK2_RE_DOWNLOAD_CONCURRENCY");

            let download_concurrency = DOWNLOAD_CONCURRENCY.get_copied()?.unwrap_or(256);

            // Split things up into smaller chunks.
            let download_chunk_size = std::cmp::max(download_concurrency / 8, 1);

            let cas_pool_size = static_metadata.cas_connection_count;

            let mut re_client_config = create_default_config();
            re_client_config.action_cache_client_config.connection_count =
                static_metadata.action_cache_connection_count;
            re_client_config.action_cache_client_config.address =
                static_metadata.action_cache_address.clone();
            re_client_config.execution_client_config.connection_count =
                static_metadata.engine_connection_count;
            re_client_config.execution_client_config.address =
                static_metadata.engine_address.clone();

            let mut embedded_cas_daemon_config = EmbeddedCASDaemonClientCfg {
                connection_count: cas_pool_size,
                address: static_metadata.cas_address.clone(),
                name: "buck2".to_owned(),
                ..Default::default()
            };

            // buck2 makes find_missing calls for the same blobs
            // so having a 50Mb cache to amortize that
            embedded_cas_daemon_config
                .cache_config
                .find_missing_cache_size_byte = 50 << 20;
            // a small cache maps inodes to digests
            // useful for both uploads and downloads
            embedded_cas_daemon_config.cache_config.digest_cache_size = 100000;

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
            embedded_cas_daemon_config.rich_client_config.disable_p2p = !static_metadata.use_p2p;
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
            embedded_cas_daemon_config.meterialization_mount_path = Some(buck_out_path.to_owned());

            embedded_cas_daemon_config.thread_count = static_metadata.cas_thread_count;

            // make sure that outputs are writable
            // otherwise actions that are modifying outputs will fail due to a permission error
            embedded_cas_daemon_config.writable_outputs = true;

            re_client_config.cas_client_config =
                CASDaemonClientCfg::embedded_config(embedded_cas_daemon_config);
            if let Some(logs_dir_path) = maybe_logs_dir_path {
                // make sure that the log dir exists as glog is expecting that :(
                std::fs::create_dir_all(logs_dir_path)?;
                re_client_config.log_file_location = Some(logs_dir_path.to_owned());
                // keep last 10 sessions (similar to a number of buck builds)
                re_client_config.log_rollup_window_size = 10;
            }

            re_client_config.features_config_path =
                "remote_execution/features/re_client_buck2".to_owned();

            // TODO(ndmitchell): For now, we just drop RE log messages, but ideally we'd put them in our log stream.
            let logger = slog::Logger::root(slog::Discard, slog::o!());
            let client = REClientBuilder::new(fb)
                .with_config(re_client_config)
                .with_logger(logger)
                .build();
            Self {
                client: Some(client),
                skip_remote_cache,
                cas_semaphore: Arc::new(Semaphore::new(cas_pool_size as usize * 30)),
                download_files_semapore: Arc::new(Semaphore::new(download_concurrency)),
                download_chunk_size,
            }
        };
        res.context("RE: creating client")
    }

    fn client(&self) -> &REClient {
        self.client
            .as_ref()
            .expect("REClient is always present unless dropped")
    }

    async fn action_cache(
        &self,
        action_digest: ActionDigest,
        use_case: RemoteExecutorUseCase,
    ) -> Option<ActionResultResponse> {
        self.client()
            .get_action_cache_client()
            .get_action_result(
                use_case.metadata(),
                ActionResultRequest {
                    digest: action_digest.0.to_re(),
                    ..Default::default()
                },
            )
            .await
            .ok()
    }

    async fn upload(
        &self,
        materializer: &Arc<dyn Materializer>,
        blobs: &ActionBlobs,
        dir_path: &ProjectRelativePath,
        input_dir: &ActionImmutableDirectory,
        use_case: RemoteExecutorUseCase,
        knobs: &ReExecutorGlobalKnobs,
    ) -> anyhow::Result<()> {
        // Actually upload to CAS
        let _cas = self.cas_semaphore.acquire().await;
        Uploader::upload(
            self.client().get_cas_client(),
            materializer,
            dir_path,
            input_dir,
            blobs,
            use_case,
            knobs,
        )
        .await
    }

    async fn upload_files_and_directories(
        &self,
        files_with_digest: Vec<NamedDigest>,
        directories: Vec<remote_execution::Path>,
        inlined_blobs_with_digest: Vec<InlinedBlobWithDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.client()
            .get_cas_client()
            .upload(
                use_case.metadata(),
                UploadRequest {
                    files_with_digest: Some(files_with_digest),
                    inlined_blobs_with_digest: Some(inlined_blobs_with_digest),
                    directories: Some(directories),
                    upload_only_missing: true,
                    ..Default::default()
                },
            )
            .await?;
        Ok(())
    }

    /// Execute an action, retrying if there are infra errors etc.
    /// This command may return an an Err (we couldn't connect to RE)
    /// or a Ok whose state is failure.
    async fn execute_action_with_retry(
        &self,
        metadata: RemoteExecutionMetadata,
        request: ExecuteRequest,
        action_digest: &ActionDigest,
        manager: &mut CommandExecutionManager,
    ) -> anyhow::Result<ExecuteResponse> {
        use buck2_data::re_stage;
        use buck2_data::ReExecute;
        use buck2_data::ReQueue;
        use buck2_data::ReUnknown;
        use buck2_data::ReWorkerDownload;
        use buck2_data::ReWorkerUpload;

        /// Wait for either the ExecuteResponse to show up, or a stage change, within a span
        /// on the CommandExecutionManager.
        async fn wait_for_response_or_stage_change(
            receiver: &mut BoxStream<'static, anyhow::Result<ExecuteWithProgressResponse>>,
            previous_stage: Stage,
            report_stage: re_stage::Stage,
            manager: &mut CommandExecutionManager,
        ) -> anyhow::Result<ExecuteWithProgressResponse> {
            manager
                .stage_async(
                    buck2_data::ReStage {
                        stage: Some(report_stage),
                    },
                    async move {
                        while let Some(event) = receiver.next().await {
                            let event = event.context("Error was returned on the stream by RE")?;
                            if event.execute_response.is_some() || event.stage != previous_stage {
                                return Ok(event);
                            }
                        }
                        Err(anyhow::anyhow!(
                            "RE execution did not yield a ExecuteResponse"
                        ))
                    },
                )
                .await
        }

        fn re_stage_from_exe_stage(stage: Stage, action_digest: String) -> re_stage::Stage {
            match stage {
                Stage::QUEUED => re_stage::Stage::Queue(ReQueue { action_digest }),
                Stage::MATERIALIZING_INPUT => {
                    re_stage::Stage::WorkerDownload(ReWorkerDownload { action_digest })
                }
                Stage::EXECUTING => re_stage::Stage::Execute(ReExecute { action_digest }),
                Stage::UPLOADING_OUTPUT => {
                    re_stage::Stage::WorkerUpload(ReWorkerUpload { action_digest })
                }
                _ => {
                    warn!(
                        "Received unexpected RE stage {:#?} for action: {}",
                        stage, action_digest
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
            .execute_with_progress(metadata.clone(), request.clone())
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
                re_stage_from_exe_stage(exe_stage, action_digest_str.clone()),
                manager,
            )
            .await?;

            // Return the result if we're done
            if let Some(execute_response) = progress_response.execute_response {
                return Ok(execute_response);
            }

            // Change the stage
            exe_stage = progress_response.stage;
        }
    }

    pub async fn execute(
        &self,
        action_digest: ActionDigest,
        platform: &RE::Platform,
        use_case: RemoteExecutorUseCase,
        identity: &ReActionIdentity<'_, '_>,
        manager: &mut CommandExecutionManager,
    ) -> anyhow::Result<ExecuteResponse> {
        let metadata = RemoteExecutionMetadata {
            action_history_info: Some(ActionHistoryInfo {
                action_key: identity.action_key.clone(),
                disable_retry_on_oom: false,
                ..Default::default()
            }),
            host_resource_requirements: Some(HostResourceRequirements {
                affinity_keys: vec![identity.affinity_key.clone()],
                input_files_bytes: identity.action_paths.input_files_bytes as i64,
                ..Default::default()
            }),
            platform: Some(re_platform(platform)),
            ..use_case.metadata()
        };
        let request = ExecuteRequest {
            skip_cache_lookup: self.skip_remote_cache,
            execution_policy: Some(TExecutionPolicy::default()),
            // Cache for as long as we can
            results_cache_policy: Some(TResultsCachePolicy {
                priority: if self.skip_remote_cache { 0 } else { i32::MAX },
                ..Default::default()
            }),
            action_digest: action_digest.0.to_re(),
            ..Default::default()
        };
        self.execute_action_with_retry(metadata, request, &action_digest, manager)
            .await
            .with_context(|| format!("RE: execution with digest {}", &action_digest))
    }

    /// Fetches a list of digests from the CAS and casts them to Tree objects.
    /// If fetching or decoding fails for one or more digests, returns an Err.
    async fn download_trees(
        &self,
        digests: Vec<TDigest>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<RE::Tree>> {
        if digests.is_empty() {
            return Ok(Vec::new());
        }
        let expected_trees = digests.len();
        let response = self
            .client()
            .get_cas_client()
            .download(
                use_case.metadata(),
                DownloadRequest {
                    inlined_digests: Some(digests),
                    ..Default::default()
                },
            )
            .await?;

        let mut trees: Vec<RE::Tree> = Vec::with_capacity(expected_trees);
        if let Some(ds) = response.inlined_blobs {
            for d in ds {
                trees.push(Message::decode(d.blob.as_slice()).with_context(|| {
                    format!("Failed to Protobuf decode tree at `{}`", d.digest)
                })?);
            }
        }

        // This shouldn't happen, but we can't just assume the CAS won't ever break
        assert_eq!(
            trees.len(),
            expected_trees,
            "CAS client returned fewer trees than expected."
        );

        Ok(trees)
    }

    pub async fn download_blob(
        &self,
        digest: &TDigest,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<Vec<u8>> {
        let response = self
            .client()
            .get_cas_client()
            .download(
                use_case.metadata(),
                DownloadRequest {
                    inlined_digests: Some(vec![digest.clone()]),
                    ..Default::default()
                },
            )
            .await
            .with_context(|| format!("Download request failed for digest {}", digest))?;

        response
            .inlined_blobs
            .into_iter()
            .flat_map(|blobs| blobs.into_iter())
            .next()
            .map(|blob| blob.blob)
            .with_context(|| format!("No digest was returned in request for {}", digest))
    }

    pub async fn upload_blob(
        &self,
        blob: Vec<u8>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<TDigest> {
        self.client().upload_blob(blob, use_case.metadata()).await
    }

    async fn materialize_files(
        &self,
        files: Vec<NamedDigestWithPermissions>,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        let use_case = &use_case;

        let futs = chunks(files, self.download_chunk_size).map(|chunk| async move {
            let _permit = self
                .download_files_semapore
                .acquire_many(chunk.len().try_into().context("chunk is too large")?)
                .await
                .context("Failed to acquire download_files_semapore")?;

            self.client()
                .get_cas_client()
                .download(
                    use_case.metadata(),
                    DownloadRequest {
                        file_digests: Some(chunk),
                        ..Default::default()
                    },
                )
                .await?;

            anyhow::Ok(())
        });

        futures::future::try_join_all(futs).await?;

        Ok(())
    }

    async fn write_action_result(
        &self,
        digest: TDigest,
        result: TActionResult2,
        use_case: RemoteExecutorUseCase,
    ) -> anyhow::Result<()> {
        self.client()
            .get_action_cache_client()
            .write_action_result(
                use_case.metadata(),
                WriteActionResultRequest {
                    action_digest: digest,
                    action_result: result,
                    ..Default::default()
                },
            )
            .await?;

        Ok(())
    }
}

/// Drop the REClient on a blocking thread. The REClient destructor does a blocking wait on async
/// calls (it tells the server to cancel its calls, but it waits for an ack), so we shouldn't drop
/// it on a runtime thread.
impl Drop for RemoteExecutionClientImpl {
    fn drop(&mut self) {
        let client = self.client.take();

        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn_blocking(move || drop(client));
        }
    }
}

#[allow(clippy::needless_collect)] // chunks() is not Send.
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
        assert_eq!(it.next().unwrap().as_ptr() as usize, addr);
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
