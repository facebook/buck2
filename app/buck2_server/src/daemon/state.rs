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
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use buck2_build_api::spawner::BuckSpawner;
use buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use buck2_common::cas_digest::DigestAlgorithm;
use buck2_common::cas_digest::DigestAlgorithmFamily;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::init::DaemonStartupConfig;
use buck2_common::init::SystemWarningConfig;
use buck2_common::init::Timeout;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::sqlite::sqlite_db::SqliteDb;
use buck2_common::sqlite::sqlite_db::SqliteIdentity;
use buck2_core::buck2_env;
use buck2_core::cells::name::CellName;
use buck2_core::configuration::data::init_deconflict_content_based_paths_rollout;
use buck2_core::facebook_only;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::is_open_source;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_core::tag_result;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::buck2_error;
use buck2_events::EventSinkWithStats;
use buck2_events::daemon_id::DaemonId;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::sink::remote;
use buck2_events::sink::tee::TeeSink;
use buck2_events::source::ChannelEventSource;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::BuckBlockingExecutor;
use buck2_execute::execute::blocking::DirectIoExecutor;
use buck2_execute::materialize::materializer::MaterializationMethod;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_execute_impl::executors::local::ForkserverAccess;
use buck2_execute_impl::materializers::deferred::AccessTimesUpdates;
use buck2_execute_impl::materializers::deferred::DeferredMaterializer;
use buck2_execute_impl::materializers::deferred::DeferredMaterializerConfigs;
use buck2_execute_impl::materializers::deferred::TtlRefreshConfiguration;
use buck2_execute_impl::materializers::deferred::clean_stale::CleanStaleConfig;
use buck2_execute_impl::re::paranoid_download::ParanoidDownloader;
use buck2_execute_impl::sqlite::incremental_state_db::IncrementalDbState;
use buck2_execute_impl::sqlite::materializer_db::MaterializerState;
use buck2_execute_impl::sqlite::materializer_db::MaterializerStateSqliteDb;
use buck2_file_watcher::file_watcher::FileWatcher;
use buck2_fs::cwd::WorkingDirectory;
use buck2_http::HttpClient;
use buck2_http::HttpClientBuilder;
use buck2_re_configuration::RemoteExecutionStaticMetadata;
use buck2_re_configuration::RemoteExecutionStaticMetadataImpl;
use buck2_resource_control::buck_cgroup_tree::BuckCgroupTree;
use buck2_resource_control::memory_tracker;
use buck2_resource_control::memory_tracker::MemoryTrackerHandle;
use buck2_server_ctx::concurrency::ConcurrencyHandler;
use buck2_server_ctx::ctx::LockedPreviousCommandData;
use buck2_wrapper_common::invocation_id::TraceId;
use dupe::Dupe;
use fbinit::FacebookInit;
use gazebo::prelude::*;
use gazebo::variants::VariantName;
use host_sharing::NamedSemaphores;
use remote::ScribeConfig;
use tokio::runtime::Handle;
use tokio::sync::Mutex;

use crate::active_commands::ActiveCommandDropGuard;
use crate::ctx::BaseServerCommandContext;
use crate::daemon::check_working_dir;
use crate::daemon::disk_state::DiskStateOptions;
use crate::daemon::disk_state::delete_unknown_disk_state;
use crate::daemon::disk_state::maybe_initialize_incremental_sqlite_db;
use crate::daemon::disk_state::maybe_initialize_materializer_sqlite_db;
use crate::daemon::forkserver::maybe_launch_forkserver;
use crate::daemon::io_provider::create_io_provider;
use crate::daemon::panic::DaemonStatePanicDiceDump;
use crate::daemon::server::BuckdServerInitPreferences;

/// For a buckd process there is a single DaemonState created at startup and never destroyed.
#[derive(Allocative)]
pub struct DaemonState {
    #[allocative(skip)]
    fb: fbinit::FacebookInit,

    pub paths: InvocationPaths,

    /// This holds the main data shared across different commands.
    pub(crate) data: Arc<DaemonStateData>,

    /// Our working directory, if we did set one.
    working_directory: Option<WorkingDirectory>,
}

/// DaemonStateData is the main shared data across all commands. It's lazily initialized on
/// the first command that requires it.
#[derive(Allocative)]
pub struct DaemonStateData {
    /// The Dice computation graph. Generally, we shouldn't add things to the DaemonStateData
    /// (or DaemonState) itself and instead they should be represented on the computation graph.
    ///
    /// The DICE graph is held by the concurrency handler to manage locking for concurrent commands
    pub(crate) dice_manager: Arc<ConcurrencyHandler>,

    /// Synced every time we run a command.
    pub(crate) file_watcher: Arc<dyn FileWatcher>,

    /// Settled every time we run a command.
    pub io: Arc<dyn IoProvider>,

    /// The RE connection, managed such that all build commands that are concurrently active uses
    /// the same connection. Once there are no active build commands, the connection will be
    /// terminated
    pub re_client_manager: Arc<ReConnectionManager>,

    /// Executor responsible for coordinating and rate limiting I/O.
    pub blocking_executor: Arc<dyn BlockingExecutor>,

    /// Most materializations go through the materializer, providing a single point
    /// where the most expensive network and fs IO operations are performed. It
    /// needs access to the `ReConnectionManager` to download from RE. It must
    /// live for the entire lifetime of the daemon, in order to allow deferred
    /// materializations to work properly between distinct build commands.
    pub(crate) materializer: Arc<dyn Materializer>,

    pub(crate) forkserver: ForkserverAccess,

    #[allocative(skip)]
    pub scribe_sink: Option<Arc<dyn EventSinkWithStats>>,

    /// Whether to consult the offline-cache buck-out dir for network action
    /// outputs prior to running them. If no cached output exists, the action
    /// (download_file, cas_artifact) will execute normally.
    ///
    /// This supports fully-offline builds, where network actions like
    /// download_file have an execution component that is inherently non-local (
    /// e.g. making a HEAD request against the remote artifact to determine if
    /// it needs to be downloaded again).
    pub use_network_action_output_cache: bool,

    /// What buck2 state to store on disk, ex. materializer state on sqlite
    pub disk_state_options: DiskStateOptions,

    pub start_time: Instant,

    #[allocative(skip)]
    pub create_unhashed_outputs_lock: Arc<Mutex<()>>,

    /// A unique identifier for the materializer state.
    pub materializer_state_identity: Option<SqliteIdentity>,

    /// Whether to enable the restarter. This controls whether the client will attempt to restart
    /// the daemon when we hit an error.
    pub enable_restarter: bool,

    /// Http client used for materializer and RunAction implementations.
    pub http_client: HttpClient,

    /// If enabled, paranoid RE downloads.
    pub paranoid: Option<ParanoidDownloader>,

    /// Spawner
    pub spawner: Arc<BuckSpawner>,

    /// Tags to be logged per command.
    pub tags: Vec<String>,

    /// Config used to display system warnings
    pub system_warning_config: SystemWarningConfig,

    /// Tracks memory usage. Used to make scheduling decisions.
    #[allocative(skip)]
    pub memory_tracker: Option<MemoryTrackerHandle>,

    /// Tracks data about previous command (e.g. configs)
    pub previous_command_data: Arc<LockedPreviousCommandData>,

    /// State of the Incremental Action DB for content-based hash paths
    #[allocative(skip)]
    pub incremental_db_state: Arc<IncrementalDbState>,

    /// A unique identifier for this instance of the daemon
    pub daemon_id: DaemonId,

    /// Semaphores for running actions locally. These need to be shared across commands.
    #[allocative(skip)]
    pub named_semaphores_for_run_actions: Arc<NamedSemaphores>,
}

impl DaemonStateData {
    pub fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> buck2_error::Result<()> {
        crate::daemon::dice_dump::dice_dump(self.dice_manager.unsafe_dice(), path, format)
    }

    pub async fn spawn_dice_dump(
        &self,
        path: &Path,
        format: DiceDumpFormat,
    ) -> buck2_error::Result<()> {
        crate::daemon::dice_dump::dice_dump_spawn(self.dice_manager.unsafe_dice(), path, format)
            .await
    }
}

impl DaemonStatePanicDiceDump for DaemonStateData {
    fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> buck2_error::Result<()> {
        self.dice_dump(path, format)
    }
}

impl DaemonState {
    #[tracing::instrument(name = "daemon_listener", skip_all)]
    pub(crate) async fn new(
        fb: fbinit::FacebookInit,
        paths: InvocationPaths,
        init_ctx: BuckdServerInitPreferences,
        rt: &Handle,
        materializations: MaterializationMethod,
        working_directory: Option<WorkingDirectory>,
        cgroup_tree: Option<BuckCgroupTree>,
        daemon_id: DaemonId,
    ) -> Result<Self, buck2_error::Error> {
        let data = Self::init_data(
            fb,
            paths.clone(),
            init_ctx,
            rt,
            materializations,
            cgroup_tree,
            daemon_id,
        )
        .await
        .map_err(|e| {
            e.context("Error initializing DaemonStateData")
                .tag([ErrorTag::DaemonStateInitFailed])
        })?;

        crate::daemon::panic::initialize(data.dupe());

        tracing::info!("Daemon state is ready.");

        let state = DaemonState {
            fb,
            paths,
            data,
            working_directory,
        };
        Ok(state)
    }

    // Creates the initial DaemonStateData.
    // Starts up the watchman query.
    async fn init_data(
        fb: fbinit::FacebookInit,
        paths: InvocationPaths,
        init_ctx: BuckdServerInitPreferences,
        rt: &Handle,
        materializations: MaterializationMethod,
        cgroup_tree: Option<BuckCgroupTree>,
        daemon_id: DaemonId,
    ) -> buck2_error::Result<Arc<DaemonStateData>> {
        if buck2_env!(
            "BUCK2_TEST_INIT_DAEMON_ERROR",
            bool,
            applicability = testing
        )? {
            // TODO(minglunli): Errors here don't actually make it to invocation records which should be fixed
            return Err(buck2_error::buck2_error!(
                ErrorTag::TestOnly,
                "Injected init daemon error"
            ));
        }

        let daemon_state_data_rt = rt.clone();
        rt.spawn(async move {
            let fs = paths.project_root().clone();

            tracing::info!("Reading config...");
            let legacy_cells = BuckConfigBasedCells::parse_with_config_args(&fs, &[]).await?;

            tracing::info!("Starting...");

            let cells = &legacy_cells.cell_resolver;
            let root_config = &legacy_cells
                .parse_single_cell(cells.root_cell(), &fs)
                .await?;

            let buffer_size = root_config
                .parse(BuckconfigKeyRef {
                    section: "buck2",
                    property: "event_log_buffer_size",
                })?
                .unwrap_or(10000);
            let retry_backoff = Duration::from_millis(
                root_config
                    .parse(BuckconfigKeyRef {
                        section: "buck2",
                        property: "event_log_retry_backoff_duration_ms",
                    })?
                    .unwrap_or(500),
            );
            let retry_attempts = root_config
                .parse(BuckconfigKeyRef {
                    section: "buck2",
                    property: "event_log_retry_attempts",
                })?
                .unwrap_or(5);
            let message_batch_size = root_config.parse(BuckconfigKeyRef {
                section: "buck2",
                property: "event_log_message_batch_size",
            })?;
            let scribe_sink = Self::init_scribe_sink(
                fb,
                ScribeConfig {
                    buffer_size,
                    retry_backoff,
                    retry_attempts,
                    message_batch_size,
                    thrift_timeout: Duration::from_secs(1),
                },
            )
            .buck_error_context("failed to init scribe sink")?;

            let default_digest_algorithm =
                buck2_env!("BUCK_DEFAULT_DIGEST_ALGORITHM", type=DigestAlgorithmFamily)?;

            let default_digest_algorithm = default_digest_algorithm.unwrap_or_else(|| {
                if buck2_core::is_open_source() {
                    DigestAlgorithmFamily::Sha256
                } else {
                    DigestAlgorithmFamily::Sha1
                }
            });

            let digest_algorithms = init_ctx
                .daemon_startup_config
                .digest_algorithms
                .as_ref()
                .map(|algos| {
                    algos
                        .split(',')
                        .map(DigestAlgorithmFamily::from_str)
                        .collect::<Result<_, _>>()
                })
                .transpose()
                .buck_error_context("Invalid digest_algorithms")?
                .unwrap_or_else(|| vec![default_digest_algorithm])
                .into_try_map(convert_algorithm_kind)?;

            let preferred_source_algorithm = init_ctx
                .daemon_startup_config
                .source_digest_algorithm
                .as_deref()
                .map(|a| convert_algorithm_kind(a.parse()?))
                .transpose()
                .buck_error_context("Invalid source_digest_algorithm")?;

            let digest_config =
                DigestConfig::leak_new(digest_algorithms, preferred_source_algorithm)
                    .buck_error_context("Error initializing DigestConfig")?;

            // TODO(rafaelc): merge configs from all cells once they are consistent
            let static_metadata = Arc::new(RemoteExecutionStaticMetadata::from_legacy_config(
                root_config,
            )?);

            let mut ignore_specs: HashMap<CellName, IgnoreSet> = HashMap::new();
            for (cell, _) in cells.cells() {
                let config = legacy_cells.parse_single_cell(cell, &fs).await?;
                ignore_specs.insert(
                    cell,
                    IgnoreSet::from_ignore_spec(
                        config
                            .get(BuckconfigKeyRef {
                                section: "project",
                                property: "ignore",
                            })
                            .unwrap_or(""),
                        cells.is_root_cell(cell),
                    )?,
                );
            }

            let disk_state_options = DiskStateOptions::new(root_config, materializations.dupe())?;

            let blocking_executor: Arc<dyn BlockingExecutor> =
                if cfg!(any(target_os = "macos", target_os = "windows")) {
                    Arc::new(DirectIoExecutor::new(fs.dupe())?)
                } else {
                    Arc::new(BuckBlockingExecutor::default_concurrency(fs.dupe())?)
                };

            let cache_dir_path = paths.cache_dir_path();
            let valid_cache_dirs = paths.valid_cache_dirs();

            let deferred_materializer_configs = {
                let defer_write_actions = root_config
                    .parse::<RolloutPercentage>(BuckconfigKeyRef {
                        section: "buck2",
                        property: "defer_write_actions",
                    })?
                    .unwrap_or_else(RolloutPercentage::never)
                    .roll();

                // RE will refresh any TTL < 1 hour, so we check twice an hour and refresh any TTL
                // < 1 hour.
                let ttl_refresh_frequency = root_config
                    .parse(BuckconfigKeyRef {
                        section: "buck2",
                        property: "ttl_refresh_frequency_seconds",
                    })?
                    .unwrap_or(1800);

                let ttl_refresh_min_ttl = root_config
                    .parse(BuckconfigKeyRef {
                        section: "buck2",
                        property: "ttl_refresh_min_ttl_seconds",
                    })?
                    .unwrap_or(3600);

                let ttl_refresh_enabled = root_config
                    .parse::<RolloutPercentage>(BuckconfigKeyRef {
                        section: "buck2",
                        property: "ttl_refresh_enabled",
                    })?
                    .unwrap_or_else(RolloutPercentage::never)
                    .roll();

                let update_access_times = AccessTimesUpdates::try_new_from_config_value(
                    root_config.get(BuckconfigKeyRef {
                        section: "buck2",
                        property: "update_access_times",
                    }),
                )?;

                let verbose_materializer_log = root_config
                    .parse(BuckconfigKeyRef {
                        section: "buck2",
                        property: "verbose_materializer_event_log",
                    })?
                    .unwrap_or(false);

                let clean_stale_config = CleanStaleConfig::from_buck_config(root_config)?;

                let disable_eager_write_dispatch = root_config
                    .parse::<RolloutPercentage>(BuckconfigKeyRef {
                        section: "buck2",
                        property: "disable_eager_write_dispatch",
                    })?
                    .unwrap_or_else(RolloutPercentage::never)
                    .roll();

                DeferredMaterializerConfigs {
                    materialize_final_artifacts: matches!(
                        materializations,
                        MaterializationMethod::Deferred
                    ),
                    defer_write_actions,
                    ttl_refresh: TtlRefreshConfiguration {
                        frequency: std::time::Duration::from_secs(ttl_refresh_frequency),
                        min_ttl: chrono::Duration::seconds(ttl_refresh_min_ttl),
                        enabled: ttl_refresh_enabled,
                    },
                    update_access_times,
                    verbose_materializer_log,
                    clean_stale_config,
                    disable_eager_write_dispatch,
                }
            };
            let disable_eager_write_dispatch =
                deferred_materializer_configs.disable_eager_write_dispatch;

            let use_eden_thrift_read = root_config
                .parse(BuckconfigKeyRef {
                    section: "buck2",
                    property: "use_eden_thrift_read",
                })?
                .unwrap_or(cfg!(any(target_os = "macos", target_os = "windows")));

            let (io, _, (materializer_db, materializer_state), incremental_db_state) =
                futures::future::try_join4(
                    create_io_provider(
                        fb,
                        fs.dupe(),
                        root_config,
                        digest_config.cas_digest_config(),
                        init_ctx.enable_trace_io,
                        use_eden_thrift_read,
                    ),
                    (blocking_executor.dupe() as Arc<dyn BlockingExecutor>).execute_io_inline(
                        || {
                            // Using `execute_io_inline` is just out of convenience.
                            // It doesn't really matter what's used here since there's no IO-heavy
                            // operations on daemon startup
                            delete_unknown_disk_state(&cache_dir_path, &valid_cache_dirs)
                        },
                    ),
                    maybe_initialize_materializer_sqlite_db(
                        &disk_state_options,
                        paths.clone(),
                        blocking_executor.dupe() as Arc<dyn BlockingExecutor>,
                        root_config,
                        &deferred_materializer_configs,
                        digest_config,
                        &init_ctx,
                        &daemon_id,
                    ),
                    maybe_initialize_incremental_sqlite_db(
                        paths.clone(),
                        blocking_executor.dupe() as Arc<dyn BlockingExecutor>,
                        root_config,
                        &daemon_id,
                    ),
                )
                .await?;

            let http_client = http_client_from_startup_config(&init_ctx.daemon_startup_config)
                .await
                .buck_error_context("Error creating HTTP client")?
                .build();

            let incremental_db_state = Arc::new(incremental_db_state);

            let materializer_state_identity =
                materializer_db.as_ref().map(|d| d.identity().clone());

            let re_client_manager = Arc::new(ReConnectionManager::new(
                fb,
                false,
                10,
                static_metadata.dupe(),
                Some(paths.re_logs_dir()),
                paths.buck_out_path(),
                init_ctx.daemon_startup_config.paranoid,
            ));
            // Used only to dispatch events to scribe that are not associated with a specific command (ex. materializer clean up events)
            let daemon_dispatcher = if let Some(sink) = scribe_sink.dupe() {
                EventDispatcher::new(TraceId::null(), daemon_id.dupe(), sink.to_event_sync())
            } else {
                // If needed this could log to a sink that redirects to a daemon event log (maybe `~/.buck/buckd/repo-path/event-log`)
                // but for now seems fine to drop events if scribe isn't enabled.
                EventDispatcher::null()
            };
            let materializer = Self::create_materializer(
                io.project_root().dupe(),
                digest_config,
                paths.buck_out_dir(),
                re_client_manager.dupe(),
                blocking_executor.dupe(),
                materializations,
                deferred_materializer_configs,
                materializer_db,
                materializer_state,
                http_client.dupe(),
                daemon_dispatcher.dupe(),
            )?;

            let memory_tracker = memory_tracker::create_memory_tracker(
                cgroup_tree,
                &init_ctx.daemon_startup_config.resource_control,
                &daemon_id,
            )
            .await?;

            // Create this after the materializer because it'll want to write to buck-out, and an Eden
            // materializer would create buck-out now.
            let forkserver = maybe_launch_forkserver(
                root_config,
                &paths.forkserver_state_dir(),
                memory_tracker.as_ref().map(|m| &m.cgroup_tree),
            )
            .await?;

            let dice = init_ctx
                .construct_dice(io.dupe(), digest_config, root_config)
                .await?;

            let file_watcher = <dyn FileWatcher>::new(
                fb,
                paths.project_root(),
                root_config,
                cells.dupe(),
                ignore_specs,
            )
            .with_buck_error_context(|| {
                format!(
                    "Error creating a FileWatcher for project root `{}`",
                    paths.project_root()
                )
            })?;

            let use_network_action_output_cache = root_config
                .parse(BuckconfigKeyRef {
                    section: "buck2",
                    property: "use_network_action_output_cache",
                })?
                .unwrap_or(false);

            let create_unhashed_outputs_lock = Arc::new(Mutex::new(()));

            let enable_restarter = root_config
                .parse::<RolloutPercentage>(BuckconfigKeyRef {
                    section: "buck2",
                    property: "restarter",
                })?
                .unwrap_or_else(RolloutPercentage::never)
                .roll();

            let paranoid = if init_ctx.daemon_startup_config.paranoid {
                Some(ParanoidDownloader::new(
                    fs.clone(),
                    blocking_executor.dupe(),
                    re_client_manager.dupe(),
                    paths.paranoid_cache_dir(),
                ))
            } else {
                None
            };

            let remote_dep_files_enabled = root_config
                .parse(BuckconfigKeyRef {
                    section: "build",
                    property: "remote_dep_file_cache_enabled",
                })?
                .unwrap_or(false);

            let action_freezing_enabled = init_ctx
                .daemon_startup_config
                .resource_control
                .enable_suspension;

            let tags = vec![
                format!("dice-detect-cycles:{}", dice.detect_cycles().variant_name()),
                // TODO(scottcao): Delete this tag since now hash all commands is always enabled.
                "hash-all-commands:true".to_owned(),
                format!(
                    "sqlite-materializer-state:{}",
                    disk_state_options.sqlite_materializer_state
                ),
                format!("paranoid:{}", paranoid.is_some()),
                format!("remote-dep-files:{}", remote_dep_files_enabled),
                #[cfg(fbcode_build)]
                format!(
                    "respect-file-symlinks:{}",
                    static_metadata.respect_file_symlinks
                ),
                format!(
                    "disable-eager-write-dispatch-v2:{}",
                    disable_eager_write_dispatch,
                ),
                format!("use-eden-thrift-read:{}", use_eden_thrift_read),
                format!("memory_tracker-enabled:{}", memory_tracker.is_some()),
                format!("action-freezing-enabled:{}", action_freezing_enabled),
                format!("has-cgroup:{}", memory_tracker.is_some()),
            ];
            let system_warning_config = SystemWarningConfig::from_config(root_config)?;

            // TODO(jtbraun): Modifies action digest, remove after confirming bvb works fine.
            let deconflict_content_based_paths_rollout = root_config.parse(BuckconfigKeyRef {
                section: "buck2",
                property: "deconflict_content_based_paths_rollout",
            })?;
            init_deconflict_content_based_paths_rollout(deconflict_content_based_paths_rollout)?;

            // Kick off an initial sync eagerly. This gets Watchamn to start watching the path we care
            // about (potentially kicking off an initial crawl).
            // disable the eager spawn for watchman until we fix dice commit to avoid a panic TODO(bobyf)
            // tokio::task::spawn(watchman_query.sync());
            Ok(Arc::new(DaemonStateData {
                dice_manager: ConcurrencyHandler::new(dice),
                file_watcher,
                io,
                re_client_manager,
                blocking_executor,
                materializer,
                forkserver,
                scribe_sink,
                use_network_action_output_cache,
                disk_state_options,
                start_time: std::time::Instant::now(),
                create_unhashed_outputs_lock,
                materializer_state_identity,
                enable_restarter,
                http_client,
                paranoid,
                spawner: Arc::new(BuckSpawner::new(daemon_state_data_rt)),
                tags,
                system_warning_config,
                memory_tracker,
                previous_command_data: LockedPreviousCommandData::new(),
                incremental_db_state,
                daemon_id: daemon_id.dupe(),
                named_semaphores_for_run_actions: Arc::new(NamedSemaphores::new()),
            }))
        })
        .await?
    }

    fn create_materializer(
        fs: ProjectRoot,
        digest_config: DigestConfig,
        buck_out_path: ProjectRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materializations: MaterializationMethod,
        deferred_materializer_configs: DeferredMaterializerConfigs,
        materializer_db: Option<MaterializerStateSqliteDb>,
        materializer_state: Option<MaterializerState>,
        http_client: HttpClient,
        daemon_dispatcher: EventDispatcher,
    ) -> buck2_error::Result<Arc<dyn Materializer>> {
        match materializations {
            MaterializationMethod::Deferred | MaterializationMethod::DeferredSkipFinalArtifacts => {
                Ok(Arc::new(DeferredMaterializer::new(
                    fs,
                    digest_config,
                    buck_out_path,
                    re_client_manager,
                    blocking_executor,
                    deferred_materializer_configs,
                    materializer_db,
                    materializer_state,
                    http_client,
                    daemon_dispatcher,
                )?))
            }
        }
    }

    fn init_scribe_sink(
        fb: FacebookInit,
        config: ScribeConfig,
    ) -> buck2_error::Result<Option<Arc<dyn EventSinkWithStats>>> {
        facebook_only();
        remote::new_remote_event_sink_if_enabled(fb, config)
            .map(|maybe_scribe| maybe_scribe.map(|scribe| Arc::new(scribe) as _))
    }

    /// Prepares an event stream for a request by bootstrapping an event source and EventDispatcher pair. The given
    /// EventDispatcher will log to the returned EventSource and (optionally) to Scribe if enabled via buckconfig.
    pub async fn prepare_events(
        &self,
        trace_id: TraceId,
    ) -> buck2_error::Result<(ChannelEventSource, EventDispatcher)> {
        // facebook only: logging events to Scribe.
        facebook_only();
        let (events, sink) = buck2_events::create_source_sink_pair();
        let data = self.data();
        let dispatcher = if let Some(scribe_sink) = data.scribe_sink.dupe() {
            EventDispatcher::new(
                trace_id,
                self.data.daemon_id.dupe(),
                TeeSink::new(scribe_sink.to_event_sync(), sink),
            )
        } else {
            EventDispatcher::new(trace_id, self.data.daemon_id.dupe(), sink)
        };
        Ok((events, dispatcher))
    }

    /// Prepares a ServerCommandContext for processing a complex command (that accesses the dice computation graph, for example).
    ///
    /// This initializes (if necessary) the shared daemon state and syncs the watchman query (to flush any recent filesystem events).
    pub async fn prepare_command(
        &self,
        dispatcher: EventDispatcher,
        drop_guard: ActiveCommandDropGuard,
    ) -> buck2_error::Result<BaseServerCommandContext> {
        let data = self.data();

        dispatcher.instant_event(buck2_data::RestartConfiguration {
            enable_restarter: data.enable_restarter,
        });

        tag_result!(
            "eden_not_connected",
            check_working_dir::check_working_dir().map_err(|e| e.into()),
            quiet: true,
            daemon_in_memory_state_is_corrupted: true,
            task: false
        )?;

        self.validate_cwd()
            .buck_error_context("Error validating working directory")?;

        self.validate_buck_out_mount()
            .buck_error_context("Error validating buck-out mount")?;

        dispatcher.instant_event(buck2_data::TagEvent {
            tags: data.tags.clone(),
        });

        // Sync any FS changes and invalidate DICE state if necessary.  Get the Eden
        // version of the underlying system in parallel if available.
        let (_, eden_version) =
            futures::future::try_join(data.io.settle(), data.io.eden_version()).await?;

        dispatcher.instant_event(buck2_data::IoProviderInfo { eden_version });

        Ok(BaseServerCommandContext {
            _fb: self.fb,
            project_root: self.paths.project_root().clone(),
            events: dispatcher,
            daemon: data.dupe(), // FIXME: Remove the duplicative fields.
            _drop_guard: drop_guard,
            spawner: data.spawner.dupe(),
        })
    }

    pub fn data(&self) -> Arc<DaemonStateData> {
        self.data.dupe()
    }

    pub fn validate_cwd(&self) -> buck2_error::Result<()> {
        if let Some(working_directory) = &self.working_directory {
            let res = working_directory.is_stale().and_then(|stale| {
                if stale {
                    Err(buck2_error!(
                        buck2_error::ErrorTag::Environment,
                        "Buck appears to be running in a stale working directory. \
                         This will likely lead to failed or slow builds. \
                         To remediate, restart Buck2."
                    ))
                } else {
                    Ok(())
                }
            });

            tag_result!(
                "stale_cwd",
                res.map_err(|e| e.into()),
                quiet: true,
                daemon_in_memory_state_is_corrupted: true,
                task: false
            )?;
        }

        Ok(())
    }

    pub fn validate_buck_out_mount(&self) -> buck2_error::Result<()> {
        #[cfg(fbcode_build)]
        {
            use buck2_core::soft_error;
            use buck2_fs::error::IoResultExt;
            use buck2_fs::fs_util;

            let project_root = self.paths.project_root().root();
            if !detect_eden::is_eden(project_root.to_path_buf())? {
                return Ok(());
            }

            let buck_out_root = project_root.join(InvocationPaths::buck_out_dir_prefix());

            if let Some(buck_out_root_meta) = fs_util::symlink_metadata_if_exists(buck_out_root)? {
                // If buck-out is a symlink, we'll be happy with that.
                if buck_out_root_meta.is_symlink() {
                    return Ok(());
                }

                // If we are on UNIX, then buck-out could also be on a different device from the repo.
                // We don't check which kind of device, we just assume it's not mounted completely
                // wrong.
                #[cfg(unix)]
                {
                    use std::os::unix::fs::MetadataExt;

                    let project_device = fs_util::symlink_metadata(project_root)
                        .categorize_internal()?
                        .dev();
                    let buck_out_device = buck_out_root_meta.dev();

                    if project_device != buck_out_device {
                        return Ok(());
                    }
                }
            }

            soft_error!(
                "eden_buck_out",
                buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Environment,
                    "Buck is running in an Eden repository, but `buck-out` is not redirected. \
                     This will likely lead to failed or slow builds. \
                     To remediate, run `eden redirect fixup`."
                )
                .into(),
                quiet:false
            )?;
        }

        Ok(())
    }
}

fn convert_algorithm_kind(kind: DigestAlgorithmFamily) -> buck2_error::Result<DigestAlgorithm> {
    buck2_error::Ok(match kind {
        DigestAlgorithmFamily::Sha1 => DigestAlgorithm::Sha1,
        DigestAlgorithmFamily::Sha256 => DigestAlgorithm::Sha256,
        DigestAlgorithmFamily::Blake3 => DigestAlgorithm::Blake3,
        DigestAlgorithmFamily::Blake3Keyed => {
            #[cfg(fbcode_build)]
            {
                let key = blake3_constants::BLAKE3_HASH_KEY;
                DigestAlgorithm::Blake3Keyed { key }
            }

            #[cfg(not(fbcode_build))]
            {
                // We probably should just add it as a separate buckconfig, there is
                // zero reason not to.
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "{} is not supported in the open source build",
                    kind
                ));
            }
        }
    })
}

/// Sensible defaults for http client when building from a DaemonStartupConfig.
const DEFAULT_MAX_REDIRECTS: usize = 10;
const DEFAULT_CONNECT_TIMEOUT_MS: u64 = 5000;
const DEFAULT_READ_TIMEOUT_MS: u64 = 10000;

/// Customize an http client based on http.* legacy buckconfigs.
async fn http_client_from_startup_config(
    config: &DaemonStartupConfig,
) -> buck2_error::Result<HttpClientBuilder> {
    let mut builder = if is_open_source() {
        HttpClientBuilder::oss().await?
    } else {
        HttpClientBuilder::internal().await?
    };
    builder.with_max_redirects(config.http.max_redirects.unwrap_or(DEFAULT_MAX_REDIRECTS));
    builder.with_http2(config.http.http2);
    builder.with_max_concurrent_requests(config.http.max_concurrent_requests);

    match config.http.connect_timeout() {
        Timeout::Value(d) => {
            builder.with_connect_timeout(Some(d));
        }
        Timeout::Default => {
            builder.with_connect_timeout(Some(Duration::from_millis(DEFAULT_CONNECT_TIMEOUT_MS)));
        }
        _ => {}
    }
    match config.http.read_timeout() {
        Timeout::Value(d) => {
            builder.with_read_timeout(Some(d));
        }
        Timeout::Default => {
            builder.with_read_timeout(Some(Duration::from_millis(DEFAULT_READ_TIMEOUT_MS)));
        }
        _ => {}
    }
    match config.http.write_timeout() {
        Timeout::Value(d) => {
            builder.with_write_timeout(Some(d));
        }
        _ => {}
    }

    Ok(builder)
}

#[cfg(test)]
mod tests {

    use buck2_common::legacy_configs::configs::testing::parse;
    use indoc::indoc;

    use super::*;

    #[tokio::test]
    async fn test_from_startup_config_defaults_internal() -> buck2_error::Result<()> {
        buck2_certs::certs::maybe_setup_cryptography();
        let builder =
            http_client_from_startup_config(&DaemonStartupConfig::testing_empty()).await?;
        assert_eq!(DEFAULT_MAX_REDIRECTS, builder.max_redirects().unwrap());
        assert_eq!(
            builder.supports_vpnless(),
            buck2_certs::certs::supports_vpnless()
        );
        assert_eq!(
            Some(Duration::from_millis(DEFAULT_CONNECT_TIMEOUT_MS)),
            builder.connect_timeout()
        );
        assert_eq!(
            Some(Duration::from_millis(DEFAULT_READ_TIMEOUT_MS)),
            builder.read_timeout()
        );
        assert_eq!(None, builder.write_timeout());

        Ok(())
    }

    #[tokio::test]
    async fn test_from_startup_config_overrides() -> buck2_error::Result<()> {
        buck2_certs::certs::maybe_setup_cryptography();
        let config = parse(
            &[(
                "config",
                indoc!(
                    r#"
                    [http]
                    max_redirects = 5
                    connect_timeout_ms = 10
                    write_timeout_ms = 5
                    "#
                ),
            )],
            "config",
        )?;
        let startup_config = DaemonStartupConfig::new(&config)?;
        let builder = http_client_from_startup_config(&startup_config).await?;
        assert_eq!(5, builder.max_redirects().unwrap());
        assert_eq!(Some(Duration::from_millis(10)), builder.connect_timeout());
        assert_eq!(
            Some(Duration::from_millis(DEFAULT_READ_TIMEOUT_MS)),
            builder.read_timeout()
        );
        assert_eq!(Some(Duration::from_millis(5)), builder.write_timeout());

        Ok(())
    }

    #[tokio::test]
    async fn test_from_startup_config_zero_for_unset() -> buck2_error::Result<()> {
        buck2_certs::certs::maybe_setup_cryptography();
        let config = parse(
            &[(
                "config",
                indoc!(
                    r#"
                    [http]
                    connect_timeout_ms = 0
                    "#,
                ),
            )],
            "config",
        )?;
        let startup_config = DaemonStartupConfig::new(&config)?;
        let builder = http_client_from_startup_config(&startup_config).await?;
        assert_eq!(None, builder.connect_timeout());
        assert_eq!(
            Some(Duration::from_millis(DEFAULT_READ_TIMEOUT_MS)),
            builder.read_timeout()
        );
        assert_eq!(None, builder.write_timeout());

        Ok(())
    }
}
