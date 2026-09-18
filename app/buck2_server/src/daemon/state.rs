/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::sync::Mutex as StdMutex;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use buck2_build_api::spawner::BuckSpawner;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use buck2_common::cas_digest::DigestAlgorithm;
use buck2_common::cas_digest::DigestAlgorithmFamily;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_common::init::DaemonStartupConfig;
use buck2_common::init::SystemWarningConfig;
use buck2_common::init::Timeout;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::invocation_paths::TenantPaths;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_common::legacy_configs::parse_buckconfig_metadata;
use buck2_common::sqlite::sqlite_db::SqliteDb;
use buck2_common::sqlite::sqlite_db::SqliteIdentity;
use buck2_common::tenant::TenantKey;
use buck2_common::tenant::TenantSpec;
use buck2_core::buck2_env;
use buck2_core::cells::name::CellName;
use buck2_core::facebook_only;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::is_open_source;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_core::soft_error;
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
use buck2_execute::dep_file_state::DepFileStore;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::BlockingExecutorFactory;
use buck2_execute::materialize::materializer::FinalArtifactMaterialization;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_execute_impl::executors::local::ForkserverAccess;
use buck2_execute_impl::materializers::deferred::AccessTimesUpdates;
use buck2_execute_impl::materializers::deferred::DeferredMaterializer;
use buck2_execute_impl::materializers::deferred::DeferredMaterializerConfigs;
use buck2_execute_impl::materializers::deferred::TtlRefreshConfiguration;
use buck2_execute_impl::materializers::deferred::clean_stale::CleanStaleConfig;
use buck2_execute_impl::re::paranoid_download::ParanoidDownloader;
use buck2_execute_impl::sqlite::dep_file_state_db::PersistedDepFileStore;
use buck2_execute_impl::sqlite::incremental_state_db::IncrementalDbState;
use buck2_execute_impl::sqlite::materializer_db::MaterializerState;
use buck2_execute_impl::sqlite::materializer_db::MaterializerStateSqliteDb;
use buck2_file_watcher::dep_files::DepFileCache;
use buck2_file_watcher::dep_files::create_dep_file_cache;
use buck2_file_watcher::file_watcher::FileWatcher;
use buck2_fs::cwd::WorkingDirectory;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_hash::StdBuckHashMap;
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
use tokio::sync::OnceCell;
use tracing::Instrument;

use crate::active_commands::ActiveCommandDropGuard;
use crate::ctx::BaseServerCommandContext;
use crate::daemon::check_working_dir;
use crate::daemon::disk_state::DiskStateOptions;
use crate::daemon::disk_state::delete_unknown_disk_state;
use crate::daemon::disk_state::maybe_initialize_dep_file_sqlite_db;
use crate::daemon::disk_state::maybe_initialize_incremental_sqlite_db;
use crate::daemon::disk_state::maybe_initialize_materializer_sqlite_db;
use crate::daemon::forkserver::maybe_launch_forkserver;
use crate::daemon::io_provider::create_io_provider;
use crate::daemon::panic::DaemonStatePanicDiceDump;
use crate::daemon::server::BuckdServerInitPreferences;
use crate::daemon::server::RepoStateInitPreferences;
use crate::daemon::tenting_provider::create_tenting_acl_provider;
use crate::paging::PageOutThresholds;
use crate::snapshot::DepFileDbSizeSampler;

/// For a buckd process there is a single DaemonState created at startup and never destroyed.
#[derive(Allocative)]
pub struct DaemonState {
    #[allocative(skip)]
    fb: fbinit::FacebookInit,

    /// This holds the main data shared across different commands.
    pub(crate) data: Arc<DaemonStateData>,

    /// Our working directory.
    working_directory: WorkingDirectory,
}

#[derive(Allocative)]
pub(crate) struct PersistedDepFileCache {
    #[allocative(skip)]
    pub(crate) store: Arc<dyn DepFileStore>,

    #[allocative(skip)]
    pub(crate) db_size: Arc<DepFileDbSizeSampler>,
}

/// State scoped to one tenant.
///
/// A tenant is the state that historically belonged to one `(project root, isolation)` daemon.
/// A shared daemon can hold multiple tenants, including multiple isolations for one project root.
#[derive(Allocative)]
pub struct RepoState {
    /// Stable paths used by tenant services. Invocation cwd is not stored here.
    pub paths: TenantPaths,

    /// The Dice computation graph. Generally, we shouldn't add things to the DaemonStateData
    /// (or DaemonState) itself and instead they should be represented on the computation graph.
    ///
    /// The DICE graph is held by the concurrency handler to manage locking for concurrent commands
    pub(crate) dice_manager: Arc<ConcurrencyHandler>,

    /// Synced every time we run a command.
    pub(crate) file_watcher: Arc<dyn FileWatcher>,

    /// Settled every time we run a command.
    pub io: Arc<dyn IoProvider>,

    /// Most materializations go through the materializer, providing a single point
    /// where the most expensive network and fs IO operations are performed. It
    /// needs access to the `ReConnectionManager` to download from RE. It must
    /// live for the entire lifetime of the daemon, in order to allow deferred
    /// materializations to work properly between distinct build commands.
    pub(crate) materializer: Arc<dyn Materializer>,

    /// Whether to consult the offline-cache buck-out dir for network action
    /// outputs prior to running them. If no cached output exists, the action
    /// (download_file, cas_artifact) will execute normally.
    ///
    /// This supports fully-offline builds, where network actions like
    /// download_file have an execution component that is inherently non-local (
    /// e.g. making a HEAD request against the remote artifact to determine if
    /// it needs to be downloaded again).
    pub use_network_action_output_cache: bool,

    /// Whether a command selecting this repo should ask the client to restart the daemon after an
    /// error.
    pub restart_daemon_on_error: bool,

    /// What buck2 state to store on disk, ex. materializer state on sqlite
    pub disk_state_options: DiskStateOptions,

    #[allocative(skip)]
    pub create_unhashed_outputs_lock: Arc<Mutex<()>>,

    /// A unique identifier for the materializer state.
    pub materializer_state_identity: Option<SqliteIdentity>,

    /// Tracks data about previous command (e.g. configs)
    pub previous_command_data: Arc<LockedPreviousCommandData>,

    /// State of the Incremental Action DB for content-based hash paths
    #[allocative(skip)]
    pub incremental_db_state: Arc<IncrementalDbState>,

    /// Persisted local dep-file cache and its size sampler for this repo, if enabled.
    pub(crate) persisted_dep_file_cache: Option<PersistedDepFileCache>,

    /// Live local dep-file cache for this repo.
    pub dep_file_cache: Arc<dyn DepFileCache>,

    /// If enabled, paranoid RE downloads.
    pub paranoid: Option<ParanoidDownloader>,

    /// The RE connection for this repo, managed such that all concurrently active build commands
    /// use the same connection. Once there are no active build commands, the connection is
    /// terminated.
    pub re_client_manager: Arc<ReConnectionManager>,

    /// Executor for blocking I/O against this repo's project root.
    pub blocking_executor: Arc<dyn BlockingExecutor>,

    pub buckconfig_metadata: StdBuckHashMap<String, String>,

    /// Tags to be logged per command.
    pub tags: Vec<String>,

    /// Config used to display system warnings
    pub system_warning_config: SystemWarningConfig,

    /// Whether to verify on each command that the Eden daemon backing this repo's `io` has not
    /// restarted underneath us. A restart invalidates cached state and file handles, so affected
    /// commands fail fast instead of hanging (`buck2.detect_eden_restart`).
    ///
    /// Repo-scoped because the identity baseline it checks against is captured per `io`, from the
    /// Eden socket named by this repo's checkout. Repos on one machine normally share an Eden
    /// daemon, so the answer is usually the same for all of them, but nothing requires that.
    pub detect_eden_restart: bool,

    /// Whether a finishing command schedules a background sweep of the local-action
    /// scratch dirs (`buck-out/<iso>/tmp*`) once the daemon is idle
    /// (`buck2.clean_scratch_on_idle`). The sweep runs through this repo's `materializer`.
    pub(crate) clean_scratch_on_idle: bool,

    /// Resource-pressure thresholds for automatic idle page-out, selected for this tenant's
    /// isolation. `None` disables automatic idle page-out for this tenant.
    pub(crate) page_out_on_idle: Option<PageOutThresholds>,
}

struct RepoStateInit<'a> {
    fb: FacebookInit,
    paths: TenantPaths,
    init_ctx: &'a RepoStateInitPreferences,
    legacy_cells: &'a BuckConfigBasedCells,
    root_config: &'a LegacyBuckConfig,
    final_artifact_materialization: FinalArtifactMaterialization,
    runtime: &'a Handle,
    shared: DaemonSharedServices<'a>,
}

struct DaemonSharedServices<'a> {
    blocking_executor_factory: &'a BlockingExecutorFactory,
    scribe_sink: Option<&'a Arc<dyn EventSinkWithStats>>,
    http_client: &'a HttpClient,
    memory_tracker: Option<&'a MemoryTrackerHandle>,
    daemon_id: &'a DaemonId,
}

#[derive(Allocative)]
struct RepoStateFactory {
    #[allocative(skip)]
    fb: FacebookInit,
    init_ctx: RepoStateInitPreferences,
    #[allocative(skip)]
    final_artifact_materialization: FinalArtifactMaterialization,
    #[allocative(skip)]
    runtime: Handle,
}

impl RepoStateFactory {
    async fn create(
        &self,
        paths: TenantPaths,
        shared: DaemonSharedServices<'_>,
    ) -> buck2_error::Result<Arc<RepoState>> {
        let buck_out_path = paths.buck_out_path();
        tokio::fs::create_dir_all(&buck_out_path)
            .await
            .tag(ErrorTag::InvalidBuckOut)
            .buck_error_context("Error creating buck_out_path")?;

        let fs = paths.project_root().clone();
        let legacy_cells = BuckConfigBasedCells::parse_with_config_args(&fs, &[]).await?;
        let cells = &legacy_cells.cell_resolver;
        let root_config = &legacy_cells
            .parse_single_cell(cells.root_cell(), &fs)
            .await?;

        self.create_with_loaded_config(paths, &legacy_cells, root_config, shared)
            .await
    }

    async fn create_with_loaded_config(
        &self,
        paths: TenantPaths,
        legacy_cells: &BuckConfigBasedCells,
        root_config: &LegacyBuckConfig,
        shared: DaemonSharedServices<'_>,
    ) -> buck2_error::Result<Arc<RepoState>> {
        RepoState::create(RepoStateInit {
            fb: self.fb,
            paths,
            init_ctx: &self.init_ctx,
            legacy_cells,
            root_config,
            final_artifact_materialization: self.final_artifact_materialization,
            runtime: &self.runtime,
            shared,
        })
        .await
    }
}

impl RepoState {
    async fn create(init: RepoStateInit<'_>) -> buck2_error::Result<Arc<Self>> {
        let RepoStateInit {
            fb,
            paths,
            init_ctx,
            legacy_cells,
            root_config,
            final_artifact_materialization,
            runtime,
            shared,
        } = init;
        let fs = paths.project_root().clone();
        let cells = &legacy_cells.cell_resolver;

        let default_digest_algorithm =
            buck2_env!("BUCK_DEFAULT_DIGEST_ALGORITHM", type=DigestAlgorithmFamily)?;

        let default_digest_algorithm = default_digest_algorithm.unwrap_or_else(|| {
            if is_open_source() {
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

        let digest_config = DigestConfig::leak_new(digest_algorithms, preferred_source_algorithm)
            .buck_error_context("Error initializing DigestConfig")?;

        // TODO(rafaelc): merge configs from all cells once they are consistent
        let static_metadata = Arc::new(RemoteExecutionStaticMetadata::from_legacy_config(
            root_config,
        )?);

        let mut ignore_specs: StdBuckHashMap<CellName, IgnoreSet> = StdBuckHashMap::default();
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

        let disk_state_options = DiskStateOptions::new(root_config)?;
        let blocking_executor = shared.blocking_executor_factory.for_project(fs.dupe());

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

            let update_access_times =
                AccessTimesUpdates::try_new_from_config_value(root_config.get(BuckconfigKeyRef {
                    section: "buck2",
                    property: "update_access_times",
                }))?;

            let verbose_materializer_log = root_config
                .parse(BuckconfigKeyRef {
                    section: "buck2",
                    property: "verbose_materializer_event_log",
                })?
                .unwrap_or(false);

            let mut clean_stale_config = CleanStaleConfig::from_buck_config(root_config)?;
            clean_stale_config.suppress_unmaterialize_without_ttl_refresh(ttl_refresh_enabled);

            DeferredMaterializerConfigs {
                materialize_final_artifacts: matches!(
                    final_artifact_materialization,
                    FinalArtifactMaterialization::Enabled
                ),
                defer_write_actions,
                ttl_refresh: TtlRefreshConfiguration {
                    frequency: Duration::from_secs(ttl_refresh_frequency),
                    min_ttl: jiff::SignedDuration::from_secs(ttl_refresh_min_ttl),
                    enabled: ttl_refresh_enabled,
                },
                update_access_times,
                verbose_materializer_log,
                clean_stale_config,
            }
        };

        let use_eden_thrift_read = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "use_eden_thrift_read",
            })?
            .unwrap_or(cfg!(any(target_os = "macos", target_os = "windows")));

        tracing::info!("Creating materializer...");
        let (io, _, (materializer_db, materializer_state), incremental_db_state, dep_file_db) =
            futures::future::try_join5(
                create_io_provider(
                    fb,
                    fs.dupe(),
                    root_config,
                    digest_config.cas_digest_config(),
                    init_ctx.enable_trace_io,
                    use_eden_thrift_read,
                ),
                (blocking_executor.dupe() as Arc<dyn BlockingExecutor>).execute_io_inline(|| {
                    // Using `execute_io_inline` is just out of convenience.
                    // It doesn't really matter what's used here since there's no IO-heavy
                    // operations on daemon startup.
                    delete_unknown_disk_state(&cache_dir_path, &valid_cache_dirs)
                }),
                maybe_initialize_materializer_sqlite_db(
                    &disk_state_options,
                    paths.clone(),
                    blocking_executor.dupe() as Arc<dyn BlockingExecutor>,
                    root_config,
                    &deferred_materializer_configs,
                    digest_config,
                    init_ctx,
                    shared.daemon_id,
                ),
                maybe_initialize_incremental_sqlite_db(
                    paths.clone(),
                    blocking_executor.dupe() as Arc<dyn BlockingExecutor>,
                    root_config,
                    shared.daemon_id,
                ),
                maybe_initialize_dep_file_sqlite_db(
                    &disk_state_options,
                    paths.clone(),
                    blocking_executor.dupe() as Arc<dyn BlockingExecutor>,
                    root_config,
                    shared.daemon_id,
                ),
            )
            .await?;

        // The cache is opt-in and best-effort, so a store that cannot be built leaves the
        // repo running without persistence rather than failing startup.
        let persisted_dep_file_cache = dep_file_db.and_then(|dep_file_db| {
            match PersistedDepFileStore::try_new(dep_file_db, digest_config) {
                Ok(store) => {
                    let store = Arc::new(store) as Arc<dyn DepFileStore>;
                    Some(PersistedDepFileCache {
                        db_size: DepFileDbSizeSampler::start(store.dupe(), runtime),
                        store,
                    })
                }
                Err(e) => {
                    let _unused = soft_error!(
                        "dep_file_store_init",
                        buck2_error::buck2_error!(
                            buck2_error::ErrorTag::Tier0,
                            "Failed to start the persisted dep-file cache; continuing without \
                             it. {}",
                            e
                        ),
                        quiet: true
                    );
                    None
                }
            }
        });
        let incremental_db_state = Arc::new(incremental_db_state);
        let materializer_state_identity = materializer_db.as_ref().map(|d| d.identity().clone());

        let re_client_manager = Arc::new(ReConnectionManager::new(
            fb,
            false,
            10,
            static_metadata.dupe(),
            Some(paths.re_logs_dir()),
            paths.buck_out_path(),
            init_ctx.daemon_startup_config.paranoid,
        ));
        // Used only to dispatch events to scribe that are not associated with a specific command
        // (ex. materializer clean up events).
        let daemon_dispatcher = if let Some(sink) = shared.scribe_sink {
            EventDispatcher::new(
                TraceId::null(),
                shared.daemon_id.dupe(),
                sink.dupe().to_event_sync(),
            )
        } else {
            EventDispatcher::null()
        };
        let materializer = Self::create_materializer(
            io.project_root().dupe(),
            digest_config,
            paths.buck_out_dir(),
            re_client_manager.dupe(),
            blocking_executor.dupe(),
            deferred_materializer_configs,
            materializer_db,
            materializer_state,
            shared.http_client.dupe(),
            daemon_dispatcher,
        )?;

        tracing::info!("Creating tenting ACL provider...");
        let tenting_acl_provider = create_tenting_acl_provider(fb, paths.project_root());

        tracing::info!("Constructing DICE...");
        let dice = init_ctx
            .construct_dice(
                io.dupe(),
                digest_config,
                root_config,
                tenting_acl_provider,
                paths.dice_state_path().as_ref(),
            )
            .await?;

        let dep_file_cache = create_dep_file_cache();

        tracing::info!("Creating file watcher...");
        let file_watcher = <dyn FileWatcher>::new(
            fb,
            paths.project_root(),
            root_config,
            cells.dupe(),
            ignore_specs,
            dep_file_cache.dupe(),
        )
        .with_buck_error_context(|| {
            format!(
                "Error creating a FileWatcher for project root `{}`",
                paths.project_root()
            )
        })?;

        // TODO(bobyf): Eagerly sync the file watcher here once the DICE commit panic is fixed.

        let use_network_action_output_cache = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "use_network_action_output_cache",
            })?
            .unwrap_or(false);

        let detect_eden_restart = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "detect_eden_restart",
            })?
            .unwrap_or(false);

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

        let page_out_on_idle = init_ctx
            .daemon_startup_config
            .idle_page_out_config_for_isolation_dir(paths.isolation())
            .map(|hydration| PageOutThresholds {
                min_free_disk_gb: hydration.page_out_min_free_disk_gb,
            });

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
            "disable-eager-write-dispatch-v2:true".to_owned(),
            format!("use-eden-thrift-read:{}", use_eden_thrift_read),
            format!("memory_tracker-enabled:{}", shared.memory_tracker.is_some()),
            format!("action-freezing-enabled:{}", action_freezing_enabled),
            format!("has-cgroup:{}", shared.memory_tracker.is_some()),
        ];

        let repo = Arc::new(Self {
            paths,
            dice_manager: ConcurrencyHandler::new(dice),
            file_watcher,
            io,
            materializer,
            use_network_action_output_cache,
            restart_daemon_on_error: root_config
                .parse::<RolloutPercentage>(BuckconfigKeyRef {
                    section: "buck2",
                    property: "restarter",
                })?
                .unwrap_or_else(RolloutPercentage::never)
                .roll(),
            disk_state_options,
            create_unhashed_outputs_lock: Arc::new(Mutex::new(())),
            materializer_state_identity,
            previous_command_data: LockedPreviousCommandData::new(),
            incremental_db_state,
            persisted_dep_file_cache,
            dep_file_cache,
            paranoid,
            re_client_manager,
            blocking_executor,
            buckconfig_metadata: parse_buckconfig_metadata(root_config),
            tags,
            system_warning_config: SystemWarningConfig::from_config(root_config)?,
            detect_eden_restart,
            clean_scratch_on_idle: root_config
                .parse::<RolloutPercentage>(BuckconfigKeyRef {
                    section: "buck2",
                    property: "clean_scratch_on_idle",
                })?
                .unwrap_or_else(RolloutPercentage::never)
                .roll(),
            page_out_on_idle,
        });

        #[cfg(fbcode_build)]
        {
            let root_path = std::path::PathBuf::from(repo.paths.project_root().root().as_os_str());
            if !buck2_env!("BUCK2_DISABLE_EDEN_HEALTH_CHECK", bool)?
                && detect_eden::is_eden(root_path).unwrap_or(false)
            {
                tracing::trace!("EdenFS root detected; starting health check job");
                crate::daemon::server::eden_health::edenfs_health_check(
                    fb,
                    repo.paths.project_root().dupe(),
                )
                .await;
            }
        }

        Ok(repo)
    }

    pub async fn spawn_dice_dump(
        &self,
        path: &Path,
        format: DiceDumpFormat,
    ) -> buck2_error::Result<()> {
        crate::daemon::dice_dump::dice_dump_spawn(self.dice_manager.unsafe_dice(), path, format)
            .await
    }

    fn create_materializer(
        fs: ProjectRoot,
        digest_config: DigestConfig,
        buck_out_path: ProjectRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        deferred_materializer_configs: DeferredMaterializerConfigs,
        materializer_db: Option<MaterializerStateSqliteDb>,
        materializer_state: Option<MaterializerState>,
        http_client: HttpClient,
        daemon_dispatcher: EventDispatcher,
    ) -> buck2_error::Result<Arc<dyn Materializer>> {
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

/// Tenant states known to this daemon.
///
/// The registry initially contains the tenant that started the daemon. Clients without an explicit
/// tenant identity continue to use that initial tenant for protocol compatibility.
#[derive(Allocative)]
struct TenantStateRegistry {
    initial_tenant: TenantKey,
    /// Stable handle for legacy single-repo callers. Registry entries are never replaced after
    /// insertion, and the same allocation is accounted for through `tenants`.
    #[allocative(skip)]
    initial_state: Arc<RepoState>,
    tenants: StdMutex<StdBuckHashMap<TenantKey, Arc<TenantStateEntry<RepoState>>>>,
}

struct TenantStateEntry<T: Allocative> {
    spec: TenantSpec,
    state: OnceCell<Arc<T>>,
}

impl<T: Allocative> Allocative for TenantStateEntry<T> {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        visitor.visit_field(allocative::Key::new("spec"), &self.spec);
        if let Some(state) = self.state.get() {
            visitor.visit_field(allocative::Key::new("state"), state);
        }
        visitor.exit();
    }
}

impl<T: Allocative> TenantStateEntry<T> {
    fn new(spec: TenantSpec) -> Self {
        Self {
            spec,
            state: OnceCell::new(),
        }
    }

    async fn get_or_try_init<F, Fut>(&self, init: F) -> buck2_error::Result<Arc<T>>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = buck2_error::Result<Arc<T>>>,
    {
        self.state.get_or_try_init(init).await.map(Arc::clone)
    }

    fn is_initialized(&self) -> bool {
        self.state.get().is_some()
    }
}

impl TenantStateRegistry {
    async fn new(initial_tenant: Arc<RepoState>) -> buck2_error::Result<Self> {
        let spec = TenantSpec::from_tenant_paths(&initial_tenant.paths);
        let initial_key = spec.key().clone();
        let registry = Self {
            initial_tenant: initial_key,
            initial_state: initial_tenant.dupe(),
            tenants: StdMutex::new(StdBuckHashMap::default()),
        };
        registry
            .get_or_create(spec, || async { Ok(initial_tenant) })
            .await?;
        Ok(registry)
    }

    async fn get_or_create<F, Fut>(
        &self,
        spec: TenantSpec,
        create: F,
    ) -> buck2_error::Result<Arc<RepoState>>
    where
        F: FnOnce() -> Fut,
        Fut: Future<Output = buck2_error::Result<Arc<RepoState>>>,
    {
        let key = spec.key().clone();
        let requested_spec = spec.clone();
        let entry = {
            let mut tenants = self
                .tenants
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            tenants
                .entry(key.clone())
                .or_insert_with(|| Arc::new(TenantStateEntry::new(spec)))
                .clone()
        };
        if entry.spec != requested_spec {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Tenant key `{:?}` was requested with conflicting specifications",
                key
            ));
        }

        entry
            .get_or_try_init(|| async {
                let state = create().await?;
                let actual_key = TenantKey::from_tenant_paths(&state.paths);
                if actual_key != key {
                    return Err(buck2_error!(
                        buck2_error::ErrorTag::Input,
                        "Constructed tenant key `{:?}` did not match requested key `{:?}`",
                        actual_key,
                        key
                    ));
                }
                Ok(state)
            })
            .await
    }

    fn initial_repo(&self) -> Arc<RepoState> {
        self.initial_state.dupe()
    }

    fn legacy_repo(&self) -> Option<Arc<RepoState>> {
        let tenants = self
            .tenants
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        debug_assert!(
            tenants
                .get(&self.initial_tenant)
                .is_some_and(|entry| entry.is_initialized())
        );
        let initialized_tenant_count = tenants
            .values()
            .filter(|entry| entry.is_initialized())
            .count();
        (initialized_tenant_count == 1).then(|| self.initial_state.dupe())
    }
}

fn tenant_paths_from_client_context(
    client_context: &ClientContext,
) -> buck2_error::Result<Option<TenantPaths>> {
    let Some(identity) = &client_context.tenant_identity else {
        return Ok(None);
    };

    let project_root = AbsNormPathBuf::try_from(identity.project_root.clone())
        .buck_error_context("Invalid tenant project root in client context")?;
    let isolation = FileNameBuf::try_from(identity.isolation.clone())
        .buck_error_context("Invalid tenant isolation in client context")?;
    Ok(Some(TenantPaths::new(
        ProjectRoot::new_unchecked(project_root),
        isolation,
    )))
}

/// DaemonStateData is the main shared data across all commands and repos. It's lazily initialized
/// on the first command that requires it.
#[derive(Allocative)]
pub struct DaemonStateData {
    tenants: TenantStateRegistry,
    repo_state_factory: RepoStateFactory,

    /// Daemon-wide scheduling resources for repo-scoped blocking executors.
    pub blocking_executor_factory: Arc<BlockingExecutorFactory>,

    pub(crate) forkserver: ForkserverAccess,

    #[allocative(skip)]
    pub scribe_sink: Option<Arc<dyn EventSinkWithStats>>,

    pub start_time: Instant,

    /// Http client used for materializer and RunAction implementations.
    pub http_client: HttpClient,

    /// Spawner
    pub spawner: Arc<BuckSpawner>,

    /// Tracks memory usage. Used to make scheduling decisions.
    #[allocative(skip)]
    pub memory_tracker: Option<MemoryTrackerHandle>,

    /// A unique identifier for this instance of the daemon
    pub daemon_id: DaemonId,

    /// Cgroup path of the process that launched this daemon before Buck moved the daemon into its
    /// managed cgroup.
    pub daemon_originating_cgroup: Option<String>,

    /// Semaphores for running actions locally. These need to be shared across commands.
    #[allocative(skip)]
    pub named_semaphores_for_run_actions: Arc<NamedSemaphores>,

    /// Running more than one automatic idle page-out during this daemon's lifetime.
    pub(crate) allow_multiple_idle_page_outs: bool,
}

impl DaemonStateData {
    fn repo_shared_services(&self) -> DaemonSharedServices<'_> {
        DaemonSharedServices {
            blocking_executor_factory: &self.blocking_executor_factory,
            scribe_sink: self.scribe_sink.as_ref(),
            http_client: &self.http_client,
            memory_tracker: self.memory_tracker.as_ref(),
            daemon_id: &self.daemon_id,
        }
    }

    /// Select or initialize the repository addressed by a client command.
    pub async fn repo_for_client_context(
        &self,
        client_context: &ClientContext,
    ) -> buck2_error::Result<Arc<RepoState>> {
        let Some(paths) = tenant_paths_from_client_context(client_context)? else {
            return self.repo_for_legacy_client();
        };
        let spec = TenantSpec::from_tenant_paths(&paths);

        self.tenants
            .get_or_create(spec, || {
                self.repo_state_factory
                    .create(paths, self.repo_shared_services())
            })
            .await
    }

    /// Select a repository for an RPC added before requests carried a client context.
    pub async fn repo_for_optional_client_context(
        &self,
        client_context: Option<&ClientContext>,
    ) -> buck2_error::Result<Arc<RepoState>> {
        match client_context {
            Some(client_context) => self.repo_for_client_context(client_context).await,
            None => self.repo_for_legacy_client(),
        }
    }

    fn repo_for_legacy_client(&self) -> buck2_error::Result<Arc<RepoState>> {
        self.tenants.legacy_repo().ok_or_else(|| {
            buck2_error!(
                ErrorTag::Input,
                "Client did not provide a tenant identity after the daemon began serving multiple tenants"
            )
        })
    }

    /// The initial repo for daemon-scoped operations whose protocol has no tenant identity.
    pub fn initial_repo(&self) -> Arc<RepoState> {
        self.tenants.initial_repo()
    }

    pub fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> buck2_error::Result<()> {
        crate::daemon::dice_dump::dice_dump(
            self.initial_repo().dice_manager.unsafe_dice(),
            path,
            format,
        )
    }
}

impl DaemonStatePanicDiceDump for DaemonStateData {
    fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> buck2_error::Result<()> {
        self.dice_dump(path, format)
    }
}

impl DaemonState {
    pub(crate) async fn new(
        fb: fbinit::FacebookInit,
        paths: InvocationPaths,
        init_ctx: BuckdServerInitPreferences,
        rt: &Handle,
        final_artifact_materialization: FinalArtifactMaterialization,
        working_directory: WorkingDirectory,
        cgroup_tree: Option<BuckCgroupTree>,
        daemon_id: DaemonId,
    ) -> Result<Self, buck2_error::Error> {
        let data = Self::init_data(
            fb,
            paths,
            init_ctx,
            rt,
            final_artifact_materialization,
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
        final_artifact_materialization: FinalArtifactMaterialization,
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
        // Owned, because repo construction happens in the spawned initialization future.
        let repo_state_rt = rt.clone();
        let init_fut = async move {
            let invocation_paths = paths;
            let paths = invocation_paths.tenant_paths();
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
            tracing::info!("Initializing scribe sink...");
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

            let blocking_executor_factory = Arc::new(BlockingExecutorFactory::create()?);

            let http_client = http_client_from_startup_config(&init_ctx.daemon_startup_config)
                .await
                .buck_error_context("Error creating HTTP client")?
                .build();

            tracing::info!("Creating memory tracker...");
            let memory_tracker = memory_tracker::create_memory_tracker(
                cgroup_tree,
                &init_ctx.daemon_startup_config.resource_control,
                &daemon_id,
            )
            .await?;

            // The forkserver creates its state directory recursively, so it does not depend on
            // the materializer creating buck-out first. One daemon-global forkserver serves every
            // repo; requests carry an absolute cwd.
            tracing::info!("Launching forkserver...");
            let forkserver = maybe_launch_forkserver(
                root_config,
                &invocation_paths.forkserver_state_dir(),
                memory_tracker.as_ref().map(|m| &m.cgroup_tree),
                &invocation_paths.isolation,
            )
            .await?;

            let (init_ctx, daemon_originating_cgroup) = init_ctx.split();
            let repo_state_factory = RepoStateFactory {
                fb,
                init_ctx,
                final_artifact_materialization,
                runtime: repo_state_rt,
            };
            let repo = repo_state_factory
                .create_with_loaded_config(
                    paths,
                    &legacy_cells,
                    root_config,
                    DaemonSharedServices {
                        blocking_executor_factory: &blocking_executor_factory,
                        scribe_sink: scribe_sink.as_ref(),
                        http_client: &http_client,
                        memory_tracker: memory_tracker.as_ref(),
                        daemon_id: &daemon_id,
                    },
                )
                .await?;

            let allow_multiple_idle_page_outs = repo_state_factory
                .init_ctx
                .daemon_startup_config
                .hydration
                .as_ref()
                .is_some_and(|h| h.allow_multiple_idle_page_outs);
            let tenants = TenantStateRegistry::new(repo).await?;
            Ok(Arc::new(DaemonStateData {
                tenants,
                repo_state_factory,
                blocking_executor_factory,
                forkserver,
                scribe_sink,
                start_time: std::time::Instant::now(),
                http_client,
                spawner: Arc::new(BuckSpawner::new(daemon_state_data_rt)),
                memory_tracker,
                daemon_id: daemon_id.dupe(),
                daemon_originating_cgroup,
                named_semaphores_for_run_actions: Arc::new(NamedSemaphores::new()),
                allow_multiple_idle_page_outs,
            }))
        };
        let daemon_listener_span = tracing::Span::current();
        rt.spawn(init_fut.instrument(daemon_listener_span)).await?
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
        repo: Arc<RepoState>,
        dispatcher: EventDispatcher,
        drop_guard: ActiveCommandDropGuard,
    ) -> buck2_error::Result<BaseServerCommandContext> {
        let data = self.data();

        dispatcher.instant_event(buck2_data::RestartConfiguration {
            enable_restarter: repo.restart_daemon_on_error,
        });

        tag_result!(
            "eden_not_connected",
            check_working_dir::check_working_dir(),
            quiet: true,
            daemon_in_memory_state_is_corrupted: true,
            task: false
        )?;

        self.validate_cwd()
            .buck_error_context("Error validating working directory")?;

        self.validate_buck_out_mount(&repo)
            .buck_error_context("Error validating buck-out mount")?;

        dispatcher.instant_event(buck2_data::TagEvent {
            tags: repo.tags.clone(),
        });

        // Sync any FS changes and invalidate DICE state if necessary.  Get the Eden
        // version of the underlying system in parallel if available, and fail fast if the
        // Eden daemon restarted underneath us (which leaves cached state and file handles
        // stale and would otherwise surface as a silent hang).
        let verify_eden_identity = async {
            if repo.detect_eden_restart {
                repo.io.verify_eden_identity().await
            } else {
                Ok(())
            }
        };
        let (_, eden_version, ()) = futures::future::try_join3(
            repo.io.settle(),
            repo.io.eden_version(),
            verify_eden_identity,
        )
        .await?;

        dispatcher.instant_event(buck2_data::IoProviderInfo { eden_version });

        Ok(BaseServerCommandContext {
            _fb: self.fb,
            events: dispatcher,
            repo,
            daemon: data.dupe(),
            _drop_guard: drop_guard,
        })
    }

    pub fn data(&self) -> Arc<DaemonStateData> {
        self.data.dupe()
    }

    pub fn validate_cwd(&self) -> buck2_error::Result<()> {
        let res = self.working_directory.is_stale().and_then(|stale| {
            if stale {
                Err(buck2_error!(
                    buck2_error::ErrorTag::DaemonStaleWorkingDir,
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
            res,
            quiet: true,
            daemon_in_memory_state_is_corrupted: true,
            task: false
        )?;

        Ok(())
    }

    pub fn validate_buck_out_mount(&self, repo: &RepoState) -> buck2_error::Result<()> {
        #[cfg(fbcode_build)]
        {
            use buck2_core::soft_error;
            use buck2_fs::error::IoResultExt;
            use buck2_fs::fs_util;

            let project_root = repo.paths.project_root().root();
            if !detect_eden::is_eden(project_root.to_path_buf())? {
                return Ok(());
            }

            let buck_out_root = project_root.join(TenantPaths::buck_out_dir_prefix());

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
                ),
                quiet:false
            )?;
        }
        #[cfg(not(fbcode_build))]
        {
            let _repo = repo;
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
                DigestAlgorithm::Blake3Keyed
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
    #[cfg(fbcode_build)]
    builder
        .with_internal_proxy_from_env(&config.http.proxy_env_allowlist)
        .await?;
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
        Timeout::Default | Timeout::NoTimeout => {}
    }

    Ok(builder)
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::AtomicUsize;
    use std::sync::atomic::Ordering;

    use buck2_cli_proto::TenantIdentity;
    use buck2_common::legacy_configs::configs::testing::parse;
    use buck2_common::settings::BuckSettings;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_fs::paths::file_name::FileNameBuf;
    use indoc::indoc;

    use super::*;

    fn tenant_spec() -> TenantSpec {
        let project_root = if cfg!(windows) {
            "C:\\project"
        } else {
            "/project"
        };
        TenantSpec::from_tenant_paths(&TenantPaths::new(
            ProjectRoot::new_unchecked(
                AbsNormPathBuf::try_from(project_root.to_owned())
                    .expect("test project root should be absolute and normalized"),
            ),
            FileNameBuf::try_from("v2".to_owned()).expect("test isolation should be a file name"),
        ))
    }

    #[tokio::test]
    async fn tenant_entry_initializes_once() -> buck2_error::Result<()> {
        let entry = TenantStateEntry::<usize>::new(tenant_spec());
        let init_count = AtomicUsize::new(0);
        let first = entry.get_or_try_init(|| async {
            init_count.fetch_add(1, Ordering::Relaxed);
            tokio::task::yield_now().await;
            Ok(Arc::new(1))
        });
        let second = entry.get_or_try_init(|| async {
            init_count.fetch_add(1, Ordering::Relaxed);
            Ok(Arc::new(2))
        });

        let (first, second) = tokio::join!(first, second);
        let first = first?;
        let second = second?;
        assert!(Arc::ptr_eq(&first, &second));
        assert_eq!(init_count.load(Ordering::Relaxed), 1);
        Ok(())
    }

    #[tokio::test]
    async fn tenant_entry_retries_failed_initialization() -> buck2_error::Result<()> {
        let entry = TenantStateEntry::<usize>::new(tenant_spec());
        let first = entry
            .get_or_try_init(|| async {
                Err(buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "injected initialization failure"
                ))
            })
            .await;
        assert!(first.is_err());
        assert!(!entry.is_initialized());

        let state = entry.get_or_try_init(|| async { Ok(Arc::new(1)) }).await?;
        assert_eq!(*state, 1);
        assert!(entry.is_initialized());
        Ok(())
    }

    #[test]
    fn tenant_paths_from_client_identity() -> buck2_error::Result<()> {
        let project_root = if cfg!(windows) {
            "C:\\project"
        } else {
            "/project"
        };
        let client_context = ClientContext {
            tenant_identity: Some(TenantIdentity {
                project_root: project_root.to_owned(),
                isolation: "v2".to_owned(),
            }),
            ..Default::default()
        };

        let paths = tenant_paths_from_client_context(&client_context)?
            .expect("the client supplied a tenant identity");
        assert_eq!(paths.project_root().to_string(), project_root);
        assert_eq!(paths.isolation().as_str(), "v2");
        Ok(())
    }

    #[test]
    fn missing_client_identity_uses_compatibility_fallback() -> buck2_error::Result<()> {
        assert!(tenant_paths_from_client_context(&ClientContext::default())?.is_none());
        Ok(())
    }

    #[test]
    fn invalid_client_project_root_is_rejected() {
        let client_context = ClientContext {
            tenant_identity: Some(TenantIdentity {
                project_root: "relative".to_owned(),
                isolation: "v2".to_owned(),
            }),
            ..Default::default()
        };

        assert!(tenant_paths_from_client_context(&client_context).is_err());
    }

    #[test]
    fn invalid_client_isolation_is_rejected() {
        let project_root = if cfg!(windows) {
            "C:\\project"
        } else {
            "/project"
        };
        let client_context = ClientContext {
            tenant_identity: Some(TenantIdentity {
                project_root: project_root.to_owned(),
                isolation: "nested/isolation".to_owned(),
            }),
            ..Default::default()
        };

        assert!(tenant_paths_from_client_context(&client_context).is_err());
    }

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
        let startup_config = DaemonStartupConfig::new(&config, &BuckSettings::empty(), false)?;
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
        let startup_config = DaemonStartupConfig::new(&config, &BuckSettings::empty(), false)?;
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
