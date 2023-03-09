/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use buck2_common::cas_digest::DigestAlgorithm;
use buck2_common::ignores::IgnoreSet;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::cells::name::CellName;
use buck2_core::facebook_only;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::sink::scribe;
use buck2_events::sink::tee::TeeSink;
use buck2_events::trace::TraceId;
use buck2_events::EventSink;
use buck2_events::EventSource;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::BuckBlockingExecutor;
use buck2_execute::materialize::materializer::MaterializationMethod;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_execute_impl::materializers::deferred::DeferredMaterializer;
use buck2_execute_impl::materializers::deferred::DeferredMaterializerConfigs;
use buck2_execute_impl::materializers::deferred::TtlRefreshConfiguration;
use buck2_execute_impl::materializers::immediate::ImmediateMaterializer;
use buck2_execute_impl::materializers::sqlite::MaterializerState;
use buck2_execute_impl::materializers::sqlite::MaterializerStateSqliteDb;
use buck2_forkserver::client::ForkserverClient;
use buck2_re_configuration::RemoteExecutionStaticMetadata;
use buck2_re_configuration::RemoteExecutionStaticMetadataImpl;
use buck2_server_ctx::concurrency::ConcurrencyHandler;
use buck2_server_ctx::concurrency::DiceCleanup;
use buck2_server_ctx::concurrency::NestedInvocation;
use buck2_server_ctx::concurrency::ParallelInvocation;
use dice::Dice;
use dupe::Dupe;
use fbinit::FacebookInit;
use gazebo::variants::VariantName;
use tokio::sync::Mutex;

use crate::active_commands::ActiveCommandDropGuard;
use crate::ctx::BaseServerCommandContext;
use crate::daemon::check_working_dir;
use crate::daemon::disk_state::delete_unknown_disk_state;
use crate::daemon::disk_state::maybe_initialize_materializer_sqlite_db;
use crate::daemon::disk_state::DiskStateOptions;
use crate::daemon::forkserver::maybe_launch_forkserver;
use crate::daemon::panic::DaemonStatePanicDiceDump;
use crate::file_watcher::FileWatcher;

/// For a buckd process there is a single DaemonState created at startup and never destroyed.
#[derive(Allocative)]
pub struct DaemonState {
    #[allocative(skip)]
    fb: fbinit::FacebookInit,

    pub paths: InvocationPaths,

    /// This holds the main data shared across different commands.
    data: SharedResult<Arc<DaemonStateData>>,

    dice_constructor: Box<dyn DaemonStateDiceConstructor>,
}

/// DaemonStateData is the main shared data across all commands. It's lazily initialized on
/// the first command that requires it.
#[derive(Allocative)]
pub struct DaemonStateData {
    /// The Dice computation graph. Generally, we shouldn't add things to the DaemonStateData
    /// (or DaemonState) itself and instead they should be represented on the computation graph.
    ///
    /// The DICE graph is held by the concurrency handler to manage locking for concurrent commands
    pub(crate) dice_manager: ConcurrencyHandler,

    /// Synced every time we run a command.
    file_watcher: Arc<dyn FileWatcher>,

    /// Settled every time we run a command.
    io: Arc<dyn IoProvider>,

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

    pub(crate) forkserver: Option<ForkserverClient>,

    #[allocative(skip)]
    pub scribe_sink: Option<Arc<dyn EventSink>>,

    /// Whether or not to hash all commands
    pub hash_all_commands: bool,

    /// What buck2 state to store on disk, ex. materializer state on sqlite
    pub disk_state_options: DiskStateOptions,

    pub start_time: Instant,

    #[allocative(skip)]
    pub create_unhashed_outputs_lock: Arc<Mutex<()>>,
}

impl DaemonStateData {
    pub fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> anyhow::Result<()> {
        crate::daemon::dice_dump::dice_dump(self.dice_manager.unsafe_dice(), path, format)
    }

    pub async fn spawn_dice_dump(&self, path: &Path, format: DiceDumpFormat) -> anyhow::Result<()> {
        crate::daemon::dice_dump::dice_dump_spawn(self.dice_manager.unsafe_dice(), path, format)
            .await
    }
}

impl DaemonStatePanicDiceDump for DaemonStateData {
    fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> anyhow::Result<()> {
        self.dice_dump(path, format)
    }
}

#[async_trait]
pub trait DaemonStateDiceConstructor: Allocative + Send + Sync + 'static {
    async fn construct_dice(
        &self,
        io: Arc<dyn IoProvider>,
        digest_config: DigestConfig,
        root_config: &LegacyBuckConfig,
    ) -> anyhow::Result<Arc<Dice>>;
}

impl DaemonState {
    pub async fn new(
        fb: fbinit::FacebookInit,
        paths: InvocationPaths,
        dice_constructor: Box<dyn DaemonStateDiceConstructor>,
    ) -> Self {
        let data = Self::init_data(fb, &paths, &*dice_constructor)
            .await
            .context("Error initializing DaemonStateData");
        if let Ok(data) = &data {
            crate::daemon::panic::initialize(data.dupe());
        }

        let data = data.shared_error();

        DaemonState {
            fb,
            paths,
            data,
            dice_constructor,
        }
    }

    // Creates the initial DaemonStateData.
    // Starts up the watchman query.
    async fn init_data(
        fb: fbinit::FacebookInit,
        paths: &InvocationPaths,
        dice_constructor: &dyn DaemonStateDiceConstructor,
    ) -> anyhow::Result<Arc<DaemonStateData>> {
        let fs = paths.project_root().clone();

        eprintln!("Reading config...");
        let legacy_cells = BuckConfigBasedCells::parse(&fs)?;

        eprintln!("Starting...");

        let (legacy_configs, cells) = (legacy_cells.configs_by_name, legacy_cells.cell_resolver);

        let root_config = legacy_configs
            .get(cells.root_cell())
            .context("No config for root cell")?;

        let digest_algorithms = root_config
            .parse_list("buck2", "digest_algorithms")?
            .unwrap_or_else(|| {
                if buck2_core::is_open_source() {
                    vec![DigestAlgorithm::Sha256]
                } else {
                    vec![DigestAlgorithm::Sha1]
                }
            });

        let digest_config =
            DigestConfig::leak_new(digest_algorithms).context("Error initializing DigestConfig")?;

        // TODO(rafaelc): merge configs from all cells once they are consistent
        let static_metadata = Arc::new(RemoteExecutionStaticMetadata::from_legacy_config(
            root_config,
        )?);

        let ignore_specs: HashMap<CellName, IgnoreSet> = legacy_configs
            .iter()
            .map(|(cell, config)| {
                Ok((
                    cell,
                    IgnoreSet::from_ignore_spec(config.get("project", "ignore").unwrap_or(""))?,
                ))
            })
            .collect::<anyhow::Result<_>>()?;

        let materialization_method =
            MaterializationMethod::try_new_from_config(legacy_configs.get(cells.root_cell()).ok())?;
        let disk_state_options = DiskStateOptions::new(root_config, materialization_method.dupe())?;
        let blocking_executor = Arc::new(BuckBlockingExecutor::default_concurrency(fs.dupe())?);
        let cache_dir_path = paths.cache_dir_path();
        let valid_cache_dirs = paths.valid_cache_dirs();
        let fs_duped = fs.dupe();

        let deferred_materializer_configs = {
            let defer_write_actions = root_config
                .parse::<RolloutPercentage>("buck2", "defer_write_actions")?
                .unwrap_or_else(RolloutPercentage::never)
                .roll();

            // RE will refresh any TTL < 1 hour, so we check twice an hour and refresh any TTL
            // < 1 hour.
            let ttl_refresh_frequency = root_config
                .parse("buck2", "ttl_refresh_frequency_seconds")?
                .unwrap_or(1800);

            let ttl_refresh_min_ttl = root_config
                .parse("buck2", "ttl_refresh_min_ttl_seconds")?
                .unwrap_or(3600);

            let ttl_refresh_enabled = root_config
                .parse::<RolloutPercentage>("buck2", "ttl_refresh_enabled")?
                .unwrap_or_else(RolloutPercentage::never)
                .roll();

            DeferredMaterializerConfigs {
                materialize_final_artifacts: matches!(
                    materialization_method,
                    MaterializationMethod::Deferred
                ),
                defer_write_actions,
                ttl_refresh: TtlRefreshConfiguration {
                    frequency: std::time::Duration::from_secs(ttl_refresh_frequency),
                    min_ttl: chrono::Duration::seconds(ttl_refresh_min_ttl),
                    enabled: ttl_refresh_enabled,
                },
            }
        };

        let (io, forkserver, _, (materializer_db, materializer_state)) =
            futures::future::try_join4(
                buck2_common::io::create_io_provider(
                    fb,
                    fs.dupe(),
                    legacy_configs.get(cells.root_cell()).ok(),
                    digest_config.cas_digest_config(),
                ),
                maybe_launch_forkserver(root_config),
                (blocking_executor.dupe() as Arc<dyn BlockingExecutor>).execute_io_inline(|| {
                    // Using `execute_io_inline` is just out of convenience.
                    // It doesn't really matter what's used here since there's no IO-heavy
                    // operations on daemon startup
                    delete_unknown_disk_state(&cache_dir_path, &valid_cache_dirs, fs_duped)
                }),
                maybe_initialize_materializer_sqlite_db(
                    &disk_state_options,
                    paths,
                    blocking_executor.dupe() as Arc<dyn BlockingExecutor>,
                    root_config,
                    &deferred_materializer_configs,
                    fs,
                    digest_config,
                ),
            )
            .await?;

        let re_client_manager = Arc::new(ReConnectionManager::new(
            fb,
            false,
            10,
            static_metadata,
            Some(paths.re_logs_dir().to_string()),
            paths.buck_out_dir().to_string(),
        ));
        let materializer = Self::create_materializer(
            fb,
            io.project_root().dupe(),
            digest_config,
            paths.buck_out_dir(),
            re_client_manager.dupe(),
            blocking_executor.dupe(),
            materialization_method,
            deferred_materializer_configs,
            materializer_db,
            materializer_state,
        )?;

        let dice = dice_constructor
            .construct_dice(io.dupe(), digest_config, root_config)
            .await?;

        // TODO(cjhopman): We want to use Expr::True here, but we need to workaround
        // https://github.com/facebook/watchman/issues/911. Adding other filetypes to
        // this list should be safe until we can revert it to Expr::True.

        let file_watcher = <dyn FileWatcher>::new(
            paths.project_root(),
            root_config,
            cells.dupe(),
            ignore_specs,
        )
        .context("Error creating a FileWatcher")?;

        let hash_all_commands = root_config
            .parse::<RolloutPercentage>("buck2", "hash_all_commands")?
            .unwrap_or_else(RolloutPercentage::never)
            .roll();

        let nested_invocation_config = root_config
            .parse::<NestedInvocation>("buck2", "nested_invocation")?
            .unwrap_or(NestedInvocation::Run);

        let parallel_invocation_config = root_config
            .parse::<ParallelInvocation>("buck2", "parallel_invocation")?
            .unwrap_or(ParallelInvocation::Run);

        let cleanup_config = root_config
            .parse::<DiceCleanup>("buck2", "dice_cleanup")?
            .unwrap_or(DiceCleanup::Run);

        let create_unhashed_outputs_lock = Arc::new(Mutex::new(()));

        let buffer_size = root_config
            .parse("buck2", "event_log_buffer_size")?
            .unwrap_or(10000);
        let retry_backoff = Duration::from_millis(
            root_config
                .parse("buck2", "event_log_retry_backoff_duration_ms")?
                .unwrap_or(500),
        );
        let retry_attempts = root_config
            .parse("buck2", "event_log_retry_attempts")?
            .unwrap_or(5);
        let message_batch_size = root_config.parse("buck2", "event_log_message_batch_size")?;
        let scribe_sink = Self::init_scribe_sink(
            fb,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        )
        .context("failed to init scribe sink")?;

        // Kick off an initial sync eagerly. This gets Watchamn to start watching the path we care
        // about (potentially kicking off an initial crawl).

        // disable the eager spawn for watchman until we fix dice commit to avoid a panic TODO(bobyf)
        // tokio::task::spawn(watchman_query.sync());
        Ok(Arc::new(DaemonStateData {
            dice_manager: ConcurrencyHandler::new(
                dice,
                nested_invocation_config,
                parallel_invocation_config,
                cleanup_config,
            ),
            file_watcher,
            io,
            re_client_manager,
            blocking_executor,
            materializer,
            forkserver,
            scribe_sink,
            hash_all_commands,
            disk_state_options,
            start_time: std::time::Instant::now(),
            create_unhashed_outputs_lock,
        }))
    }

    fn create_materializer(
        fb: FacebookInit,
        fs: ProjectRoot,
        digest_config: DigestConfig,
        buck_out_path: ProjectRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materialization_method: MaterializationMethod,
        deferred_materializer_configs: DeferredMaterializerConfigs,
        materializer_db: Option<MaterializerStateSqliteDb>,
        materializer_state: Option<MaterializerState>,
    ) -> anyhow::Result<Arc<dyn Materializer>> {
        match materialization_method {
            MaterializationMethod::Immediate => Ok(Arc::new(ImmediateMaterializer::new(
                fs,
                digest_config,
                re_client_manager,
                blocking_executor,
            ))),
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
                )?))
            }
            MaterializationMethod::Eden => {
                #[cfg(any(fbcode_build, cargo_internal_build))]
                {
                    use buck2_execute::materialize::eden_api::EdenBuckOut;
                    use buck2_execute_impl::materializers::eden::EdenMaterializer;

                    let buck_out_mount = fs.root().join(&buck_out_path);

                    if cfg!(unix) {
                        Ok(Arc::new(
                            EdenMaterializer::new(
                                fs,
                                digest_config,
                                re_client_manager.dupe(),
                                blocking_executor,
                                EdenBuckOut::new(
                                    fb,
                                    buck_out_path,
                                    buck_out_mount,
                                    re_client_manager,
                                )
                                .context("Failed to create EdenFS-based buck-out")?,
                            )
                            .context("Failed to create Eden materializer")?,
                        ))
                    } else {
                        Err(anyhow::anyhow!(
                            "`eden` materialization method is not supported on Windows"
                        ))
                    }
                }
                #[cfg(not(any(fbcode_build, cargo_internal_build)))]
                {
                    let _unused = buck_out_path;
                    let _unused = fs;
                    let _unused = fb;
                    Err(anyhow::anyhow!(
                        "`eden` materialization method is only supported in Meta internal builds"
                    ))
                }
            }
        }
    }

    pub fn init_scribe_sink(
        fb: FacebookInit,
        buffer_size: usize,
        retry_backoff: Duration,
        retry_attempts: usize,
        message_batch_size: Option<usize>,
    ) -> anyhow::Result<Option<Arc<dyn EventSink>>> {
        facebook_only();
        scribe::new_thrift_scribe_sink_if_enabled(
            fb,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        )
        .map(|maybe_scribe| maybe_scribe.map(|scribe| Arc::new(scribe) as _))
    }

    /// Prepares an event stream for a request by bootstrapping an event source and EventDispatcher pair. The given
    /// EventDispatcher will log to the returned EventSource and (optionally) to Scribe if enabled via buckconfig.
    pub async fn prepare_events(
        &self,
        trace_id: TraceId,
    ) -> SharedResult<(impl EventSource, EventDispatcher)> {
        // facebook only: logging events to Scribe.
        facebook_only();
        let (events, sink) = buck2_events::create_source_sink_pair();
        let data = self.data()?;
        let dispatcher = if let Some(scribe_sink) = data.scribe_sink.dupe() {
            EventDispatcher::new(trace_id, TeeSink::new(scribe_sink, sink))
        } else {
            EventDispatcher::new(trace_id, sink)
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
    ) -> SharedResult<BaseServerCommandContext> {
        check_working_dir::check_working_dir()?;

        let data = self.data()?;

        let tags = vec![
            format!(
                "dice-detect-cycles:{}",
                data.dice_manager
                    .unsafe_dice()
                    .detect_cycles()
                    .variant_name()
            ),
            format!("hash-all-commands:{}", data.hash_all_commands),
            format!(
                "sqlite-materializer-state:{}",
                data.disk_state_options.sqlite_materializer_state
            ),
        ];

        dispatcher.instant_event(buck2_data::TagEvent { tags });

        // Sync any FS changes and invalidate DICE state if necessary.
        data.io.settle().await?;

        Ok(BaseServerCommandContext {
            _fb: self.fb,
            project_root: self.paths.project_root().clone(),
            dice_manager: data.dice_manager.dupe(),
            io: data.io.dupe(),
            re_client_manager: data.re_client_manager.dupe(),
            blocking_executor: data.blocking_executor.dupe(),
            materializer: data.materializer.dupe(),
            file_watcher: data.file_watcher.dupe(),
            events: dispatcher,
            forkserver: data.forkserver.dupe(),
            hash_all_commands: data.hash_all_commands,
            _drop_guard: drop_guard,
            daemon_start_time: data.start_time,
            create_unhashed_outputs_lock: data.create_unhashed_outputs_lock.dupe(),
        })
    }

    pub fn data(&self) -> anyhow::Result<Arc<DaemonStateData>> {
        Ok(self.data.dupe()?)
    }
}
