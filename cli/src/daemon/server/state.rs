/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::significant_drop_in_scrutinee)] // FIXME?

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::configure_dice::configure_dice_for_buck;
use buck2_build_api::execute::blocking::BlockingExecutor;
use buck2_build_api::execute::blocking::BuckBlockingExecutor;
use buck2_build_api::execute::commands::re::client::RemoteExecutionStaticMetadata;
use buck2_build_api::execute::commands::re::manager::ReConnectionManager;
use buck2_build_api::execute::materializer::deferred::DeferredMaterializer;
use buck2_build_api::execute::materializer::deferred::DeferredMaterializerConfigs;
use buck2_build_api::execute::materializer::immediate::ImmediateMaterializer;
use buck2_build_api::execute::materializer::MaterializationMethod;
use buck2_build_api::execute::materializer::Materializer;
use buck2_bxl::bxl::calculation::BxlCalculationImpl;
use buck2_common::file_ops::IgnoreSet;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::BuckConfigBasedCells;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::async_once_cell::AsyncOnceCell;
use buck2_core::cells::CellName;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_forkserver::client::ForkserverClient;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use dice::cycles::DetectCycles;
use dice::Dice;
use events::dispatch::EventDispatcher;
use events::EventSource;
use events::TraceId;
use fbinit::FacebookInit;
use gazebo::dupe::Dupe;
use gazebo::variants::VariantName;

use crate::daemon::server;
use crate::daemon::server::ctx::BaseCommandContext;
use crate::daemon::server::file_watcher::FileWatcher;
use crate::daemon::server::ActiveCommandDropGuard;
use crate::Paths;

/// For a buckd process there is a single DaemonState created at startup and never destroyed.
pub(crate) struct DaemonState {
    fb: fbinit::FacebookInit,

    pub(crate) paths: Paths,

    /// This holds the main data shared across different commands.
    data: AsyncOnceCell<SharedResult<Arc<DaemonStateData>>>,

    /// Whether to detect cycles in Dice
    detect_cycles: Option<DetectCycles>,
}

/// DaemonStateData is the main shared data across all commands. It's lazily initialized on
/// the first command that requires it.
pub(crate) struct DaemonStateData {
    /// The Dice computation graph. Generally, we shouldn't add things to the DaemonStateData
    /// (or DaemonState) itself and instead they should be represented on the computation graph.
    dice: Arc<Dice>,

    /// Synced every time we run a command.
    file_watcher: Arc<dyn FileWatcher>,

    /// Settled every time we run a command.
    io: Arc<dyn IoProvider>,

    /// The RE connection, managed such that all build commands that are concurrently active uses
    /// the same connection. Once there are no active build commands, the connection will be
    /// terminated
    pub(crate) re_client_manager: Arc<ReConnectionManager>,

    /// Executor responsible for coordinating and rate limiting I/O.
    pub(crate) blocking_executor: Arc<dyn BlockingExecutor>,

    /// Most materializations go through the materializer, providing a single point
    /// where the most expensive network and fs IO operations are performed. It
    /// needs access to the `ReConnectionManager` to download from RE. It must
    /// live for the entire lifetime of the daemon, in order to allow deferred
    /// materializations to work properly between distinct build commands.
    materializer: Arc<dyn Materializer>,

    forkserver: Option<ForkserverClient>,

    /// Data pertaining to event logging, which controls the ways that event data is written throughout the course of
    /// a command.
    #[cfg_attr(not(fbcode_build), allow(dead_code))]
    event_logging_data: Arc<EventLoggingData>,
}

impl DaemonStateData {
    pub(crate) fn dice_dump(&self, path: &Path, format: DiceDumpFormat) -> anyhow::Result<()> {
        crate::daemon::dice_dump::dice_dump(&self.dice, path, format)
    }

    pub(crate) async fn spawn_dice_dump(
        &self,
        path: &Path,
        format: DiceDumpFormat,
    ) -> anyhow::Result<()> {
        crate::daemon::dice_dump::dice_dump_spawn(&self.dice, path, format).await
    }
}

/// Configuration pertaining to event logging.
#[cfg_attr(not(fbcode_build), allow(dead_code))]
pub(crate) struct EventLoggingData {
    /// The size of the queue for in-flight messages.
    buffer_size: usize,
}

impl DaemonState {
    pub(crate) fn new(
        fb: fbinit::FacebookInit,
        paths: Paths,
        detect_cycles: Option<DetectCycles>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            fb,
            paths,
            data: AsyncOnceCell::new(),
            detect_cycles,
        })
    }

    // Creates the initial DaemonStateData.
    // Starts up the watchman query.
    async fn init_data(
        fb: fbinit::FacebookInit,
        paths: &Paths,
        detect_cycles: Option<DetectCycles>,
    ) -> anyhow::Result<Arc<DaemonStateData>> {
        let fs = ProjectFilesystem::new(paths.project_root().to_owned());

        let legacy_cells = BuckConfigBasedCells::parse(&fs)?;
        let (legacy_configs, cells) = (legacy_cells.configs_by_name, legacy_cells.cell_resolver);

        let root_config = legacy_configs
            .get(cells.root_cell())
            .context("No config for root cell")?;

        // TODO(rafaelc): merge configs from all cells once they are consistent
        let static_metadata = Arc::new(RemoteExecutionStaticMetadata::from_legacy_config(
            root_config,
        )?);

        let ignore_specs: HashMap<CellName, IgnoreSet> = legacy_configs
            .iter()
            .map(|(cell, config)| {
                Ok((
                    cell.clone(),
                    IgnoreSet::from_ignore_spec(config.get("project", "ignore").unwrap_or(""))?,
                ))
            })
            .collect::<anyhow::Result<_>>()?;

        let io = buck2_common::io::create_io_provider(
            fb,
            Arc::new(fs),
            legacy_configs.get(cells.root_cell()).ok(),
        )
        .await?;

        let blocking_executor = Arc::new(BuckBlockingExecutor::default_concurrency(
            (**io.fs()).clone(),
        )?);
        let re_client_manager = Arc::new(ReConnectionManager::new(
            fb,
            false,
            10,
            static_metadata,
            Some(
                paths
                    .project_root()
                    .join(paths.buck_out_dir())
                    .join(ForwardRelativePathBuf::unchecked_new("re_logs".to_owned()))
                    .to_string(),
            ),
            paths.buck_out_dir().to_string(),
        ));
        let materialization_method =
            MaterializationMethod::try_new_from_config(legacy_configs.get(cells.root_cell()).ok())?;
        let materializer = Self::create_materializer(
            fb,
            (**io.fs()).clone(),
            paths.buck_out_dir(),
            re_client_manager.dupe(),
            blocking_executor.dupe(),
            materialization_method,
            root_config,
        )?;

        let buffer_size = root_config
            .parse("buck2", "event_log_buffer_size")?
            .unwrap_or(10000);
        let event_logging_data = Arc::new(EventLoggingData { buffer_size });

        let dice = configure_dice_for_buck(
            io.dupe(),
            &BxlCalculationImpl,
            Some(root_config),
            detect_cycles,
        )?;

        let forkserver = server::maybe_launch_forkserver(root_config).await?;

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

        // Kick off an initial sync eagerly. This gets Watchamn to start watching the path we care
        // about (potentially kicking off an initial crawl).

        // disable the eager spawn for watchman until we fix dice commit to avoid a panic TODO(bobyf)
        // tokio::task::spawn(watchman_query.sync());
        Ok(Arc::new(DaemonStateData {
            dice,
            file_watcher,
            io,
            re_client_manager,
            blocking_executor,
            materializer,
            forkserver,
            event_logging_data,
        }))
    }

    fn create_materializer(
        fb: FacebookInit,
        fs: ProjectFilesystem,
        buck_out_path: ProjectRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materialization_method: MaterializationMethod,
        root_config: &LegacyBuckConfig,
    ) -> anyhow::Result<Arc<dyn Materializer>> {
        match materialization_method {
            MaterializationMethod::Immediate => Ok(Arc::new(ImmediateMaterializer::new(
                fs,
                re_client_manager,
                blocking_executor,
            ))),
            MaterializationMethod::Deferred | MaterializationMethod::DeferredSkipFinalArtifacts => {
                let config = DeferredMaterializerConfigs {
                    materialize_final_artifacts: matches!(
                        materialization_method,
                        MaterializationMethod::Deferred
                    ),
                    enable_local_caching_of_re_artifacts: root_config
                        .parse("buck2", "enable_local_caching_of_re_artifacts")?
                        .unwrap_or(false),
                    defer_write_actions: root_config
                        .parse("buck2", "defer_write_actions")?
                        .unwrap_or(false),
                };

                Ok(Arc::new(DeferredMaterializer::new(
                    fs,
                    re_client_manager,
                    blocking_executor,
                    config,
                )))
            }
            MaterializationMethod::Eden => {
                #[cfg(all(unix, feature = "eden_materializer"))]
                {
                    use buck2_build_api::execute::materializer::eden::EdenMaterializer;
                    use buck2_build_api::execute::materializer::eden_api::EdenBuckOut;

                    let buck_out_mount = fs.root.join(&buck_out_path);

                    Ok(Arc::new(
                        EdenMaterializer::new(
                            fs,
                            re_client_manager.dupe(),
                            blocking_executor,
                            EdenBuckOut::new(fb, buck_out_path, buck_out_mount, re_client_manager)
                                .context("Failed to create EdenFS-based buck-out")?,
                        )
                        .context("Failed to create Eden materializer")?,
                    ))
                }
                #[cfg(any(not(feature = "eden_materializer"), not(unix)))]
                {
                    let _unused = buck_out_path;
                    let _unused = fs;
                    let _unused = fb;
                    Err(anyhow::anyhow!(
                        "`eden` materialization method is not supported unless you build with `eden_materializer`"
                    ))
                }
            }
        }
    }

    /// Prepares an event stream for a request by bootstrapping an event source and EventDispatcher pair. The given
    /// EventDispatcher will log to the returned EventSource and (optionally) to Scribe if enabled via buckconfig.
    #[cfg(fbcode_build)]
    pub(crate) async fn prepare_events(
        &self,
        trace_id: TraceId,
    ) -> SharedResult<(impl EventSource, EventDispatcher)> {
        use buck2_core::facebook_only;
        use events::sink::scribe;
        use events::sink::scribe::ThriftScribeSink;
        use events::sink::tee::TeeSink;

        // The Scribe category to which we'll write buck2 events.
        const BUCK2_EVENTS_CATEGORY: &str = "buck2_events";

        // facebook only: logging events to Scribe.
        facebook_only();
        let (events, sink) = events::create_source_sink_pair();
        let data = self.data().await?;
        let dispatcher = if scribe::is_enabled() {
            EventDispatcher::new(
                trace_id,
                TeeSink::new(
                    ThriftScribeSink::new(
                        self.fb,
                        BUCK2_EVENTS_CATEGORY.to_owned(),
                        data.event_logging_data.buffer_size,
                    )?,
                    sink,
                ),
            )
        } else {
            // Writing to Scribe via the HTTP gateway (what we do for a Cargo build) is many times slower than the fbcode
            // Scribe client, so we don't do it. It's really, really bad for build performance - turning it on regresses
            // build performance by 10x.
            EventDispatcher::new(trace_id, sink)
        };
        Ok((events, dispatcher))
    }

    #[cfg(not(fbcode_build))]
    pub(crate) async fn prepare_events(
        &self,
        trace_id: TraceId,
    ) -> SharedResult<(impl EventSource, EventDispatcher)> {
        let (events, sink) = events::create_source_sink_pair();
        Ok((events, EventDispatcher::new(trace_id, sink)))
    }

    /// Prepares a ServerCommandContext for processing a complex command (that accesses the dice computation graph, for example).
    ///
    /// This initializes (if necessary) the shared daemon state and syncs the watchman query (to flush any recent filesystem events).
    pub(crate) async fn prepare_command(
        &self,
        dispatcher: EventDispatcher,
    ) -> SharedResult<BaseCommandContext> {
        server::check_working_dir()?;

        let data = self.data().await?;

        let tags = vec![
            format!(
                "dice-detect-cycles:{}",
                data.dice.detect_cycles().variant_name()
            ),
            format!("forkserver:{}", data.forkserver.is_some()),
        ];

        dispatcher.instant_event(buck2_data::TagEvent { tags });

        let drop_guard = ActiveCommandDropGuard::new(dispatcher.trace_id().dupe());

        // Sync any FS changes and invalidate DICE state if necessary.
        data.io.settle().await?;

        Ok(BaseCommandContext {
            _fb: self.fb,
            project_root: self.paths.project_root().to_owned(),
            dice: data.dice.dupe(),
            io: data.io.dupe(),
            re_client_manager: data.re_client_manager.dupe(),
            blocking_executor: data.blocking_executor.dupe(),
            materializer: data.materializer.dupe(),
            file_watcher: data.file_watcher.dupe(),
            events: dispatcher,
            forkserver: data.forkserver.dupe(),
            _event_config: data.event_logging_data.dupe(),
            _drop_guard: drop_guard,
        })
    }

    /// Initializes and returns the DaemonStateData, if it hasn't already been initialized already.
    pub(crate) async fn data(&self) -> SharedResult<Arc<DaemonStateData>> {
        self.data
            .get_or_init(async move {
                let result = Self::init_data(self.fb, &self.paths, self.detect_cycles)
                    .await
                    .context("Error initializing DaemonStateData");
                if let Ok(ref data) = result {
                    crate::daemon::panic::initialize(data.dupe());
                }
                result.shared_error()
            })
            .await
            .dupe()
    }
}
