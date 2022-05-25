/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::{HashMap, HashSet},
    io::{self, BufWriter, Write},
    marker::PhantomData,
    path::{Path, PathBuf},
    pin::Pin,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
    task::{Context, Poll},
    time::{Duration, Instant, SystemTime},
};

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_build_api::{
    actions::{
        build_listener::{self, BuildSignalSender, SetBuildSignals},
        run::knobs::{HasRunActionKnobs, RunActionKnobs},
    },
    bxl::starlark_defs::configure_bxl_file_globals,
    context::SetBuildContextData,
    execute::{
        blocking::{BlockingExecutor, BuckBlockingExecutor, SetBlockingExecutor},
        commands::{
            dice_data::{set_fallback_executor_config, SetCommandExecutor},
            re::{
                client::RemoteExecutionStaticMetadata,
                manager::{ReConnectionHandle, ReConnectionManager, ReConnectionObserver},
                ReExecutorGlobalKnobs,
            },
        },
        materializer::{
            deferred::DeferredMaterializer, immediate::ImmediateMaterializer,
            MaterializationMethod, Materializer, SetMaterializer,
        },
        ActionExecutorConfig, LocalExecutorOptions,
    },
    interpreter::context::{
        configure_build_file_globals, configure_extension_file_globals, fbcode_prelude,
        BuildInterpreterConfiguror,
    },
};
use buck2_common::{
    dice::{cells::HasCellResolver, data::SetIoProvider},
    file_ops::IgnoreSet,
    io::IoProvider,
    legacy_configs::{dice::HasLegacyConfigs, BuckConfigBasedCells, LegacyBuckConfig},
    memory,
    truncate::truncate,
};
use buck2_core::{
    async_once_cell::AsyncOnceCell,
    cells::CellName,
    env_helper::EnvHelper,
    facebook_only,
    fs::{
        paths::{AbsPath, AbsPathBuf, ForwardRelativePathBuf},
        project::{ProjectFilesystem, ProjectRelativePathBuf},
    },
    result::{SharedResult, ToSharedResultExt},
};
use buck2_data::*;
use buck2_interpreter::{
    dice::interpreter_setup::{setup_interpreter, setup_interpreter_basic},
    extra::InterpreterHostPlatform,
    pattern::ProvidersPattern,
    starlark_profiler::{StarlarkProfilerImpl, StarlarkProfilerInstrumentation},
};
use cli_proto::{
    client_context::HostPlatformOverride, common_build_options::ExecutionStrategy,
    daemon_api_server::*, profile_request::Profiler, *,
};
use dice::{
    cycles::DetectCycles, data::DiceData, Dice, DiceEvent, DiceTracker, DiceTransaction,
    UserComputationData,
};
use events::{dispatch::EventDispatcher, ControlEvent, Event, EventSource, TraceId};
use futures::{
    channel::mpsc::{self, UnboundedReceiver, UnboundedSender},
    future::{AbortHandle, Abortable},
    Future, Stream, StreamExt,
};
use gazebo::{dupe::Dupe, prelude::*};
use host_sharing::{HostSharingBroker, HostSharingStrategy};
use more_futures::{drop::DropTogether, spawn::spawn_dropcancel};
use once_cell::sync::Lazy;
use starlark::eval::ProfileMode;
use thiserror::Error;
use tokio::io::{AsyncRead, AsyncWrite};
use tonic::{
    transport::{server::Connected, Server},
    Code, Request, Response, Status,
};
use tracing::debug_span;

use crate::{
    configs::parse_legacy_cells,
    daemon::{
        build::build,
        bxl::bxl,
        clean::clean,
        common::{
            get_executor_config_for_strategy, parse_concurrency, parse_patterns_from_cli_args,
            CommandExecutorFactory, ToProtoDuration,
        },
        dice_dump::dice_dump_spawn,
        install::install,
        materialize::materialize,
        profile::generate_profile,
        server::file_watcher::FileWatcher,
        test::test,
        uquery::uquery,
    },
    metadata,
    paths::Paths,
};

mod file_watcher;
mod snapshot;

// TODO(cjhopman): Figure out a reasonable value for this.
static DEFAULT_KILL_TIMEOUT: Duration = Duration::from_millis(500);

#[derive(Debug, Error)]
enum DaemonCommunicationError {
    #[error("Got invalid working directory `{0}`")]
    InvalidWorkingDirectory(String),
}

pub trait BuckdServerDelegate: Send + Sync {
    fn force_shutdown(&self) -> anyhow::Result<()>;

    fn force_shutdown_with_timeout(&self, timeout: Duration);
}

/// For a buckd process there is a single DaemonState created at startup and never destroyed.

pub(crate) struct DaemonState {
    fb: fbinit::FacebookInit,

    paths: Paths,

    /// This holds the main data shared across different commands.
    data: AsyncOnceCell<SharedResult<Arc<DaemonStateData>>>,

    /// Whether to detect cycles in Dice
    detect_cycles: DetectCycles,
}

/// DaemonStateData is the main shared data across all commands. It's lazily initialized on
/// the first command that requires it.
pub struct DaemonStateData {
    /// The Dice computation graph. Generally, we shouldn't add things to the DaemonStateData
    /// (or DaemonState) itself and instead they should be represented on the computation graph.
    dice: Arc<Dice>,

    /// Synced every time we run a command.
    file_watcher: Box<dyn FileWatcher>,

    /// Settled every time we run a command.
    io: Arc<dyn IoProvider>,

    /// The RE connection, managed such that all build commands that are concurrently active uses
    /// the same connection. Once there are no active build commands, the connection will be
    /// terminated
    re_client_manager: Arc<ReConnectionManager>,

    /// Executor responsible for coordinating and rate limiting I/O.
    blocking_executor: Arc<dyn BlockingExecutor>,

    /// Most materializations go through the materializer, providing a single point
    /// where the most expensive network and fs IO operations are performed. It
    /// needs access to the `ReConnectionManager` to download from RE. It must
    /// live for the entire lifetime of the daemon, in order to allow deferred
    /// materializations to work properly between distinct build commands.
    materializer: Arc<dyn Materializer>,

    /// Data pertaining to event logging, which controls the ways that event data is written throughout the course of
    /// a command.
    #[cfg_attr(not(fbcode_build), allow(dead_code))]
    event_logging_data: Arc<EventLoggingData>,
}

impl DaemonStateData {
    pub fn dice(&self) -> Arc<Dice> {
        self.dice.dupe()
    }
}

/// Configuration pertaining to event logging.
#[cfg_attr(not(fbcode_build), allow(dead_code))]
pub struct EventLoggingData {
    /// The size of the queue for in-flight messages.
    buffer_size: usize,
}

pub static ACTIVE_COMMANDS: Lazy<Mutex<HashSet<TraceId>>> =
    Lazy::new(|| Mutex::new(HashSet::new()));

struct ActiveCommandDropGuard {
    trace_id: TraceId,
}

impl ActiveCommandDropGuard {
    fn new(trace_id: TraceId) -> Self {
        ACTIVE_COMMANDS.lock().unwrap().insert(trace_id.dupe());
        Self { trace_id }
    }
}

impl Drop for ActiveCommandDropGuard {
    fn drop(&mut self) {
        ACTIVE_COMMANDS.lock().unwrap().remove(&self.trace_id);
    }
}

/// The BuckDiceTracker keeps track of the started/finished events for a dice computation and periodically sends a snapshot to the client.
///
/// There are too many events coming out of dice for us to forward them all to the client, so we need to aggregate
/// them in some way in the daemon.
///
/// The tracker will send a snapshot event every 500ms (only if there have been changes since the last snapshot).
///
/// A client won't necessarily get a final snapshot before a command returns.
struct BuckDiceTracker {
    event_forwarder: UnboundedSender<DiceEvent>,
}

const DICE_SNAPSHOT_INTERVAL: Duration = Duration::from_millis(500);

impl BuckDiceTracker {
    fn new(events: EventDispatcher) -> Self {
        let (event_forwarder, receiver) = mpsc::unbounded();

        std::thread::spawn(move || {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            runtime.block_on(Self::run_task(events, receiver))
        });

        Self { event_forwarder }
    }

    async fn run_task(events: EventDispatcher, mut receiver: UnboundedReceiver<DiceEvent>) {
        let mut needs_update = false;
        let mut states = HashMap::new();
        let mut interval = tokio::time::interval(DICE_SNAPSHOT_INTERVAL);
        interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
        // This will loop until the sender side of the channel is dropped.
        loop {
            tokio::select! {
                ev = receiver.next() => {
                    needs_update = true;
                    match ev {
                        Some(DiceEvent::Started{key_type}) => {
                            states.entry(key_type).or_insert_with(DiceKeyState::default).started += 1;
                        }
                        Some(DiceEvent::Finished{key_type}) => {
                            states.entry(key_type).or_insert_with(DiceKeyState::default).finished += 1;
                        }
                        None => {
                            // This indicates that the sender side has been dropped and we can exit.
                            break;
                        }
                    }
                }
                _ = interval.tick() => {
                    if needs_update {
                        needs_update = false;
                        events.instant_event(DiceComputationStateSnapshot {
                            key_states: states
                                .iter()
                                .map(|(k, v)| ((*k).to_owned(), v.clone()))
                                .collect(),
                        });
                    }
                }
            }
        }
    }
}

impl DiceTracker for BuckDiceTracker {
    fn event(&self, event: DiceEvent) {
        let _ = self.event_forwarder.unbounded_send(event);
    }
}

/// BaseCommandContext provides access to the global daemon state and information specific to a command (like the
/// EventDispatcher). Most commands use a ServerCommandContext which has more command/client-specific information.
pub struct BaseCommandContext {
    /// An fbinit token for using things that require fbinit. fbinit is initialized on daemon startup.
    pub fb: fbinit::FacebookInit,
    /// Absolute path to the project root.
    pub project_root: AbsPathBuf,
    /// A reference to the dice graph. Most interesting things are accessible from this (and new interesting things should be
    /// added there rather than as fields here). This has some per-request setup done already (like attaching a per-request
    /// event dispatcher).
    dice: Arc<Dice>,
    /// A reference to the I/O provider.
    io: Arc<dyn IoProvider>,
    /// The RE connection, managed such that all build commands that are concurrently active uses
    /// the same connection.
    pub re_client_manager: Arc<ReConnectionManager>,
    /// Executor responsible for coordinating and rate limiting I/O.
    pub blocking_executor: Arc<dyn BlockingExecutor>,
    /// Object responsible for handling most materializations.
    pub materializer: Arc<dyn Materializer>,
    /// The event dispatcher for this command context.
    pub events: EventDispatcher,
    /// Event logging configuration for this command context.
    pub event_config: Arc<EventLoggingData>,
    /// Removes this command from the set of active commands when dropped.
    _drop_guard: ActiveCommandDropGuard,
}

impl BaseCommandContext {
    pub fn file_system(&self) -> ProjectFilesystem {
        ProjectFilesystem::new(self.project_root.clone())
    }

    /// Provides a DiceComputations. This may be missing some data or injected keys that
    /// we normally expect. To get a full dice context, use a ServerCommandContext.
    pub fn unsafe_dice_ctx(&self) -> DiceTransaction {
        self.unsafe_dice_ctx_with_more_data(|v| v)
    }

    pub(crate) fn dice(&self) -> &Arc<Dice> {
        &self.dice
    }

    fn unsafe_dice_ctx_with_more_data<F: FnOnce(UserComputationData) -> UserComputationData>(
        &self,
        func: F,
    ) -> DiceTransaction {
        let dice_data = {
            let mut data = DiceData::new();
            data.set(self.events.dupe());

            // For commands that don't set a fallback executor config, set a local one.
            set_fallback_executor_config(
                &mut data,
                ActionExecutorConfig::Local(LocalExecutorOptions {}),
            );

            let data = UserComputationData {
                data,
                tracker: Arc::new(BuckDiceTracker::new(self.events.dupe())),
                ..Default::default()
            };
            func(data)
        };
        self.dice.with_ctx_data(dice_data)
    }
}

/// ServerCommandContext provides access to the global daemon state and information about the calling client for
/// the implementation of DaemonApi endpoints (ex. targets, query, build).
pub struct ServerCommandContext {
    base_context: BaseCommandContext,

    /// The working directory of the client. This is used for resolving things in the request in a
    /// working-dir relative way. For example, it's common to resolve target patterns relative to
    /// the working directory and resolving cell aliases there. This should generally only be used
    /// to interpret values that are in the request. We should convert to client-agnostic things early.
    pub working_dir: ProjectRelativePathBuf,

    /// The oncall specified by the client, if any. This gets injected into request metadata.
    pub oncall: Option<String>,

    config_overrides: Vec<ConfigOverride>,

    host_platform_override: HostPlatformOverride,

    // This ensures that there's only one RE connection during the lifetime of this context. It's possible
    // that we give out other handles, but we don't depend on the lifetimes of those for this guarantee. We
    // also use this to send a RemoteExecutionSessionCreated if the connection is made.
    _re_connection_handle: ReConnectionHandle,

    /// A sender for build signals. This field is exposed to the rest of the command via DICE.
    build_signals: BuildSignalSender,

    /// Starlark profiler instrumentation requested throughout the duration of this command. Usually associated with
    /// the `buck2 profile` command.
    pub starlark_profiler_instrumentation_override: Option<StarlarkProfilerInstrumentation>,

    record_target_call_stacks: bool,
    disable_starlark_types: bool,

    /// Common build options associated with this command.
    build_options: Option<CommonBuildOptions>,

    /// The DiceTransaction to use when servicing computations triggered by this command.
    dice: AsyncOnceCell<SharedResult<DiceTransaction>>,
}

impl ServerCommandContext {
    pub fn new(
        base_context: BaseCommandContext,
        client_context: &ClientContext,
        build_signals: BuildSignalSender,
        starlark_profiler_instrumentation_override: Option<StarlarkProfilerInstrumentation>,
        build_options: Option<&CommonBuildOptions>,
        record_target_call_stacks: bool,
    ) -> anyhow::Result<Self> {
        let abs_path = AbsPath::new(&client_context.working_dir)?;

        let project_path = abs_path
            .strip_prefix(&base_context.project_root)
            .map_err(|_| {
                Into::<anyhow::Error>::into(DaemonCommunicationError::InvalidWorkingDirectory(
                    client_context.working_dir.clone(),
                ))
            })?;

        struct Observer {
            events: EventDispatcher,
        }

        impl ReConnectionObserver for Observer {
            fn session_created(&self, session_id: &str) {
                self.events
                    .instant_event(buck2_data::RemoteExecutionSessionCreated {
                        session_id: session_id.to_owned(),
                    })
            }
        }

        let mut re_connection_handle = base_context.re_client_manager.get_re_connection();

        re_connection_handle.set_observer(Arc::new(Observer {
            events: base_context.events.dupe(),
        }));

        let oncall = if client_context.oncall.is_empty() {
            None
        } else {
            Some(client_context.oncall.clone())
        };

        Ok(ServerCommandContext {
            base_context,
            working_dir: project_path.to_buf().into(),
            config_overrides: client_context.config_overrides.clone(),
            host_platform_override: client_context.host_platform(),
            oncall,
            _re_connection_handle: re_connection_handle,
            build_signals,
            starlark_profiler_instrumentation_override,
            build_options: build_options.cloned(),
            dice: AsyncOnceCell::new(),
            record_target_call_stacks,
            disable_starlark_types: client_context.disable_starlark_types,
        })
    }

    /// Provides a DiceComputations. This may be missing some data or injected keys that
    /// we normally expect. To get a full dice context, use a ServerCommandContext.
    pub fn unsafe_dice_ctx(&self) -> DiceTransaction {
        self.base_context.unsafe_dice_ctx()
    }

    /// Provides a DiceTransaction, initialized on first use and shared after initialization.
    pub async fn dice_ctx(&self) -> SharedResult<DiceTransaction> {
        self.dice
            .get_or_init(self.construct_dice_ctx())
            .await
            .dupe()
    }

    async fn construct_dice_ctx(&self) -> SharedResult<DiceTransaction> {
        let concurrency = self.build_options.as_ref().map_or_else(
            || parse_concurrency(0),
            |opts| parse_concurrency(opts.concurrency),
        )?;
        let execution_strategy = self
            .build_options
            .as_ref()
            .map(|opts| opts.execution_strategy)
            .map_or(ExecutionStrategy::LocalOnly, |strategy| {
                ExecutionStrategy::from_i32(strategy).expect("execution strategy should be valid")
            });

        let run_action_knobs = self
            .build_options
            .as_ref()
            .map(|opts| RunActionKnobs {
                eager_dep_files: opts.eager_dep_files,
                hash_all_commands: opts.hash_all_commands,
            })
            .unwrap_or_default();

        let fs = self.file_system();
        let cwd = &self.working_dir;
        // TODO(cjhopman): The CellResolver and the legacy configs shouldn't be leaves on the graph. This should
        // just be setting the config overrides and host platform override as leaves on the graph.
        let configuror =
            get_interpreter_configuror(self.host_platform_override, self.record_target_call_stacks);
        let (cell_resolver, legacy_configs) =
            parse_legacy_cells(self.config_overrides.iter(), &fs.resolve(cwd), &fs)?;

        let re_global_knobs = {
            let root_config = legacy_configs
                .get(cell_resolver.root_cell())
                .context("No config for root cell")?;

            ReExecutorGlobalKnobs {
                always_check_ttls: root_config
                    .parse("buck2", "re_always_check_ttls")?
                    .unwrap_or(true),
            }
        };

        let dice_ctx = self.base_context.unsafe_dice_ctx();
        setup_interpreter(
            &dice_ctx,
            cell_resolver,
            configuror,
            legacy_configs,
            self.starlark_profiler_instrumentation_override.dupe(),
            self.disable_starlark_types,
        );
        dice_ctx.commit();

        let executor_config =
            get_executor_config_for_strategy(execution_strategy, self.host_platform_override);
        let blocking_executor: Arc<_> = self.base_context.blocking_executor.dupe();
        let materializer = self.base_context.materializer.dupe();
        let re_connection = self.get_re_connection();
        let build_signals = self.build_signals.dupe();
        let host_sharing_broker =
            HostSharingBroker::new(HostSharingStrategy::SmallerTasksFirst, concurrency);

        Ok(self
            .base_context
            .unsafe_dice_ctx_with_more_data(move |mut data| {
                set_fallback_executor_config(&mut data.data, executor_config);
                data.set_command_executor(box CommandExecutorFactory::new(
                    re_connection,
                    host_sharing_broker,
                    materializer.dupe(),
                    execution_strategy.into(),
                    re_global_knobs,
                ));
                data.set_blocking_executor(blocking_executor);
                data.set_materializer(materializer);
                data.set_build_signals(build_signals);
                data.set_run_action_knobs(run_action_knobs);
                data
            }))
    }

    pub fn file_system(&self) -> ProjectFilesystem {
        self.base_context.file_system()
    }

    pub(crate) fn project_root(&self) -> &AbsPath {
        &self.base_context.project_root
    }

    pub(crate) fn get_re_connection(&self) -> ReConnectionHandle {
        self.base_context.re_client_manager.get_re_connection()
    }

    pub(crate) fn events(&self) -> &EventDispatcher {
        &self.base_context.events
    }

    pub(crate) fn stdout(&mut self) -> anyhow::Result<RawOuputGuard<'_>> {
        // Buffer until MESSAGE_BUFFER_SIZE bytes get written to save gRPC communication overheads
        Ok(RawOuputGuard {
            _phantom: PhantomData,
            inner: BufWriter::with_capacity(4096, RawOutputWriter::new(self)?),
        })
    }
}

fn get_interpreter_configuror(
    host_platform: HostPlatformOverride,
    record_target_call_stacks: bool,
) -> Arc<BuildInterpreterConfiguror> {
    let linux = InterpreterHostPlatform::Linux;
    let mac = InterpreterHostPlatform::MacOS;
    let windows = InterpreterHostPlatform::Windows;

    let interpreter_platform = match host_platform {
        HostPlatformOverride::Linux => linux,
        HostPlatformOverride::MacOs => mac,
        HostPlatformOverride::Windows => windows,
        HostPlatformOverride::Default => match std::env::consts::OS {
            "linux" => linux,
            "macos" => mac,
            "windows" => windows,
            v => unimplemented!("no support yet for operating system `{}`", v),
        },
    };
    BuildInterpreterConfiguror::new(
        Some(fbcode_prelude()),
        interpreter_platform,
        record_target_call_stacks,
        configure_build_file_globals,
        configure_extension_file_globals,
        configure_bxl_file_globals,
    )
}

impl DaemonState {
    fn new(
        fb: fbinit::FacebookInit,
        paths: Paths,
        detect_cycles: DetectCycles,
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
        detect_cycles: DetectCycles,
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

        let configuror = BuildInterpreterConfiguror::new(
            Some(fbcode_prelude()),
            InterpreterHostPlatform::Linux,
            false,
            configure_build_file_globals,
            configure_extension_file_globals,
            configure_bxl_file_globals,
        );

        let io = buck2_common::io::create_io_provider(
            fb,
            Arc::new(fs),
            legacy_configs.get(cells.root_cell()).ok(),
        )?;

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
                    .join_unnormalized(paths.buck_out_dir())
                    .join_unnormalized(ForwardRelativePathBuf::unchecked_new("re_logs".to_owned()))
                    .to_string(),
            ),
        ));
        let materialization_method =
            MaterializationMethod::try_new_from_config(legacy_configs.get(cells.root_cell()).ok())?;
        let materializer = Self::create_materializer(
            paths.project_root().to_owned(),
            paths.buck_out_dir(),
            re_client_manager.dupe(),
            blocking_executor.dupe(),
            materialization_method,
        )?;

        let buffer_size = root_config
            .parse("buck2", "event_log_buffer_size")?
            .unwrap_or(10000);
        let event_logging_data = Arc::new(EventLoggingData { buffer_size });

        let dice = {
            let mut builder = Dice::builder();
            builder.set_io_provider(io.dupe());
            builder.build(detect_cycles)
        };
        let ctx = dice.ctx();
        ctx.set_buck_out_path(Some(paths.buck_out_dir()));
        setup_interpreter_basic(&ctx, cells.dupe(), configuror, legacy_configs.dupe());
        ctx.commit();

        // TODO(cjhopman): We want to use Expr::True here, but we need to workaround
        // https://github.com/facebook/watchman/issues/911. Adding other filetypes to
        // this list should be safe until we can revert it to Expr::True.

        let file_watcher =
            <dyn FileWatcher>::new(paths, root_config, dice.dupe(), cells.dupe(), ignore_specs)
                .await
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
            event_logging_data,
        }))
    }

    fn create_materializer(
        project_root: AbsPathBuf,
        buck_out_path: ForwardRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        materialization_method: MaterializationMethod,
    ) -> anyhow::Result<Arc<dyn Materializer>> {
        match materialization_method {
            MaterializationMethod::Immediate => Ok(Arc::new(ImmediateMaterializer::new(
                project_root,
                re_client_manager,
                blocking_executor,
            ))),
            MaterializationMethod::Deferred => Ok(Arc::new(DeferredMaterializer::new(
                project_root,
                re_client_manager,
                blocking_executor,
                true,
            ))),
            MaterializationMethod::DeferredSkipFinalArtifacts => {
                Ok(Arc::new(DeferredMaterializer::new(
                    project_root,
                    re_client_manager,
                    blocking_executor,
                    false,
                )))
            }
            MaterializationMethod::Eden => {
                #[cfg(feature = "eden_materializer")]
                {
                    Ok(Arc::new(
                        buck2_build_api::execute::materializer::eden::EdenMaterializer::new(
                            project_root.clone(),
                            re_client_manager.dupe(),
                            blocking_executor,
                            buck2_build_api::execute::materializer::eden_api::EdenBuckOut::new(
                                ProjectRelativePathBuf::from(buck_out_path.clone()),
                                project_root.join_unnormalized(buck_out_path),
                                re_client_manager,
                            )
                            .context("Failed to create EdenFS-based buck-out")?,
                        )
                        .context("Failed to create Eden materializer")?,
                    ))
                }
                #[cfg(not(feature = "eden_materializer"))]
                {
                    let _unused = buck_out_path;
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
    async fn prepare_events(&self) -> SharedResult<(impl EventSource, EventDispatcher)> {
        use events::sink::{
            scribe::{self, ThriftScribeSink},
            tee::TeeSink,
        };

        // The Scribe category to which we'll write buck2 events.
        const BUCK2_EVENTS_CATEGORY: &str = "buck2_events";

        // facebook only: logging events to Scribe.
        facebook_only();
        let (events, sink) = events::create_source_sink_pair();
        let data = self.data().await?;
        let dispatcher = if scribe::is_enabled() {
            EventDispatcher::new(
                TraceId::new(),
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
            EventDispatcher::new(TraceId::new(), sink)
        };
        Ok((events, dispatcher))
    }

    #[cfg(not(fbcode_build))]
    async fn prepare_events(&self) -> SharedResult<(impl EventSource, EventDispatcher)> {
        let (events, sink) = events::create_source_sink_pair();
        Ok((events, EventDispatcher::new(TraceId::new(), sink)))
    }

    /// Prepares a ServerCommandContext for processing a complex command (that accesses the dice computation graph, for example).
    ///
    /// This initializes (if necessary) the shared daemon state and syncs the watchman query (to flush any recent filesystem events).
    async fn prepare_command(
        &self,
        dispatcher: EventDispatcher,
    ) -> SharedResult<BaseCommandContext> {
        check_working_dir()?;

        let data = self.data().await?;

        // Sync any FS changes and invalidate DICE state if necessary.
        data.file_watcher.sync(&dispatcher).await?;
        data.io.settle().await?;

        let drop_guard = ActiveCommandDropGuard::new(dispatcher.trace_id().dupe());

        Ok(BaseCommandContext {
            fb: self.fb,
            project_root: self.paths.project_root().to_owned(),
            dice: data.dice.dupe(),
            io: data.io.dupe(),
            re_client_manager: data.re_client_manager.dupe(),
            blocking_executor: data.blocking_executor.dupe(),
            materializer: data.materializer.dupe(),
            events: dispatcher,
            event_config: data.event_logging_data.dupe(),
            _drop_guard: drop_guard,
        })
    }

    /// Initializes and returns the DaemonStateData, if it hasn't already been initialized already.
    async fn data(&self) -> SharedResult<Arc<DaemonStateData>> {
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

/// Verify that our working directory is still here. We often run on Eden, and if Eden restarts
/// ungracefully, our working dir will become unreadable and we are just about done.
fn check_working_dir() -> anyhow::Result<()> {
    use std::fs;

    let err = match fs::metadata(".") {
        Ok(..) => return Ok(()),
        Err(e) => e,
    };

    if err.kind() == io::ErrorKind::NotConnected {
        let err = "Buck2 is running in an Eden mount but Eden restarted uncleanly. \
            This error is unrecoverable and you should restart Buck using `buck2 kill`.";
        return Err(anyhow::anyhow!(err));
    }

    tracing::warn!(
        "Buck2 is unable to read its current working directory: {}. Consider restarting",
        err
    );

    Ok(())
}

struct DaemonShutdown {
    delegate: Box<dyn BuckdServerDelegate>,

    /// This channel is used to trigger a graceful shutdown of the grpc server. After
    /// an item is sent on this channel, the server will start rejecting new requests
    /// and once current requests are finished the server will shutdown.
    shutdown_channel: UnboundedSender<()>,
}

impl DaemonShutdown {
    /// Trigger a graceful server shutdown with a timeout. After the timeout expires, a hard shutdown
    /// will be triggered.
    ///
    /// As we might be processing a `kill()` (or other) request, we cannot wait for the server to actually
    /// shutdown (as it will wait for current requests to finish), so this returns immediately.
    fn start_shutdown(&self, timeout: Option<Duration>) {
        let timeout = timeout.unwrap_or(DEFAULT_KILL_TIMEOUT);

        // Ignore errrors on shutdown_channel as that would mean we've already started shutdown;
        let _ = self.shutdown_channel.unbounded_send(());
        self.delegate.force_shutdown_with_timeout(timeout);
    }
}

/// The BuckdServer implements the DaemonApi.
///
/// Simple endpoints are implemented here and complex things will be implemented in a sibling
/// module taking just a ServerCommandContext.
pub struct BuckdServer {
    /// The flag that is set to true when server is shutting down.
    stop_accepting_requests: AtomicBool,
    process_info: DaemonProcessInfo,
    start_time: prost_types::Timestamp,
    start_instant: Instant,
    daemon_shutdown: Arc<DaemonShutdown>,
    daemon_state: Arc<DaemonState>,
}

impl BuckdServer {
    pub async fn run<I, IO, IE>(
        fb: fbinit::FacebookInit,
        paths: Paths,
        delegate: Box<dyn BuckdServerDelegate>,
        detect_cycles: DetectCycles,
        process_info: DaemonProcessInfo,
        listener: I,
    ) -> anyhow::Result<()>
    where
        I: Stream<Item = Result<IO, IE>>,
        IO: AsyncRead + AsyncWrite + Connected + Unpin + Send + 'static,
        IE: Into<Box<dyn std::error::Error + Send + Sync>> + Send,
    {
        let now = SystemTime::now();
        let now = now.duration_since(SystemTime::UNIX_EPOCH)?;

        let (shutdown_channel, mut receiver): (UnboundedSender<()>, _) = mpsc::unbounded();

        let api_server = Self {
            stop_accepting_requests: AtomicBool::new(false),
            process_info,
            start_time: prost_types::Timestamp {
                seconds: now.as_secs() as i64,
                nanos: now.subsec_nanos() as i32,
            },
            start_instant: Instant::now(),
            daemon_shutdown: Arc::new(DaemonShutdown {
                delegate,
                shutdown_channel,
            }),
            daemon_state: Arc::new(DaemonState::new(fb, paths, detect_cycles)?),
        };

        let server = Server::builder()
            .add_service(DaemonApiServer::new(api_server))
            .serve_with_incoming_shutdown(listener, async move {
                receiver.next().await;
            });

        server.await?;

        Ok(())
    }

    /// Runs a single command (given by the func F). Prior to running the command, calls the
    /// `opts`'s `pre_run` hook.  then bootstraps an event source and command context so that the
    /// invoked function has the ability to stream events to the caller.
    async fn run_streaming<Req, Res, Fut, F>(
        &self,
        req: Request<Req>,
        opts: impl StreamingCommandOptions<Req>,
        func: F,
    ) -> Result<Response<ResponseStream>, Status>
    where
        F: FnOnce(ServerCommandContext, Req) -> Fut + Send + 'static,
        Fut: Future<Output = anyhow::Result<Res>> + Send,
        Req: HasClientContext + HasBuildOptions + HasRecordTargetCallStacks + Send + Sync + 'static,
        Res: Into<command_result::Result> + Send + 'static,
    {
        OneshotCommandOptions::pre_run(&opts, self)?;

        let daemon_state = self.daemon_state.dupe();

        let (events, dispatch) = daemon_state
            .prepare_events()
            .await
            .map_err(|e| Status::new(Code::Internal, format!("{:?}", e)))?;

        streaming(req, events, move |req| async move {
            let result: CommandResult = {
                let result: anyhow::Result<CommandResult> = try {
                    let data = daemon_state.data().await?;
                    let _heartbeat = HeartbeatGuard::new(dispatch.dupe(), data);
                    let base_context = daemon_state.prepare_command(dispatch.dupe()).await?;
                    build_listener::scope(base_context.events.dupe(), |build_sender| async {
                        let context = ServerCommandContext::new(
                            base_context,
                            req.client_context()?,
                            build_sender,
                            opts.starlark_profiler_instrumentation_override(&req)?,
                            req.build_options(),
                            req.record_target_call_stacks(),
                        )?;

                        let result = match func(context, req).await {
                            Ok(res) => CommandResult {
                                result: Some(res.into()),
                            },
                            Err(e) => error_to_command_result(e),
                        };

                        Ok(result)
                    })
                    .await?
                };

                match result {
                    Ok(result) => result,
                    Err(e) => error_to_command_result(e),
                }
            };
            dispatch.control_event(ControlEvent::CommandResult(result));
        })
        .await
    }

    async fn oneshot<
        Req,
        Res: Into<command_result::Result>,
        Fut: Future<Output = anyhow::Result<Res>> + Send,
        F: FnOnce(Req) -> Fut,
    >(
        &self,
        req: Request<Req>,
        opts: impl OneshotCommandOptions,
        func: F,
    ) -> Result<Response<CommandResult>, Status> {
        opts.pre_run(self)?;

        let req = req.into_inner();
        match func(req).await {
            Ok(val) => Ok(Response::new(CommandResult {
                result: Some(val.into()),
            })),
            Err(e) => Ok(Response::new(error_to_command_result(e))),
        }
    }

    /// Checks if the server is accepting requests.
    fn check_if_accepting_requests(&self) -> Result<(), Status> {
        if self.stop_accepting_requests.load(Ordering::Relaxed) {
            Err(Status::failed_precondition(
                "Failed to run command, `buckd` is shutting down soon!",
            ))
        } else {
            Ok(())
        }
    }
}

// Spawns a thread to occasionally output snapshots of resource utilization.
struct HeartbeatGuard {
    abort_handle: AbortHandle,
}

impl HeartbeatGuard {
    fn new(events: EventDispatcher, daemon_data: Arc<DaemonStateData>) -> Self {
        let (abort_handle, abort_registration) = AbortHandle::new_pair();
        let _heartbeat_handle = tokio::spawn(Abortable::new(
            async move {
                let mut interval = tokio::time::interval(Duration::from_secs(1));
                interval.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Delay);
                loop {
                    interval.tick().await;

                    let snapshot = snapshot::create_snapshot(&daemon_data);

                    events.event(buck2_data::InstantEvent {
                        data: Some(snapshot.into()),
                    });
                }
            },
            abort_registration,
        ));
        Self { abort_handle }
    }
}

impl Drop for HeartbeatGuard {
    fn drop(&mut self) {
        self.abort_handle.abort();
    }
}

fn convert_positive_duration(proto_duration: &prost_types::Duration) -> Result<Duration, Status> {
    if proto_duration.seconds < 0 || proto_duration.nanos < 0 {
        return Err(Status::new(
            Code::Unknown,
            format!("received invalid timeout: `{:?}`", proto_duration),
        ));
    }
    Ok(Duration::from_secs(proto_duration.seconds as u64)
        + Duration::from_nanos(proto_duration.nanos as u64))
}

fn error_to_command_result(e: anyhow::Error) -> CommandResult {
    let messages = vec![format!("{:?}", e)];

    CommandResult {
        result: Some(command_result::Result::Error(CommandError { messages })),
    }
}

/// tonic requires the response for a streaming api to be a Sync Stream. With async/await, that requirement is really difficult
/// to meet. This simple wrapper allows us to wrap a non-Sync stream into a Sync one (the inner stream is never accessed in a
/// non-exclusive manner).
struct SyncStream<T: Stream<Item = Result<CommandProgress, Status>> + Send> {
    // SyncWrapper provides a Sync type that only allows (statically checked) exclusive access to
    // the underlying object, this allows using a non-Sync object where a Sync one is required
    // but is never accessed from multiple threads.
    // See https://internals.rust-lang.org/t/what-shall-sync-mean-across-an-await/12020/31
    // and https://github.com/hyperium/tonic/issues/117
    wrapped: sync_wrapper::SyncWrapper<T>,
}

impl<T: Stream<Item = Result<CommandProgress, Status>> + Send> Stream for SyncStream<T> {
    type Item = <T as Stream>::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        // This is a safe pin projection. See https://doc.rust-lang.org/std/pin/index.html#projections-and-structural-pinning
        // Specifically see the requirements when pinning is structural for a field here: https://doc.rust-lang.org/std/pin/index.html#pinning-is-structural-for-field
        unsafe { self.map_unchecked_mut(|a| a.wrapped.get_mut()) }.poll_next(cx)
    }
}

pub struct RawOuputGuard<'a> {
    _phantom: PhantomData<&'a mut ServerCommandContext>,
    inner: BufWriter<RawOutputWriter>,
}

/// A writer that fires InstantEvent (RawOutput) when `write` function is called.
/// Client is supposed to print the message to its stdout immediately as verbatim.
struct RawOutputWriter {
    dispatcher: EventDispatcher,
    /// Maximum bytes of a message that is delivered to cli per `write` call
    chunk_size: usize,
}

impl<'a> Write for RawOuputGuard<'a> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.inner.write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.inner.flush()
    }
}

impl<'a> Drop for RawOuputGuard<'a> {
    fn drop(&mut self) {
        // This would only happen if we had output that isn't utf-8 and got flushed. For now we live with ignoring
        // this.
        if let Err(e) = self.inner.flush() {
            tracing::error!("Discarded RawOutputWriter output: {:#}", e);
        }
    }
}

impl RawOutputWriter {
    pub fn new(context: &ServerCommandContext) -> anyhow::Result<Self> {
        Ok(Self {
            dispatcher: context.base_context.events.dupe(),
            chunk_size: RawOutputWriter::get_chunk_size()?,
        })
    }

    fn get_chunk_size() -> anyhow::Result<usize> {
        // protobuf recommends each message should be under 1MB
        const DEFAULT_CHUNK_SIZE: usize = 1024 * 1024;
        static CHUNK_SIZE: EnvHelper<usize> = EnvHelper::new("BUCK2_DEBUG_RAWOUTPUT_CHUNK_SIZE");
        Ok(CHUNK_SIZE.get()?.unwrap_or(DEFAULT_CHUNK_SIZE))
    }
}

impl Write for RawOutputWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let len = std::cmp::min(buf.len(), self.chunk_size);
        if len > 0 {
            let raw_output = buck2_data::RawOutput {
                raw_output: String::from_utf8(buf[..len].to_vec()).map_err(|_| {
                    io::Error::new(io::ErrorKind::InvalidInput, "Output is not utf-8")
                })?,
            };
            self.dispatcher.instant_event(raw_output);
        }
        Ok(len)
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

/// Dispatches a request to the given function and returns a stream of responses, suitable for streaming to a client.
#[allow(clippy::mut_mut)] // select! does this internally
async fn streaming<
    Req: Send + Sync + 'static,
    Fut: Future<Output = ()> + Send + 'static,
    F: FnOnce(Req) -> Fut,
    E: EventSource + 'static,
>(
    req: Request<Req>,
    mut events: E,
    func: F,
) -> Result<Response<ResponseStream>, Status>
where
    F: Send + 'static,
    E: Sync,
{
    // This function is responsible for receiving all events coming into an EventSource and reacting accordingly. There
    // are two categories events that can be seen:
    // 1. Control events, which are not to be sent across the gRPC boundary but instruct this function to do something.
    // 2. Buck events, which are to be sent across the gRPC boundary.
    //
    // The function `func` is the computation that we are going to run. It communicates its success or failure using
    // control events; our first step is to spawn it.
    let req = req.into_inner();
    let cancellable = spawn_dropcancel(func(req), debug_span!(parent: None, "running-command",));
    let (output_send, output_recv) = tokio::sync::mpsc::unbounded_channel();

    // We run the event consumer on a totally separate tokio runtime to avoid the consumer task from getting stuck behind
    // another tokio task in its lifo task slot. See T96012305 and https://github.com/tokio-rs/tokio/issues/4323 for more
    // information.
    let _merge_task = std::thread::spawn(move || {
        while let Some(next_event) = events.receive() {
            // Note that writes to `output_send` have their errors explicitly ignored here. There is only one reason
            // for a write to a `mpsc::channel` to fail: the receiving end of the channel has already been closed.
            //
            // This function returns the receiving channel back to `tonic` as part of a streaming response. Tonic can
            // drop the stream before it is fully resolved if, for example, the gRPC client disconnects during the
            // command. In this case, we explicitly ignore write errors and let them float off into the void, since no
            // client is listening.
            //
            // TODO(swgillespie) - We should handle client disconnects better.
            match next_event {
                Event::Control(control_event) => {
                    // A control event. This event isn't going to be sent to gRPC, but we do need to react to it. In
                    // this case, the CommandResult event indicates that the spawned computation has produced a result
                    // and will not be producing any more events.
                    match control_event {
                        ControlEvent::CommandResult(result) => {
                            let _ignore = output_send.send(Ok(CommandProgress {
                                progress: Some(command_progress::Progress::Result(result)),
                            }));
                        }
                    }
                    return;
                }
                Event::Buck(buck_event) => {
                    // A buck event. These events should be forwarded directly to gRPC.
                    let _ignore = output_send.send(Ok(CommandProgress {
                        progress: Some(command_progress::Progress::Event(buck_event.into())),
                    }));
                }
            }
        }
    });

    // The stream we ultimately return is the receiving end of the channel that the above task is writing to.
    Ok(Response::new(Box::pin(SyncStream {
        wrapped: sync_wrapper::SyncWrapper::new(DropTogether::new(
            tokio_stream::wrappers::UnboundedReceiverStream::new(output_recv),
            cancellable,
        )),
    })))
}

#[cfg(all(unix, not(fbcode_build)))]
fn jemalloc_stats(response: &mut StatusResponse) {
    use jemalloc_ctl::{epoch, stats};

    fn set<T>(to: &mut u64, from: Result<usize, T>) {
        if let Ok(from) = from {
            *to = from as u64;
        }
    }

    // Many statistics are cached and only updated when the epoch is advanced.
    if epoch::advance().is_err() {
        return;
    }
    set(&mut response.bytes_allocated, stats::allocated::read());
    set(&mut response.bytes_resident, stats::resident::read());
    set(&mut response.bytes_retained, stats::retained::read());
}

#[cfg(not(all(unix, not(fbcode_build))))]
fn jemalloc_stats(_response: &mut StatusResponse) {}

type ResponseStream = Pin<Box<dyn Stream<Item = Result<CommandProgress, Status>> + Send + Sync>>;
#[async_trait]
impl DaemonApi for BuckdServer {
    async fn kill(&self, req: Request<KillRequest>) -> Result<Response<CommandResult>, Status> {
        struct KillRunCommandOptions;

        impl OneshotCommandOptions for KillRunCommandOptions {
            /// kill should be always available
            fn pre_run(&self, _server: &BuckdServer) -> Result<(), Status> {
                Ok(())
            }
        }

        self.oneshot(req, KillRunCommandOptions, move |req| async move {
            self.stop_accepting_requests.store(true, Ordering::Relaxed);

            let timeout = req
                .timeout
                .as_ref()
                .map(convert_positive_duration)
                .transpose()?;

            self.daemon_shutdown.start_shutdown(timeout);
            Ok(KillResponse {})
        })
        .await
    }

    async fn ping(&self, req: Request<PingRequest>) -> Result<Response<CommandResult>, Status> {
        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            match &req.delay {
                Some(delay) => {
                    let delay = convert_positive_duration(delay)?;
                    tokio::time::sleep(delay).await;
                }
                _ => {}
            }

            Ok(PingResponse {})
        })
        .await
    }

    async fn status(&self, req: Request<StatusRequest>) -> Result<Response<CommandResult>, Status> {
        let daemon_state = self.daemon_state.dupe();

        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            let snapshot = if req.snapshot {
                let data = daemon_state.data().await?;
                Some(snapshot::create_snapshot(&data))
            } else {
                None
            };

            let uptime = self.start_instant.elapsed();
            let mut base = StatusResponse {
                process_info: Some(self.process_info.clone()),
                start_time: Some(self.start_time.clone()),
                uptime: Some(uptime.to_proto()),
                snapshot,
                ..Default::default()
            };
            jemalloc_stats(&mut base);
            Ok(base)
        })
        .await
    }

    async fn flush_dep_files(
        &self,
        req: Request<FlushDepFilesRequest>,
    ) -> Result<Response<CommandResult>, Status> {
        self.oneshot(req, DefaultCommandOptions, move |req| async move {
            let FlushDepFilesRequest {} = req;
            buck2_build_api::actions::run::dep_files::flush_dep_files();
            Ok(GenericResponse {})
        })
        .await
    }

    type CleanStream = ResponseStream;
    async fn clean(&self, req: Request<CleanRequest>) -> Result<Response<ResponseStream>, Status> {
        struct ShutdownDropGuard {
            daemon_shutdown: Arc<DaemonShutdown>,
        }

        impl Drop for ShutdownDropGuard {
            fn drop(&mut self) {
                self.daemon_shutdown.start_shutdown(None);
            }
        }

        let shut_down_after = !req.get_ref().dry_run;

        struct CleanRunCommandOptions {
            shut_down_after: bool,
        }

        impl OneshotCommandOptions for CleanRunCommandOptions {
            fn pre_run(&self, server: &BuckdServer) -> Result<(), Status> {
                server.check_if_accepting_requests()?;
                if self.shut_down_after {
                    // NOTE: we don't do reject requests via start_shutdown() because that would
                    // cause us to stop listening on our socket, which is not ideal for callers as
                    // it means they can't tell *why* the server is not accepting requests.
                    server
                        .stop_accepting_requests
                        .store(true, Ordering::Relaxed);
                }
                Ok(())
            }
        }

        impl StreamingCommandOptions<CleanRequest> for CleanRunCommandOptions {}

        let drop_guard = if shut_down_after {
            Some(ShutdownDropGuard {
                daemon_shutdown: self.daemon_shutdown.dupe(),
            })
        } else {
            None
        };

        self.run_streaming(
            req,
            CleanRunCommandOptions { shut_down_after },
            move |context, req| async move {
                let metadata = request_metadata(&context).await;
                let start_event = buck2_data::CommandStart {
                    metadata: metadata.clone(),
                    data: Some(buck2_data::CleanCommandStart {}.into()),
                };
                let events = context.base_context.events.dupe();
                let res = events
                    .span_async(start_event, async {
                        let result = clean(context, req).await;
                        let (is_success, error_messages) = match &result {
                            Ok(_e) => (true, vec![]),
                            Err(e) => (false, vec![format!("{:#}", e)]),
                        };
                        let end_event = buck2_data::CommandEnd {
                            metadata,
                            data: Some(buck2_data::CleanCommandEnd {}.into()),
                            is_success,
                            error_messages,
                        };

                        (result, end_event)
                    })
                    .await;

                // Ensure that if all goes well, the drop guard lives until this point.
                drop(drop_guard);

                res
            },
        )
        .await
    }

    type BuildStream = ResponseStream;
    async fn build(&self, req: Request<BuildRequest>) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let project_root = context.base_context.project_root.to_string();
            let metadata = request_metadata(&context).await;
            let patterns_for_logging =
                canonicalize_patterns_for_logging(&context, &req.target_patterns).await?;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::BuildCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            let result = events
                .span_async(start_event, async {
                    let result = build(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(response) => (
                            response.error_messages.is_empty(),
                            response.error_messages.clone(),
                        ),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata,
                        data: Some(
                            buck2_data::BuildCommandEnd {
                                target_patterns: patterns_for_logging,
                            }
                            .into(),
                        ),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;

            Ok(BuildResponse {
                build_targets: result.build_targets,
                project_root,
                serialized_build_report: result.serialized_build_report.unwrap_or_default(),
                error_messages: result.error_messages,
            })
        })
        .await
    }

    type BxlStream = ResponseStream;
    async fn bxl(&self, req: Request<BxlRequest>) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let project_root = context.base_context.project_root.to_string();
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::BxlCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            let result = events
                .span_async(start_event, async {
                    let result = bxl(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(response) => (
                            response.error_messages.is_empty(),
                            response.error_messages.clone(),
                        ),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata,
                        data: Some(buck2_data::BxlCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;

            Ok(BxlResponse {
                build_targets: result.built,
                project_root,
                serialized_build_report: result.serialized_build_report.unwrap_or_default(),
                error_messages: result.error_messages,
            })
        })
        .await
    }

    type TestStream = ResponseStream;
    async fn test(&self, req: Request<TestRequest>) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let events = context.base_context.events.dupe();
            let patterns_for_logging =
                canonicalize_patterns_for_logging(&context, &req.target_patterns).await?;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::TestCommandStart {}.into()),
            };
            let test_response = events
                .span_async(start_event, async {
                    let result = test(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(response) => (
                            response.error_messages.is_empty(),
                            response.error_messages.clone(),
                        ),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(
                            buck2_data::TestCommandEnd {
                                target_patterns: patterns_for_logging,
                            }
                            .into(),
                        ),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;
            Ok(test_response)
        })
        .await
    }

    type AqueryStream = ResponseStream;
    async fn aquery(
        &self,
        req: Request<AqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::AqueryCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            events
                .span_async(start_event, async {
                    let result = crate::daemon::aquery::aquery(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(response) => (
                            response.error_messages.is_empty(),
                            response.error_messages.clone(),
                        ),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::AqueryCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await
        })
        .await
    }

    type UqueryStream = ResponseStream;
    async fn uquery(
        &self,
        req: Request<UqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::QueryCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            events
                .span_async(start_event, async {
                    let result = uquery(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(response) => (
                            response.error_messages.is_empty(),
                            response.error_messages.clone(),
                        ),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::QueryCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await
        })
        .await
    }

    type CqueryStream = ResponseStream;
    async fn cquery(
        &self,
        req: Request<CqueryRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(
                    buck2_data::CQueryCommandStart {
                        query: req.query.clone(),
                        query_args: truncate(&req.query_args.join(","), 1000),
                        target_universe: truncate(&req.target_universe.join(","), 1000),
                    }
                    .into(),
                ),
            };
            let events = context.base_context.events.dupe();
            events
                .span_async(start_event, async {
                    let result = crate::daemon::cquery::cquery(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(response) => (
                            response.error_messages.is_empty(),
                            response.error_messages.clone(),
                        ),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::CQueryCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await
        })
        .await
    }

    type TargetsStream = ResponseStream;
    async fn targets(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::TargetsCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            let response = events
                .span_async(start_event, async {
                    let result = crate::daemon::targets::targets(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(_e) => (true, vec![]),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::TargetsCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;
            Ok(response)
        })
        .await
    }

    type TargetsShowOutputsStream = ResponseStream;
    async fn targets_show_outputs(
        &self,
        req: Request<TargetsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::TargetsCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            let response = events
                .span_async(start_event, async {
                    let result =
                        crate::daemon::targets_show_outputs::targets_show_outputs(context, req)
                            .await;
                    let (is_success, error_messages) = match &result {
                        Ok(_e) => (true, vec![]),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::TargetsCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;
            Ok(response)
        })
        .await
    }

    type AuditStream = ResponseStream;
    async fn audit(
        &self,
        req: Request<GenericRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let req = req; // capture req into async block
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::AuditCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            let command: crate::commands::audit::AuditCommand =
                serde_json::from_str(&req.serialized_opts)?;
            let dir = context.working_dir.as_str().to_owned();
            // TODO pass in log setting thru ClientContext instead of env var (see D29824148)

            static LOG_REPRODUCE: EnvHelper<bool> = EnvHelper::new("LOG_REPRODUCE");
            if LOG_REPRODUCE.get()?.unwrap_or(false) {
                events.instant_hg().await;
            }

            events
                .span_async(start_event, async {
                    let result = command
                        .server_execute(
                            context,
                            req.context.expect("buck cli always sets a client context"),
                        )
                        .await;
                    let (status, error_messages) = match &result {
                        Ok(_e) => (0, vec![]),
                        Err(e) => (1, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(
                            buck2_data::AuditCommandEnd {
                                status,
                                args: req.serialized_opts.to_owned(),
                                dir,
                            }
                            .into(),
                        ),
                        is_success: status == 0,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;

            Ok(GenericResponse {})
        })
        .await
    }

    type InstallStream = ResponseStream;
    async fn install(
        &self,
        req: Request<InstallRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let patterns_for_logging =
                canonicalize_patterns_for_logging(&context, &req.target_patterns).await?;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::InstallCommandStart {}.into()),
            };
            let events = context.base_context.events.dupe();
            events
                .span_async(start_event, async {
                    let result = install(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(_e) => (true, vec![]),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata,
                        data: Some(
                            buck2_data::InstallCommandEnd {
                                target_patterns: patterns_for_logging,
                            }
                            .into(),
                        ),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await
        })
        .await
    }

    async fn unstable_crash(
        &self,
        _req: Request<UnstableCrashRequest>,
    ) -> Result<Response<UnstableCrashResponse>, Status> {
        panic!("explicitly requested panic (via unstable_crash)");
    }

    async fn segfault(
        &self,
        _req: Request<SegfaultRequest>,
    ) -> Result<Response<SegfaultResponse>, Status> {
        unsafe {
            std::ptr::null_mut::<&'static str>()
                .write("Explicitly requested segfault (via `segfault`)")
        };
        unreachable!()
    }

    async fn unstable_heap_dump(
        &self,
        req: Request<UnstableHeapDumpRequest>,
    ) -> Result<Response<UnstableHeapDumpResponse>, Status> {
        self.check_if_accepting_requests()?;

        let heap_dump = memory::write_heap_to_file(&req.into_inner().destination_path);
        match heap_dump {
            Ok(_) => Ok(Response::new(UnstableHeapDumpResponse {})),
            Err(e) => Err(Status::invalid_argument(format!(
                "failed to perform heap dump: {}",
                e
            ))),
        }
    }

    async fn unstable_allocator_stats(
        &self,
        req: Request<UnstableAllocatorStatsRequest>,
    ) -> Result<Response<UnstableAllocatorStatsResponse>, Status> {
        self.check_if_accepting_requests()?;

        let response = memory::allocator_stats(&req.into_inner().options)
            .context("Failed to retrieve allocator stats");

        match response {
            Ok(response) => Ok(Response::new(UnstableAllocatorStatsResponse { response })),
            Err(e) => Err(Status::invalid_argument(format!("{:#}", e))),
        }
    }

    async fn unstable_dice_dump(
        &self,
        req: Request<UnstableDiceDumpRequest>,
    ) -> Result<Response<UnstableDiceDumpResponse>, Status> {
        self.check_if_accepting_requests()?;

        let inner = req.into_inner();
        let path = inner.destination_path;
        let res: anyhow::Result<_> = try {
            let (_, dispatch) = self.daemon_state.prepare_events().await?;
            let ctx = self.daemon_state.prepare_command(dispatch.dupe()).await?;

            let path = Path::new(&path);
            let format_proto =
                cli_proto::unstable_dice_dump_request::DiceDumpFormat::from_i32(inner.format)
                    .context("Invalid DICE dump format")?;

            dice_dump_spawn(ctx, path, format_proto)
                .await
                .with_context(|| format!("Failed to perform dice dump to {}", path.display()))?;

            UnstableDiceDumpResponse {}
        };

        res.map(Response::new)
            .map_err(|e| Status::internal(format!("{:#}", e)))
    }

    type UnstableDocsStream = ResponseStream;
    async fn unstable_docs(
        &self,
        req: Request<UnstableDocsRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::DocsCommandStart {}.into()),
            };
            let events = context.events().dupe();
            let result = events
                .span_async(start_event, async {
                    let result = crate::daemon::docs::docs(context, req).await;
                    let (is_success, error_messages) = match &result {
                        Ok(_e) => (true, vec![]),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::DocsCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;
            Ok(result)
        })
        .await
    }

    type Profile2Stream = ResponseStream;
    async fn profile2(
        &self,
        req: Request<ProfileRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        struct ProfileCommandOptions;

        impl OneshotCommandOptions for ProfileCommandOptions {}

        impl StreamingCommandOptions<ProfileRequest> for ProfileCommandOptions {
            fn starlark_profiler_instrumentation_override(
                &self,
                req: &ProfileRequest,
            ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>> {
                let profiler_proto = cli_proto::profile_request::Profiler::from_i32(req.profiler)
                    .context("Invalid profiler")?;

                let profile_mode = match profiler_proto {
                    Profiler::HeapFlame => ProfileMode::HeapFlame,
                    Profiler::HeapSummary => ProfileMode::HeapSummary,
                    Profiler::TimeFlame => ProfileMode::TimeFlame,
                    Profiler::Statement => ProfileMode::Statement,
                    Profiler::Bytecode => ProfileMode::Bytecode,
                    Profiler::BytecodePairs => ProfileMode::BytecodePairs,
                    Profiler::Typecheck => ProfileMode::Typecheck,
                };

                Ok(Some(StarlarkProfilerInstrumentation::new(Some(
                    profile_mode,
                ))))
            }
        }

        self.run_streaming(req, ProfileCommandOptions, move |context, req| async move {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::ProfileCommandStart {}.into()),
            };
            let events = context.events().dupe();
            let result = events
                .span_async(start_event, async {
                    let result: anyhow::Result<_> = try {
                        let output: PathBuf = req.destination_path.clone().into();

                        let profile_mode = context
                            .starlark_profiler_instrumentation_override
                            .as_ref()
                            .and_then(|profiler| profiler.dupe().into_profile_mode())
                            .context("Missing profile mode")?;

                        let mut profiler = StarlarkProfilerImpl::new(profile_mode, output);

                        generate_profile(
                            context,
                            req.context.context("Missing client context")?,
                            req.target_pattern.context("Missing target pattern")?,
                            cli_proto::profile_request::Action::from_i32(req.action)
                                .context("Invalid action")?,
                            &mut profiler,
                        )
                        .await?;

                        ProfileResponse {
                            elapsed: Some(profiler.elapsed()?.into()),
                            total_allocated_bytes: profiler.total_allocated_bytes()? as u64,
                        }
                    };

                    let (is_success, error_messages) = match &result {
                        Ok(_e) => (true, vec![]),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };

                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::ProfileCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await?;

            Ok(result)
        })
        .await
    }

    type MaterializeStream = ResponseStream;
    async fn materialize(
        &self,
        req: Request<MaterializeRequest>,
    ) -> Result<Response<ResponseStream>, Status> {
        self.run_streaming(req, DefaultCommandOptions, |context, req| async {
            let metadata = request_metadata(&context).await;
            let start_event = buck2_data::CommandStart {
                metadata: metadata.clone(),
                data: Some(buck2_data::MaterializeCommandStart {}.into()),
            };
            let events = context.events().dupe();
            events
                .span_async(start_event, async {
                    let result = materialize(context.base_context, req.paths)
                        .await
                        .map(|()| MaterializeResponse {})
                        .context("Failed to materialize paths");
                    let (is_success, error_messages) = match &result {
                        Ok(_e) => (true, vec![]),
                        Err(e) => (false, vec![format!("{:#}", e)]),
                    };
                    let end_event = buck2_data::CommandEnd {
                        metadata: metadata.clone(),
                        data: Some(buck2_data::MaterializeCommandEnd {}.into()),
                        is_success,
                        error_messages,
                    };

                    (result, end_event)
                })
                .await
        })
        .await
    }
}

/// Gathers metadata to attach to events for when a command starts and stops.
async fn request_metadata(ctx: &ServerCommandContext) -> HashMap<String, String> {
    // Facebook only: metadata collection for Scribe writes
    facebook_only();

    fn add_config(
        map: &mut HashMap<String, String>,
        cfg: &LegacyBuckConfig,
        section: &'static str,
        key: &'static str,
        field_name: &'static str,
    ) {
        if let Some(value) = cfg.get(section, key) {
            map.insert(field_name.to_owned(), value.to_owned());
        }
    }

    fn extract_scuba_defaults(
        config: Option<&LegacyBuckConfig>,
    ) -> Option<serde_json::Map<String, serde_json::Value>> {
        let config = config?.get("scuba", "defaults")?;
        let unescaped_config = shlex::split(config)?.join("");
        let sample_json: serde_json::Value = serde_json::from_str(&unescaped_config).ok()?;
        sample_json.get("normals")?.as_object().cloned()
    }

    let mut metadata = metadata::collect();
    // In the case of invalid configuration (e.g. something like buck2 build -c X), `dice_ctx_default` returns an
    // error. We won't be able to get configs to log in that case, but we shouldn't crash.
    if let Ok(dice_ctx) = ctx.dice_ctx().await {
        let cells = dice_ctx.get_cell_resolver().await;
        let root_cell_config = dice_ctx.get_legacy_config_for_cell(cells.root_cell()).await;
        if let Ok(config) = root_cell_config {
            add_config(&mut metadata, &config, "log", "repository", "repository");

            // Buck1 honors a configuration field, `scuba.defaults`, by drawing values from the configuration value and
            // inserting them verbatim into Scuba samples. Buck2 doesn't write to Scuba in the same way that Buck1
            // does, but metadata in this function indirectly makes its way to Scuba, so it makes sense to respect at
            // least some of the data within it.
            //
            // The configuration field is expected to be the canonical JSON representation for a Scuba sample, which is
            // to say something like this:
            // ```
            // {
            //   "normals": { "key": "value" },
            //   "ints": { "key": 0 },
            // }
            // ```
            //
            // TODO(swgillespie) - This only covers the normals since Buck2's event protocol only allows for string
            // metadata. Depending on what sort of things we're missing by dropping int default columns, we might want
            // to consider adding support to the protocol for integer metadata.

            if let Ok(cwd_cell_name) = cells.find(&ctx.working_dir) {
                let cwd_cell_config = dice_ctx.get_legacy_config_for_cell(cwd_cell_name).await;
                if let Some(normals_obj) = extract_scuba_defaults(cwd_cell_config.as_ref().ok()) {
                    for (key, value) in normals_obj.iter() {
                        if let Some(value) = value.as_str() {
                            metadata.insert(key.clone(), value.to_owned());
                        }
                    }
                }

                // `client.id` is often set via the `-c` flag; `-c` configuration is assigned to the cwd cell and not
                // the root cell.
                if let Ok(config) = cwd_cell_config {
                    add_config(&mut metadata, &config, "client", "id", "client");
                }
            }
        }
    }

    metadata.insert(
        "io_provider".to_owned(),
        ctx.base_context.io.name().to_owned(),
    );

    if let Some(oncall) = &ctx.oncall {
        metadata.insert("oncall".to_owned(), oncall.clone());
    }

    metadata
}

/// Options to configure the execution of a oneshot command (i.e. what happens in `oneshot()`).
trait OneshotCommandOptions: Send + Sync + 'static {
    fn pre_run(&self, server: &BuckdServer) -> Result<(), Status> {
        server.check_if_accepting_requests()
    }
}

/// Options to configure the execution of a streaming command (i.e. what happens in `run_streaming()`).
trait StreamingCommandOptions<Req>: OneshotCommandOptions {
    fn starlark_profiler_instrumentation_override(
        &self,
        _req: &Req,
    ) -> anyhow::Result<Option<StarlarkProfilerInstrumentation>> {
        Ok(None)
    }
}

/// No-op set of command options.
struct DefaultCommandOptions;

impl OneshotCommandOptions for DefaultCommandOptions {}
impl<Req> StreamingCommandOptions<Req> for DefaultCommandOptions {}

/// The target patterns sent to the Buck2 daemon are ambiguous and require some amount of disambiguation prior to
/// running a command. There are two key things that need to be disambiguated by the Buck2 daemon:
///   1) Target patterns can be relative to the Buck2 client's current working directory, in which case we must
///      canonicalize the path to the root of the nearest cell,
///   2) Target patterns do not require an explicit cell at the root of the path, in which case Buck2 infers the cell
///      based on configuration.
///
/// This function produces a canonicalized list of target patterns from a command-supplied list of target patterns, with
/// all ambiguities resolved. This greatly simplifies logging as we only ever log unambiguous target patterns and do not
/// need to log things like the command's working directory or cell.
async fn canonicalize_patterns_for_logging(
    ctx: &ServerCommandContext,
    patterns: &[buck2_data::TargetPattern],
) -> anyhow::Result<Vec<buck2_data::TargetPattern>> {
    let dice_txn = ctx.dice_ctx().await?;
    let providers_patterns =
        parse_patterns_from_cli_args::<ProvidersPattern>(patterns, &dice_txn, &ctx.working_dir)
            .await?;
    let patterns = providers_patterns.into_map(|pat| buck2_data::TargetPattern {
        value: format!("{}", pat),
    });

    Ok(patterns)
}
