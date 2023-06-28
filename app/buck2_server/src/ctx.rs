/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufWriter;
use std::marker::PhantomData;
use std::ops::Deref;
use std::sync::Arc;
use std::time::Instant;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::actions::impls::run_action_knobs::HasRunActionKnobs;
use buck2_build_api::actions::impls::run_action_knobs::RunActionKnobs;
use buck2_build_api::build::HasCreateUnhashedSymlinkLock;
use buck2_build_api::build_signals::create_build_signals;
use buck2_build_api::build_signals::BuildSignalsInstaller;
use buck2_build_api::build_signals::SetBuildSignals;
use buck2_build_api::context::SetBuildContextData;
use buck2_build_api::keep_going::HasKeepGoing;
use buck2_build_api::spawner::BuckSpawner;
use buck2_build_signals::CriticalPathBackendName;
use buck2_build_signals::HasCriticalPathBackend;
use buck2_cli_proto::client_context::HostArchOverride;
use buck2_cli_proto::client_context::HostPlatformOverride;
use buck2_cli_proto::common_build_options::ExecutionStrategy;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::CommonBuildOptions;
use buck2_cli_proto::ConfigOverride;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::cycles::CycleDetectorAdapter;
use buck2_common::dice::cycles::PairDiceCycleDetector;
use buck2_common::dice::data::HasIoProvider;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::http::counting_client::CountingHttpClient;
use buck2_common::http::SetHttpClient;
use buck2_common::io::trace::TracingIoProvider;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_common::result::SharedError;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_configured::calculation::ConfiguredGraphCycleDescriptor;
use buck2_core::async_once_cell::AsyncOnceCell;
use buck2_core::cells::CellResolver;
use buck2_core::facebook_only;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_events::daemon_id;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::metadata;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::SetBlockingExecutor;
use buck2_execute::execute::dice_data::set_fallback_executor_config;
use buck2_execute::execute::dice_data::SetCommandExecutor;
use buck2_execute::execute::dice_data::SetReClient;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::materialize::materializer::SetMaterializer;
use buck2_execute::re::client::RemoteExecutionClient;
use buck2_execute::re::manager::ReConnectionHandle;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_execute::re::manager::ReConnectionObserver;
use buck2_execute_impl::executors::worker::WorkerPool;
use buck2_execute_impl::low_pass_filter::LowPassFilter;
use buck2_file_watcher::file_watcher::FileWatcher;
use buck2_file_watcher::mergebase::SetMergebase;
use buck2_forkserver::client::ForkserverClient;
use buck2_interpreter::dice::starlark_debug::SetStarlarkDebugger;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::extra::xcode::XcodeVersionInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::prelude_path::prelude_path;
use buck2_interpreter_for_build::interpreter::configuror::BuildInterpreterConfiguror;
use buck2_interpreter_for_build::interpreter::cycles::LoadCycleDescriptor;
use buck2_interpreter_for_build::interpreter::globals::configure_build_file_globals;
use buck2_interpreter_for_build::interpreter::globals::configure_bxl_file_globals;
use buck2_interpreter_for_build::interpreter::globals::configure_extension_file_globals;
use buck2_interpreter_for_build::interpreter::globals::configure_package_file_globals;
use buck2_interpreter_for_build::interpreter::interpreter_setup::setup_interpreter;
use buck2_server_ctx::concurrency::ConcurrencyHandler;
use buck2_server_ctx::concurrency::DiceDataProvider;
use buck2_server_ctx::concurrency::DiceUpdater;
use buck2_server_ctx::ctx::DiceAccessor;
use buck2_server_ctx::ctx::PrivateStruct;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::stderr_output_guard::StderrOutputGuard;
use buck2_server_ctx::stderr_output_guard::StderrOutputWriter;
use buck2_server_starlark_debug::create_debugger_handle;
use buck2_server_starlark_debug::BuckStarlarkDebuggerHandle;
use buck2_util::truncate::truncate_container;
use dice::DiceComputations;
use dice::DiceData;
use dice::DiceTransactionUpdater;
use dice::UserComputationData;
use dice::UserCycleDetector;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use host_sharing::HostSharingBroker;
use host_sharing::HostSharingStrategy;
use more_futures::cancellation::ExplicitCancellationContext;
use tokio::sync::Mutex;
use tracing::warn;

use crate::active_commands::ActiveCommandDropGuard;
use crate::configs::parse_legacy_cells;
use crate::daemon::common::get_default_executor_config;
use crate::daemon::common::parse_concurrency;
use crate::daemon::common::CommandExecutorFactory;
use crate::dice_tracker::BuckDiceTracker;
use crate::heartbeat_guard::HeartbeatGuard;
use crate::host_info;

#[derive(Debug, thiserror::Error)]
enum DaemonCommunicationError {
    #[error("Got invalid working directory `{0}`")]
    InvalidWorkingDirectory(String),
}

/// BaseCommandContext provides access to the global daemon state and information specific to a command (like the
/// EventDispatcher). Most commands use a ServerCommandContext which has more command/client-specific information.
pub struct BaseServerCommandContext {
    /// An fbinit token for using things that require fbinit. fbinit is initialized on daemon startup.
    pub _fb: fbinit::FacebookInit,
    /// Absolute path to the project root.
    pub project_root: ProjectRoot,
    /// A reference to the dice graph. Most interesting things are accessible from this (and new interesting things should be
    /// added there rather than as fields here). This has some per-request setup done already (like attaching a per-request
    /// event dispatcher).
    pub dice_manager: ConcurrencyHandler,
    /// A reference to the I/O provider.
    pub io: Arc<dyn IoProvider>,
    /// The RE connection, managed such that all build commands that are concurrently active uses
    /// the same connection.
    pub re_client_manager: Arc<ReConnectionManager>,
    /// Executor responsible for coordinating and rate limiting I/O.
    pub blocking_executor: Arc<dyn BlockingExecutor>,
    /// Object responsible for handling most materializations.
    pub materializer: Arc<dyn Materializer>,
    /// Forkserver connection, if any was started
    pub forkserver: Option<ForkserverClient>,
    /// The event dispatcher for this command context.
    pub events: EventDispatcher,
    /// Removes this command from the set of active commands when dropped.
    pub _drop_guard: ActiveCommandDropGuard,
    /// The file watcher that keeps buck2 up to date with disk changes.
    pub file_watcher: Arc<dyn FileWatcher>,
    /// Whether or not to hash all commands
    pub hash_all_commands: bool,
    /// Whether to try to read from the network action output cache when running
    /// network actions like download_file; only useful for offline builds.
    pub use_network_action_output_cache: bool,
    /// Start time to track daemon uptime
    pub daemon_start_time: Instant,
    /// Mutex for creating symlinks
    pub create_unhashed_outputs_lock: Arc<Mutex<()>>,
    /// Http client used during run actions; shared with materializer.
    pub http_client: CountingHttpClient,
}

/// ServerCommandContext provides access to the global daemon state and information about the calling client for
/// the implementation of DaemonApi endpoints (ex. targets, query, build).
pub struct ServerCommandContext<'a> {
    pub base_context: BaseServerCommandContext,

    /// The working directory of the client. This is used for resolving things in the request in a
    /// working-dir relative way. For example, it's common to resolve target patterns relative to
    /// the working directory and resolving cell aliases there. This should generally only be used
    /// to interpret values that are in the request. We should convert to client-agnostic things early.
    pub working_dir: ProjectRelativePathBuf,

    working_dir_abs: WorkingDir,

    /// The oncall specified by the client, if any. This gets injected into request metadata.
    pub oncall: Option<String>,

    host_platform_override: HostPlatformOverride,
    host_arch_override: HostArchOverride,
    host_xcode_version_override: Option<String>,

    // This ensures that there's only one RE connection during the lifetime of this context. It's possible
    // that we give out other handles, but we don't depend on the lifetimes of those for this guarantee. We
    // also use this to send a RemoteExecutionSessionCreated if the connection is made.
    _re_connection_handle: ReConnectionHandle,

    /// Starlark profiler instrumentation requested throughout the duration of this command. Usually associated with
    /// the `buck2 profile` command.
    pub starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,

    debugger_handle: Option<BuckStarlarkDebuggerHandle>,

    record_target_call_stacks: bool,
    skip_targets_with_duplicate_names: bool,
    disable_starlark_types: bool,

    pub buck_out_dir: ProjectRelativePathBuf,

    /// Common build options associated with this command.
    build_options: Option<CommonBuildOptions>,

    /// The CellResolver and Configs loader for this command
    cell_configs_loader: Arc<CellConfigLoader>,

    /// Keep emitting heartbeat events while the ServerCommandContext is alive  We put this in an
    /// Option so that we can ensure heartbeat events are cancelled before everything else is
    /// dropped.
    heartbeat_guard_handle: Option<HeartbeatGuard>,

    /// Daemon uuid passed in from the client side to detect nested invocation.
    pub(crate) daemon_uuid_from_client: Option<String>,

    /// Sanitized argument vector from the CLI from the client side.
    pub(crate) sanitized_argv: Vec<String>,

    cancellations: &'a ExplicitCancellationContext,

    exit_when_different_state: bool,
}

impl<'a> ServerCommandContext<'a> {
    pub fn new(
        base_context: BaseServerCommandContext,
        client_context: &ClientContext,
        starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,
        build_options: Option<&CommonBuildOptions>,
        buck_out_dir: ProjectRelativePathBuf,
        cancellations: &'a ExplicitCancellationContext,
    ) -> anyhow::Result<Self> {
        let working_dir = AbsNormPath::new(&client_context.working_dir)?;

        let working_dir_project_relative = working_dir
            .strip_prefix(base_context.project_root.root())
            .map_err(|_| {
                Into::<anyhow::Error>::into(DaemonCommunicationError::InvalidWorkingDirectory(
                    client_context.working_dir.clone(),
                ))
            })?;

        #[derive(Allocative)]
        struct Observer {
            events: EventDispatcher,
        }

        impl ReConnectionObserver for Observer {
            fn session_created(&self, client: &RemoteExecutionClient) {
                let session_id = client.get_session_id();
                let experiment_name = match client.get_experiment_name() {
                    Ok(Some(exp)) => exp,
                    Ok(None) => "".to_owned(),
                    Err(e) => {
                        tracing::debug!("Failed to access RE experiment name: {:#}", e);
                        "<ffi error>".to_owned()
                    }
                };

                self.events
                    .instant_event(buck2_data::RemoteExecutionSessionCreated {
                        session_id: session_id.to_owned(),
                        experiment_name,
                    })
            }
        }

        let mut re_connection_handle = base_context.re_client_manager.get_re_connection();

        re_connection_handle.set_observer(Arc::new(Observer {
            events: base_context.events.dupe(),
        }));

        // Add argfiles read by client into IO tracing state.
        if let Some(tracing_provider) = base_context.io.as_any().downcast_ref::<TracingIoProvider>()
        {
            let argfiles: anyhow::Result<Vec<_>> = client_context
                .argfiles
                .iter()
                .map(|s| AbsNormPathBuf::new(s.into()))
                .collect();
            tracing_provider.add_config_paths(&base_context.project_root, argfiles?);
        }

        let oncall = if client_context.oncall.is_empty() {
            None
        } else {
            Some(client_context.oncall.clone())
        };

        let heartbeat_guard_handle = HeartbeatGuard::new(&base_context);

        let cell_configs_loader = Arc::new(CellConfigLoader {
            project_root: base_context.project_root.clone(),
            working_dir: working_dir_project_relative.to_buf().into(),
            reuse_current_config: client_context.reuse_current_config,
            config_overrides: client_context.config_overrides.clone(),
            loaded_cell_configs: AsyncOnceCell::new(),
        });

        let debugger_handle = create_debugger_handle(base_context.events.dupe());

        Ok(ServerCommandContext {
            base_context,
            working_dir: working_dir_project_relative.to_buf().into(),
            working_dir_abs: WorkingDir::unchecked_new(working_dir.to_buf()),
            host_platform_override: client_context.host_platform(),
            host_arch_override: client_context.host_arch(),
            host_xcode_version_override: client_context.host_xcode_version.clone(),
            oncall,
            _re_connection_handle: re_connection_handle,
            starlark_profiler_instrumentation_override,
            buck_out_dir,
            build_options: build_options.cloned(),
            cell_configs_loader,
            record_target_call_stacks: client_context.target_call_stacks,
            skip_targets_with_duplicate_names: client_context.skip_targets_with_duplicate_names,
            disable_starlark_types: client_context.disable_starlark_types,
            heartbeat_guard_handle: Some(heartbeat_guard_handle),
            daemon_uuid_from_client: client_context.daemon_uuid.clone(),
            sanitized_argv: client_context.sanitized_argv.clone(),
            debugger_handle,
            cancellations,
            exit_when_different_state: client_context.exit_when_different_state,
        })
    }

    async fn dice_data_constructor(
        &self,
        build_signals: BuildSignalsInstaller,
    ) -> DiceCommandDataProvider {
        let execution_strategy = self
            .build_options
            .as_ref()
            .map(|opts| opts.execution_strategy)
            .map_or(ExecutionStrategy::LocalOnly, |strategy| {
                ExecutionStrategy::from_i32(strategy).expect("execution strategy should be valid")
            });

        let skip_cache_read = self
            .build_options
            .as_ref()
            .map(|opts| opts.skip_cache_read)
            .unwrap_or_default();

        let skip_cache_write = self
            .build_options
            .as_ref()
            .map(|opts| opts.skip_cache_write)
            .unwrap_or_default();

        let mut run_action_knobs = RunActionKnobs {
            hash_all_commands: self.base_context.hash_all_commands,
            use_network_action_output_cache: self.base_context.use_network_action_output_cache,
            ..Default::default()
        };

        if let Some(build_options) = self.build_options.as_ref() {
            run_action_knobs.eager_dep_files = build_options.eager_dep_files;
        }

        let concurrency = self
            .build_options
            .as_ref()
            .and_then(|opts| opts.concurrency.as_ref())
            .map(|obj| parse_concurrency(obj.concurrency))
            .map(|v| v.map_err(SharedError::from));

        let executor_config = get_default_executor_config(self.host_platform_override);
        let blocking_executor: Arc<_> = self.base_context.blocking_executor.dupe();
        let materializer = self.base_context.materializer.dupe();
        let re_connection = Arc::new(self.get_re_connection());

        let forkserver = self.base_context.forkserver.dupe();

        let upload_all_actions = self
            .build_options
            .as_ref()
            .map_or(false, |opts| opts.upload_all_actions);

        let create_unhashed_symlink_lock = self.base_context.create_unhashed_outputs_lock.dupe();

        DiceCommandDataProvider {
            cell_configs_loader: self.cell_configs_loader.dupe(),
            events: self.events().dupe(),
            execution_strategy,
            run_action_knobs,
            concurrency,
            executor_config: Arc::new(executor_config),
            blocking_executor,
            materializer,
            re_connection,
            build_signals,
            forkserver,
            upload_all_actions,
            skip_cache_read,
            skip_cache_write,
            create_unhashed_symlink_lock,
            starlark_debugger: self.debugger_handle.dupe(),
            keep_going: self
                .build_options
                .as_ref()
                .map_or(false, |opts| opts.keep_going),
            http_client: self.base_context.http_client.dupe(),
        }
    }

    async fn dice_updater(&self) -> anyhow::Result<DiceCommandUpdater> {
        let (interpreter_platform, interpreter_architecture, interpreter_xcode_version) =
            host_info::get_host_info(
                self.host_platform_override,
                self.host_arch_override,
                &self.host_xcode_version_override,
            )?;

        Ok(DiceCommandUpdater {
            file_watcher: self.base_context.file_watcher.dupe(),
            cell_config_loader: self.cell_configs_loader.dupe(),
            buck_out_dir: self.buck_out_dir.clone(),
            interpreter_platform,
            interpreter_architecture,
            interpreter_xcode_version,
            starlark_profiler_instrumentation_override: self
                .starlark_profiler_instrumentation_override
                .dupe(),
            disable_starlark_types: self.disable_starlark_types,
            skip_targets_with_duplicate_names: self.skip_targets_with_duplicate_names,
            record_target_call_stacks: self.record_target_call_stacks,
        })
    }

    pub fn get_re_connection(&self) -> ReConnectionHandle {
        self.base_context.re_client_manager.get_re_connection()
    }
}

struct CellConfigLoader {
    project_root: ProjectRoot,
    working_dir: ProjectRelativePathBuf,
    /// Reuses build config from the previous invocation if there is one
    reuse_current_config: bool,
    config_overrides: Vec<ConfigOverride>,
    loaded_cell_configs:
        AsyncOnceCell<SharedResult<(CellResolver, LegacyBuckConfigs, HashSet<AbsNormPathBuf>)>>,
}

impl CellConfigLoader {
    pub async fn cells_and_configs(
        &self,
        dice_ctx: &DiceComputations,
    ) -> SharedResult<(CellResolver, LegacyBuckConfigs, HashSet<AbsNormPathBuf>)> {
        self.loaded_cell_configs
            .get_or_init(async move {
                if self.reuse_current_config {
                    // If there is a previous command and --reuse-current-config is set, then the old config is used, ignoring any overrides.
                    if dice_ctx.is_cell_resolver_key_set().await?
                        && dice_ctx.is_legacy_configs_key_set().await?
                    {
                        if !self.config_overrides.is_empty() {
                            warn!(
                                "Found config overrides while using --reuse-current-config flag. Ignoring overrides [{}] and using current config instead",
                                truncate_container(self.config_overrides.iter().map(|e| &*e.config_override), 200),
                            );
                        }
                        return Ok::<(CellResolver, LegacyBuckConfigs, HashSet<AbsNormPathBuf>), anyhow::Error>((
                            dice_ctx.get_cell_resolver().await?,
                            dice_ctx.get_legacy_configs().await?,
                            HashSet::new(),
                        )).shared_error();
                    } else {
                        // If there is no previous command but the flag was set, then the flag is ignored, the command behaves as if there isn't the reuse config flag.
                        warn!(
                            "--reuse-current-config flag was set, but there was no previous invocation detected. Ignoring --reuse-current-config flag"
                        );
                    }
                }
                parse_legacy_cells(self.config_overrides.iter(), &self.working_dir, &self.project_root)
                    .shared_error()
            })
            .await
            .clone()
    }
}

struct DiceCommandDataProvider {
    cell_configs_loader: Arc<CellConfigLoader>,
    execution_strategy: ExecutionStrategy,
    events: EventDispatcher,
    concurrency: Option<Result<usize, SharedError>>,
    executor_config: Arc<CommandExecutorConfig>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    materializer: Arc<dyn Materializer>,
    re_connection: Arc<ReConnectionHandle>,
    build_signals: BuildSignalsInstaller,
    forkserver: Option<ForkserverClient>,
    upload_all_actions: bool,
    run_action_knobs: RunActionKnobs,
    skip_cache_read: bool,
    skip_cache_write: bool,
    create_unhashed_symlink_lock: Arc<Mutex<()>>,
    starlark_debugger: Option<BuckStarlarkDebuggerHandle>,
    keep_going: bool,
    http_client: CountingHttpClient,
}

#[async_trait]
impl DiceDataProvider for DiceCommandDataProvider {
    async fn provide(&self, ctx: &DiceComputations) -> anyhow::Result<UserComputationData> {
        let (cell_resolver, legacy_configs, _): (CellResolver, LegacyBuckConfigs, _) =
            self.cell_configs_loader.cells_and_configs(ctx).await?;

        // TODO(cjhopman): The CellResolver and the legacy configs shouldn't be leaves on the graph. This should
        // just be setting the config overrides and host platform override as leaves on the graph.

        let root_config = legacy_configs
            .get(cell_resolver.root_cell())
            .context("No config for root cell")?;

        let config_threads = root_config.parse("build", "threads")?.unwrap_or(0);

        let concurrency = match self.concurrency.as_ref() {
            Some(v) => v.dupe()?,
            None => parse_concurrency(config_threads)?,
        };

        if let Some(max_lines) = root_config.parse("ui", "thread_line_limit")? {
            self.events
                .instant_event(buck2_data::ConsolePreferences { max_lines });
        }

        let enable_miniperf = root_config
            .parse::<RolloutPercentage>("buck2", "miniperf2")?
            .unwrap_or_else(RolloutPercentage::always)
            .roll();

        let log_action_keys = root_config
            .parse::<RolloutPercentage>("buck2", "log_action_keys")?
            .unwrap_or_else(RolloutPercentage::always)
            .roll();

        let executor_global_knobs = ExecutorGlobalKnobs {
            enable_miniperf,
            log_action_keys,
        };

        let host_sharing_broker =
            HostSharingBroker::new(HostSharingStrategy::SmallerTasksFirst, concurrency);

        // We use the job count for the low pass filter too. The low pass filter prevents sending
        // RE-eligile tasks to local if their concurrency is higher than our threshold. While it
        // doesn't *have* to be the same as the concurrency we give the actual executor, it's a
        // reasonable pick, because if we send more tasks than our concurrency limit allows, we
        // would expect to start losing out to RE in terms of perf.
        let low_pass_filter = LowPassFilter::new(concurrency);

        let mut data = DiceData::new();
        data.set(self.events.dupe());

        let cycle_detector = if root_config
            .parse::<bool>("build", "lazy_cycle_detector")?
            .unwrap_or(true)
        {
            Some(create_cycle_detector())
        } else {
            None
        };
        let has_cycle_detector = cycle_detector.is_some();

        let mut run_action_knobs = self.run_action_knobs.dupe();
        run_action_knobs.use_network_action_output_cache |= root_config
            .parse::<bool>("buck2", "use_network_action_output_cache")?
            .unwrap_or(false);

        run_action_knobs.enforce_re_timeouts = root_config
            .parse::<bool>("buck2", "enforce_re_timeouts")?
            .unwrap_or(true);

        let mut data = UserComputationData {
            data,
            tracker: Arc::new(BuckDiceTracker::new(self.events.dupe())),
            cycle_detector,
            activation_tracker: Some(self.build_signals.activation_tracker.dupe()),
            ..Default::default()
        };

        let worker_pool = Arc::new(WorkerPool::new());

        let critical_path_backend = root_config
            .parse("buck2", "critical_path_backend2")?
            .unwrap_or(CriticalPathBackendName::Default);

        set_fallback_executor_config(&mut data.data, self.executor_config.dupe());
        data.set_re_client(self.re_connection.get_client());
        data.set_command_executor(Box::new(CommandExecutorFactory::new(
            self.re_connection.dupe(),
            host_sharing_broker,
            low_pass_filter,
            self.materializer.dupe(),
            self.blocking_executor.dupe(),
            self.execution_strategy,
            executor_global_knobs,
            self.upload_all_actions,
            self.forkserver.dupe(),
            self.skip_cache_read,
            self.skip_cache_write,
            ctx.global_data()
                .get_io_provider()
                .project_root()
                .to_owned(),
            worker_pool,
        )));
        data.set_blocking_executor(self.blocking_executor.dupe());
        data.set_http_client(self.http_client.dupe());
        data.set_materializer(self.materializer.dupe());
        data.set_build_signals(self.build_signals.build_signals.dupe());
        data.set_run_action_knobs(run_action_knobs);
        data.set_create_unhashed_symlink_lock(self.create_unhashed_symlink_lock.dupe());
        data.set_starlark_debugger_handle(self.starlark_debugger.clone().map(|v| Box::new(v) as _));
        data.set_keep_going(self.keep_going);
        data.set_critical_path_backend(critical_path_backend);
        data.spawner = Arc::new(BuckSpawner);

        let tags = vec![
            format!("lazy-cycle-detector:{}", has_cycle_detector),
            format!("miniperf:{}", enable_miniperf),
        ];
        self.events.instant_event(buck2_data::TagEvent { tags });

        self.events.instant_event(buck2_data::CommandOptions {
            concurrency: concurrency as _,
        });

        Ok(data)
    }
}

fn create_cycle_detector() -> Arc<dyn UserCycleDetector> {
    Arc::new(PairDiceCycleDetector(
        CycleDetectorAdapter::<LoadCycleDescriptor>::new(),
        CycleDetectorAdapter::<ConfiguredGraphCycleDescriptor>::new(),
    ))
}

struct DiceCommandUpdater {
    file_watcher: Arc<dyn FileWatcher>,
    cell_config_loader: Arc<CellConfigLoader>,
    buck_out_dir: ProjectRelativePathBuf,
    interpreter_platform: InterpreterHostPlatform,
    interpreter_architecture: InterpreterHostArchitecture,
    interpreter_xcode_version: Option<XcodeVersionInfo>,
    starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,
    disable_starlark_types: bool,
    record_target_call_stacks: bool,
    skip_targets_with_duplicate_names: bool,
}

#[async_trait]
impl DiceUpdater for DiceCommandUpdater {
    async fn update(
        &self,
        ctx: DiceTransactionUpdater,
        user_data: &mut UserComputationData,
    ) -> anyhow::Result<DiceTransactionUpdater> {
        let (cell_resolver, legacy_configs, _): (CellResolver, LegacyBuckConfigs, _) = self
            .cell_config_loader
            .cells_and_configs(ctx.existing_state().await.deref())
            .await?;
        // TODO(cjhopman): The CellResolver and the legacy configs shouldn't be leaves on the graph. This should
        // just be setting the config overrides and host platform override as leaves on the graph.

        let cell_alias_resolver = cell_resolver.root_cell_instance().cell_alias_resolver();

        let configuror = BuildInterpreterConfiguror::new(
            Some(prelude_path(cell_alias_resolver)?),
            self.interpreter_platform,
            self.interpreter_architecture,
            self.interpreter_xcode_version.clone(),
            self.record_target_call_stacks,
            self.skip_targets_with_duplicate_names,
            configure_build_file_globals,
            configure_package_file_globals,
            configure_extension_file_globals,
            configure_bxl_file_globals,
            None,
        )?;

        let (mut ctx, mergebase) = self.file_watcher.sync(ctx).await?;
        user_data.set_mergebase(mergebase);

        ctx.set_buck_out_path(Some(self.buck_out_dir.clone()))?;

        setup_interpreter(
            &mut ctx,
            cell_resolver,
            configuror,
            legacy_configs,
            self.starlark_profiler_instrumentation_override.dupe(),
            self.disable_starlark_types,
        )?;

        Ok(ctx)
    }
}

impl<'a> Drop for ServerCommandContext<'a> {
    fn drop(&mut self) {
        // Ensure we cancel the heartbeat guard first.
        std::mem::drop(self.heartbeat_guard_handle.take());
    }
}

#[async_trait]
impl<'a> ServerCommandContextTrait for ServerCommandContext<'a> {
    fn working_dir(&self) -> &ProjectRelativePath {
        &self.working_dir
    }

    fn working_dir_abs(&self) -> &WorkingDir {
        &self.working_dir_abs
    }

    fn project_root(&self) -> &ProjectRoot {
        &self.base_context.project_root
    }

    fn materializer(&self) -> Arc<dyn Materializer> {
        self.base_context.materializer.dupe()
    }

    /// Provides a DiceTransaction, initialized on first use and shared after initialization.
    async fn dice_accessor(&self, _private: PrivateStruct) -> SharedResult<DiceAccessor> {
        let (build_signals_installer, deferred_build_signals) = create_build_signals();

        let is_nested_invocation = if let Some(uuid) = &self.daemon_uuid_from_client {
            uuid == &daemon_id::DAEMON_UUID.to_string()
        } else {
            false
        };

        Ok(DiceAccessor {
            dice_handler: self.base_context.dice_manager.dupe(),
            data: Box::new(self.dice_data_constructor(build_signals_installer).await),
            setup: Box::new(self.dice_updater().await?),
            is_nested_invocation,
            sanitized_argv: self.sanitized_argv.clone(),
            exit_when_different_state: self.exit_when_different_state,
            build_signals: deferred_build_signals,
        })
    }

    fn events(&self) -> &EventDispatcher {
        &self.base_context.events
    }

    fn stderr(&self) -> anyhow::Result<StderrOutputGuard<'_>> {
        Ok(StderrOutputGuard {
            _phantom: PhantomData,
            inner: BufWriter::with_capacity(
                // TODO(nga): no need to buffer here.
                4096,
                StderrOutputWriter::new(self)?,
            ),
        })
    }

    /// Gathers metadata to attach to events for when a command starts and stops.
    async fn request_metadata(&self) -> anyhow::Result<HashMap<String, String>> {
        // Facebook only: metadata collection for Scribe writes
        facebook_only();

        let mut metadata = metadata::collect();

        metadata.insert(
            "io_provider".to_owned(),
            self.base_context.io.name().to_owned(),
        );

        metadata.insert(
            "materializer".to_owned(),
            self.base_context.materializer.name().to_owned(),
        );

        if let Some(oncall) = &self.oncall {
            metadata.insert("oncall".to_owned(), oncall.clone());
        }

        Ok(metadata)
    }

    /// Gathers metadata from buckconfig to attach to events for when a command enters the critical
    /// section
    async fn config_metadata(
        &self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<HashMap<String, String>> {
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

        let mut metadata = HashMap::new();
        // In the case of invalid configuration (e.g. something like buck2 build -c X), `dice_ctx_default` returns an
        // error. We won't be able to get configs to log in that case, but we shouldn't crash.
        let (cells, configs, paths): (CellResolver, LegacyBuckConfigs, HashSet<AbsNormPathBuf>) =
            self.cell_configs_loader.cells_and_configs(ctx).await?;

        // Add legacy config paths to I/O tracing (if enabled).
        if let Some(tracing_provider) = self
            .base_context
            .io
            .as_any()
            .downcast_ref::<TracingIoProvider>()
        {
            tracing_provider.add_config_paths(&self.base_context.project_root, paths);
        }

        let root_cell_config = configs.get(cells.root_cell());
        if let Ok(config) = root_cell_config {
            add_config(&mut metadata, config, "log", "repository", "repository");

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

            if let Ok(cwd_cell_name) = cells.find(&self.working_dir) {
                let cwd_cell_config = configs.get(cwd_cell_name).ok();
                if let Some(normals_obj) = extract_scuba_defaults(cwd_cell_config) {
                    for (key, value) in normals_obj.iter() {
                        if let Some(value) = value.as_str() {
                            metadata.insert(key.clone(), value.to_owned());
                        }
                    }
                }

                // `client.id` is often set via the `-c` flag; `-c` configuration is assigned to the cwd cell and not
                // the root cell.
                if let Some(config) = cwd_cell_config {
                    add_config(&mut metadata, config, "client", "id", "client");
                    add_config(
                        &mut metadata,
                        config,
                        "cache",
                        "schedule_type",
                        "schedule_type",
                    );
                }
            }
        }

        Ok(metadata)
    }

    fn log_target_pattern(
        &self,
        providers_patterns: &[ParsedPattern<ConfiguredProvidersPatternExtra>],
    ) {
        let patterns = providers_patterns.map(|pat| buck2_data::TargetPattern {
            value: format!("{}", pat),
        });

        self.events()
            .instant_event(buck2_data::ParsedTargetPatterns {
                target_patterns: patterns,
            })
    }

    fn cancellation_context(&self) -> &ExplicitCancellationContext {
        self.cancellations
    }
}
