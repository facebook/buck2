/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufWriter;
use std::marker::PhantomData;
use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::actions::execute::dice_data::SetCommandExecutor;
use buck2_build_api::actions::execute::dice_data::SetReClient;
use buck2_build_api::actions::execute::dice_data::set_fallback_executor_config;
use buck2_build_api::actions::impls::run_action_knobs::HasRunActionKnobs;
use buck2_build_api::actions::impls::run_action_knobs::RunActionKnobs;
use buck2_build_api::build::HasCreateUnhashedSymlinkLock;
use buck2_build_api::build::detailed_aggregated_metrics::dice::SetDetailedAggregatedMetricsEventsHolder;
use buck2_build_api::build_signals::BuildSignalsInstaller;
use buck2_build_api::build_signals::SetBuildSignals;
use buck2_build_api::build_signals::create_build_signals;
use buck2_build_api::context::SetBuildContextData;
use buck2_build_api::keep_going::HasKeepGoing;
use buck2_build_api::materialize::HasMaterializationQueueTracker;
use buck2_build_api::materialize::HasMaterializerFastRolloutConfig;
use buck2_build_api::materialize::MaterializerFastRolloutConfig;
use buck2_build_api::spawner::BuckSpawner;
use buck2_build_signals::env::CriticalPathBackendName;
use buck2_build_signals::env::EarlyCommandTimingBuilder;
use buck2_build_signals::env::FILE_WATCHER_WAIT;
use buck2_build_signals::env::HasCriticalPathBackend;
use buck2_certs::validate::CertState;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::CommonBuildOptions;
use buck2_cli_proto::ConfigOverride;
use buck2_cli_proto::client_context::ExitWhen;
use buck2_cli_proto::client_context::HostArchOverride;
use buck2_cli_proto::client_context::HostPlatformOverride;
use buck2_cli_proto::client_context::PreemptibleWhen;
use buck2_cli_proto::common_build_options::ExecutionStrategy;
use buck2_cli_proto::config_override::ConfigType;
use buck2_common::dice::cycles::CycleDetectorAdapter;
use buck2_common::dice::cycles::PairDiceCycleDetector;
use buck2_common::file_ops::io::initialize_read_dir_cache;
use buck2_common::http::SetHttpClient;
use buck2_common::invocation_paths::InvocationPaths;
use buck2_common::io::trace::TracingIoProvider;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::configs::LegacyBuckConfig;
use buck2_common::legacy_configs::dice::HasInjectedLegacyConfigs;
use buck2_common::legacy_configs::file_ops::ConfigPath;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_configured::cycle::ConfiguredGraphCycleDescriptor;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::facebook_only;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern::ParsedPatternWithModifiers;
use buck2_core::pattern::pattern_type::ConfiguredProvidersPatternExtra;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_core::target::label::interner::ConcurrentTargetLabelInterner;
use buck2_directory::directory::dashmap_directory_interner::DashMapDirectoryInterner;
use buck2_events::dispatch::EventDispatcher;
use buck2_events::metadata;
use buck2_events::schedule_type::SandcastleScheduleType;
use buck2_execute::execute::blocking::SetBlockingExecutor;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::materialize::materializer::SetMaterializer;
use buck2_execute::re::client::RemoteExecutionClient;
use buck2_execute::re::manager::ReConnectionHandle;
use buck2_execute::re::manager::ReConnectionObserver;
use buck2_execute::re::output_trees_download_config::OutputTreesDownloadConfig;
use buck2_execute_impl::executors::worker::WorkerPool;
use buck2_execute_impl::low_pass_filter::LowPassFilter;
use buck2_file_watcher::mergebase::SetMergebase;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;
use buck2_fs::working_dir::AbsWorkingDir;
use buck2_interpreter::dice::starlark_debug::SetStarlarkDebugger;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::extra::xcode::XcodeVersionInfo;
use buck2_interpreter::factory::SetProfileEventListener;
use buck2_interpreter::prelude_path::prelude_path;
use buck2_interpreter_for_build::interpreter::configuror::BuildInterpreterConfiguror;
use buck2_interpreter_for_build::interpreter::cycles::LoadCycleDescriptor;
use buck2_interpreter_for_build::interpreter::interpreter_setup::setup_interpreter;
use buck2_resource_control::HasResourceControl;
use buck2_server_ctx::bxl::InitBxlStreamingTracker;
use buck2_server_ctx::concurrency::DiceUpdater;
use buck2_server_ctx::ctx::DiceAccessor;
use buck2_server_ctx::ctx::LockedPreviousCommandData;
use buck2_server_ctx::ctx::PrivateStruct;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::stderr_output_guard::StderrOutputGuard;
use buck2_server_ctx::stderr_output_guard::StderrOutputWriter;
use buck2_server_starlark_debug::BuckStarlarkDebuggerHandle;
use buck2_server_starlark_debug::create_debugger_handle;
use buck2_test::local_resource_registry::InitLocalResourceRegistry;
use buck2_util::arc_str::ArcS;
use buck2_util::truncate::truncate_container;
use buck2_validation::enabled_optional_validations_key::SetEnabledOptionalValidations;
use dice::DiceComputations;
use dice::DiceData;
use dice::DiceTransactionUpdater;
use dice::UserComputationData;
use dice::UserCycleDetector;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use host_sharing::HostSharingBroker;
use host_sharing::HostSharingStrategy;
use tracing::warn;

use crate::active_commands::ActiveCommandDropGuard;
use crate::daemon::common::CommandExecutorFactory;
use crate::daemon::common::get_default_executor_config;
use crate::daemon::state::DaemonStateData;
use crate::dice_tracker::BuckDiceTracker;
use crate::heartbeat_guard::HeartbeatGuard;
use crate::host_info;
use crate::profile_patterns::FileWritingProfileEventListener;
use crate::profiling_manager::StarlarkProfilingManager;
use crate::snapshot::SnapshotCollector;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Environment)]
enum DaemonCommunicationError {
    #[error("Got invalid working directory `{0}`")]
    InvalidWorkingDirectory(String),
}

fn parse_concurrency(requested: u32) -> Option<usize> {
    let ret: usize = requested
        .try_into()
        .expect("Buck2 isn't built for 16 bit systems");

    if ret == 0 { None } else { Some(ret) }
}

/// BaseCommandContext provides access to the global daemon state and information specific to a command (like the
/// EventDispatcher). Most commands use a ServerCommandContext which has more command/client-specific information.
pub struct BaseServerCommandContext {
    /// An fbinit token for using things that require fbinit. fbinit is initialized on daemon startup.
    pub _fb: fbinit::FacebookInit,
    /// Absolute path to the project root.
    pub project_root: ProjectRoot,
    /// The event dispatcher for this command context.
    pub events: EventDispatcher,
    /// Underlying data that isn't command-level.
    pub(crate) daemon: Arc<DaemonStateData>,
    /// Removes this command from the set of active commands when dropped.
    pub _drop_guard: ActiveCommandDropGuard,
    /// Spawner
    pub spawner: Arc<BuckSpawner>,
}

/// ServerCommandContext provides access to the global daemon state and information about the calling client for
/// the implementation of DaemonApi endpoints (ex. targets, query, build).
pub struct ServerCommandContext<'a> {
    pub base_context: BaseServerCommandContext,

    /// The working directory of the client. This is used for resolving things in the request in a
    /// working-dir relative way. For example, it's common to resolve target patterns relative to
    /// the working directory and resolving cell aliases there. This should generally only be used
    /// to interpret values that are in the request. We should convert to client-agnostic things early.
    pub working_dir: ArcS<ProjectRelativePath>,

    working_dir_abs: AbsWorkingDir,

    /// The oncall specified by the client, if any. This gets injected into request metadata.
    pub oncall: Option<String>,
    /// The client ID, if one was provided via --client-metadata.
    pub client_id_from_client_metadata: Option<String>,

    host_platform_override: HostPlatformOverride,
    host_arch_override: HostArchOverride,
    host_xcode_version_override: Option<String>,

    reuse_current_config: bool,
    config_overrides: Vec<ConfigOverride>,

    // This ensures that there's only one RE connection during the lifetime of this context. It's possible
    // that we give out other handles, but we don't depend on the lifetimes of those for this guarantee. We
    // also use this to send a RemoteExecutionSessionCreated if the connection is made.
    _re_connection_handle: ReConnectionHandle,

    /// Starlark profiler instrumentation requested throughout the duration of this command. Usually associated with
    /// the `buck2 profile` command.
    pub starlark_profiling_manager: StarlarkProfilingManager,

    debugger_handle: Option<BuckStarlarkDebuggerHandle>,

    record_target_call_stacks: bool,
    skip_targets_with_duplicate_names: bool,
    disable_starlark_types: bool,
    unstable_typecheck: bool,

    pub buck_out_dir: ProjectRelativePathBuf,
    isolation_prefix: FileNameBuf,

    /// Common build options associated with this command.
    build_options: Option<CommonBuildOptions>,

    /// Keep emitting heartbeat events while the ServerCommandContext is alive  We put this in an
    /// Option so that we can ensure heartbeat events are cancelled before everything else is
    /// dropped.
    heartbeat_guard_handle: Option<HeartbeatGuard>,

    /// The current state of the certificate. This is used to detect errors due to invalid certs.
    cert_state: CertState,

    /// Daemon uuid passed in from the client side to detect nested invocation.
    pub(crate) daemon_uuid_from_client: Option<String>,

    /// Command named passed from the CLI
    pub(crate) command_name: String,

    /// Sanitized argument vector from the CLI from the client side.
    pub(crate) sanitized_argv: Vec<String>,

    cancellations: &'a CancellationContext,

    preemptible: PreemptibleWhen,

    exit_when: ExitWhen,

    command_start: Instant,
}

impl<'a> ServerCommandContext<'a> {
    pub fn new(
        base_context: BaseServerCommandContext,
        client_context: &ClientContext,
        starlark_profiling_manager: StarlarkProfilingManager,
        build_options: Option<&CommonBuildOptions>,
        paths: &InvocationPaths,
        cert_state: CertState,
        snapshot_collector: SnapshotCollector,
        cancellations: &'a CancellationContext,
        command_start: Instant,
    ) -> buck2_error::Result<Self> {
        let working_dir = AbsNormPath::new(&client_context.working_dir)?;

        let working_dir_project_relative = working_dir
            .strip_prefix(base_context.project_root.root())
            .map_err(|_| {
                Into::<buck2_error::Error>::into(DaemonCommunicationError::InvalidWorkingDirectory(
                    client_context.working_dir.clone(),
                ))
            })?;
        let working_dir_project_relative: ArcS<ProjectRelativePath> =
            ArcS::from(<&ProjectRelativePath>::from(&*working_dir_project_relative));

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
                        persistent_cache_mode: client.get_persistent_cache_mode(),
                    })
            }
        }

        let mut re_connection_handle = base_context.daemon.re_client_manager.get_re_connection();

        re_connection_handle.set_observer(Arc::new(Observer {
            events: base_context.events.dupe(),
        }));

        // Add argfiles read by client into IO tracing state.
        if let Some(tracing_provider) = TracingIoProvider::from_io(&*base_context.daemon.io) {
            for p in client_context
                .argfiles
                .iter()
                .map(|s| AbsNormPathBuf::new(s.into()))
            {
                tracing_provider.add_external_path(p?);
            }
        }

        let oncall = if client_context.oncall.is_empty() {
            None
        } else {
            Some(client_context.oncall.clone())
        };

        // Use rev() to get the last "id" entry if there are duplicates.
        let client_id_from_client_metadata = client_context
            .client_metadata
            .iter()
            .rev()
            .find(|m| m.key == "id")
            .map(|m| m.value.clone());

        let heartbeat_guard_handle =
            HeartbeatGuard::new(base_context.events.dupe(), snapshot_collector);

        let debugger_handle = create_debugger_handle(base_context.events.dupe());

        Ok(ServerCommandContext {
            base_context,
            working_dir: working_dir_project_relative,
            working_dir_abs: AbsWorkingDir::unchecked_new(working_dir.to_buf()),
            host_platform_override: client_context.host_platform(),
            host_arch_override: client_context.host_arch(),
            host_xcode_version_override: client_context.host_xcode_version.clone(),
            reuse_current_config: client_context.reuse_current_config,
            config_overrides: client_context.config_overrides.clone(),
            oncall,
            client_id_from_client_metadata,
            _re_connection_handle: re_connection_handle,
            cert_state,
            starlark_profiling_manager,
            buck_out_dir: paths.buck_out_dir(),
            isolation_prefix: paths.isolation.clone(),
            build_options: build_options.cloned(),
            record_target_call_stacks: client_context.target_call_stacks,
            skip_targets_with_duplicate_names: client_context.skip_targets_with_duplicate_names,
            disable_starlark_types: client_context.disable_starlark_types,
            unstable_typecheck: client_context.unstable_typecheck,
            heartbeat_guard_handle: Some(heartbeat_guard_handle),
            daemon_uuid_from_client: client_context.daemon_uuid.clone(),
            command_name: client_context.command_name.clone(),
            sanitized_argv: client_context.sanitized_argv.clone(),
            debugger_handle,
            cancellations,
            preemptible: client_context.preemptible(),
            exit_when: client_context.exit_when(),
            command_start,
        })
    }

    async fn dice_updater<'s>(
        &'s self,
        build_signals: BuildSignalsInstaller,
    ) -> buck2_error::Result<DiceCommandUpdater<'s, 'a>> {
        let execution_strategy = self
            .build_options
            .as_ref()
            .map(|opts| opts.execution_strategy)
            .map_or(ExecutionStrategy::LocalOnly, |strategy| {
                ExecutionStrategy::try_from(strategy).expect("execution strategy should be valid")
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

        let eager_dep_files = if let Some(build_options) = self.build_options.as_ref() {
            build_options.eager_dep_files
        } else {
            false
        };

        let run_action_knobs = RunActionKnobs {
            use_network_action_output_cache: self
                .base_context
                .daemon
                .use_network_action_output_cache,
            eager_dep_files,
            default_allow_cache_upload: false,
            action_paths_interner: None,
            deduplicate_get_digests_ttl_calls: false,
            re_outputs_required: false,
        };

        let concurrency = self
            .build_options
            .as_ref()
            .and_then(|opts| opts.concurrency.as_ref())
            .and_then(|obj| parse_concurrency(obj.concurrency));

        let executor_config = get_default_executor_config(self.host_platform_override);
        let re_connection = Arc::new(self.get_re_connection());

        let upload_all_actions = self
            .build_options
            .as_ref()
            .is_some_and(|opts| opts.upload_all_actions);

        let (interpreter_platform, interpreter_architecture, interpreter_xcode_version) =
            host_info::get_host_info(
                self.host_platform_override,
                self.host_arch_override,
                &self.host_xcode_version_override,
            )?;

        Ok(DiceCommandUpdater {
            cmd_ctx: self,
            execution_strategy,
            run_action_knobs,
            concurrency,
            executor_config: Arc::new(executor_config),
            re_connection,
            build_signals,
            upload_all_actions,
            skip_cache_read,
            skip_cache_write,
            keep_going: self
                .build_options
                .as_ref()
                .is_some_and(|opts| opts.keep_going),
            materialize_failed_inputs: self
                .build_options
                .as_ref()
                .is_some_and(|opts| opts.materialize_failed_inputs),
            interpreter_platform,
            interpreter_architecture,
            interpreter_xcode_version,
            materialize_failed_outputs: self
                .build_options
                .as_ref()
                .is_some_and(|opts| opts.materialize_failed_outputs),
            profile_event_listener: self
                .starlark_profiling_manager
                .profile_event_listener
                .dupe(),
        })
    }

    pub fn get_re_connection(&self) -> ReConnectionHandle {
        self.base_context
            .daemon
            .re_client_manager
            .get_re_connection()
    }

    // Called at the end of the command to perform any necessary final actions or cleanup.
    pub(crate) async fn finalize(mut self) -> buck2_error::Result<()> {
        self.starlark_profiling_manager.finalize()?;
        self.heartbeat_guard_handle.take().unwrap().finalize().await;
        Ok(())
    }
}

impl ServerCommandContext<'_> {
    async fn load_new_configs(
        &self,
        dice_ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<BuckConfigBasedCells> {
        let new_configs = BuckConfigBasedCells::parse_with_config_args(
            &self.base_context.project_root,
            &self.config_overrides,
        )
        .await?;

        self.report_traced_config_paths(&new_configs.config_paths)?;
        if self.reuse_current_config {
            if dice_ctx
                .is_injected_external_buckconfig_data_key_set()
                .await?
            {
                if !self.config_overrides.is_empty() {
                    let config_type_str = |c| match ConfigType::try_from(c) {
                        Ok(ConfigType::Value) => "--config",
                        Ok(ConfigType::File) => "--config-file",
                        Err(_) => "",
                    };
                    warn!(
                        "Found config overrides while using --reuse-current-config flag. Ignoring overrides [{}] and using current config instead",
                        truncate_container(
                            self.config_overrides.iter().map(|o| {
                                format!("{} {}", config_type_str(o.config_type), o.config_override)
                            }),
                            200
                        ),
                    );
                }
                // If `--reuse-current-config` is set, use the external config data from the
                // previous command.
                Ok(BuckConfigBasedCells {
                    cell_resolver: new_configs.cell_resolver,
                    root_config: new_configs.root_config,
                    config_paths: HashSet::new(),
                    external_data: (*dice_ctx.get_injected_external_buckconfig_data().await?)
                        .clone(),
                })
            } else {
                // If there is no previous command but the flag was set, then the flag is ignored,
                // the command behaves as if there isn't the reuse config flag.
                warn!(
                    "--reuse-current-config flag was set, but there was no previous invocation detected. Ignoring --reuse-current-config flag"
                );
                Ok(new_configs)
            }
        } else {
            Ok(new_configs)
        }
    }

    fn report_traced_config_paths(&self, paths: &HashSet<ConfigPath>) -> buck2_error::Result<()> {
        if let Some(tracing_provider) = TracingIoProvider::from_io(&*self.base_context.daemon.io) {
            for config_path in paths {
                match config_path {
                    ConfigPath::Global(p) => {
                        // FIXME(JakobDegen): This is wrong, since we might fail to add symlinks that we depend on.
                        let p = fs_util::canonicalize(p)
                            // input path could be from --config-file
                            .categorize_input()?;
                        tracing_provider.add_external_path(p)
                    }
                    ConfigPath::Project(p) => tracing_provider.add_project_path(p.clone()),
                }
            }
        }

        Ok(())
    }
}

struct DiceCommandUpdater<'s, 'a: 's> {
    cmd_ctx: &'s ServerCommandContext<'a>,
    execution_strategy: ExecutionStrategy,
    concurrency: Option<usize>,
    executor_config: Arc<CommandExecutorConfig>,
    re_connection: Arc<ReConnectionHandle>,
    profile_event_listener: Option<Arc<FileWritingProfileEventListener>>,
    build_signals: BuildSignalsInstaller,
    upload_all_actions: bool,
    run_action_knobs: RunActionKnobs,
    skip_cache_read: bool,
    skip_cache_write: bool,
    keep_going: bool,
    materialize_failed_inputs: bool,
    materialize_failed_outputs: bool,
    interpreter_platform: InterpreterHostPlatform,
    interpreter_architecture: InterpreterHostArchitecture,
    interpreter_xcode_version: Option<XcodeVersionInfo>,
}

fn create_cycle_detector() -> Arc<dyn UserCycleDetector> {
    Arc::new(PairDiceCycleDetector(
        CycleDetectorAdapter::<LoadCycleDescriptor>::new(),
        CycleDetectorAdapter::<ConfiguredGraphCycleDescriptor>::new(),
    ))
}

#[async_trait]
impl DiceUpdater for DiceCommandUpdater<'_, '_> {
    async fn update(
        &self,
        mut ctx: DiceTransactionUpdater,
        early_timings: &mut EarlyCommandTimingBuilder,
    ) -> buck2_error::Result<(DiceTransactionUpdater, UserComputationData)> {
        let existing_state = &mut ctx.existing_state().await.clone();
        let cells_and_configs = self.cmd_ctx.load_new_configs(existing_state).await?;
        let cell_resolver = cells_and_configs.cell_resolver;

        let configuror = BuildInterpreterConfiguror::new(
            prelude_path(&cell_resolver)?,
            self.interpreter_platform,
            self.interpreter_architecture,
            self.interpreter_xcode_version.clone(),
            self.cmd_ctx.record_target_call_stacks,
            self.cmd_ctx.skip_targets_with_duplicate_names,
            None,
            // New interner for each transaction.
            Arc::new(ConcurrentTargetLabelInterner::default()),
        )?;

        ctx.set_buck_out_path(Some(self.cmd_ctx.buck_out_dir.clone()))?;

        let optional_validations = self
            .cmd_ctx
            .build_options
            .as_ref()
            .map_or(Vec::new(), |opts| opts.enable_optional_validations.clone());

        ctx.set_enabled_optional_validations(optional_validations)?;

        let profiler_instrumentation_override =
            &self.cmd_ctx.starlark_profiling_manager.configuration;

        setup_interpreter(
            &mut ctx,
            cell_resolver,
            configuror,
            cells_and_configs.external_data,
            profiler_instrumentation_override.clone(),
            self.cmd_ctx.disable_starlark_types,
            self.cmd_ctx.unstable_typecheck,
        )?;

        early_timings.start_span(FILE_WATCHER_WAIT.to_owned());
        let (ctx, mergebase) = self
            .cmd_ctx
            .base_context
            .daemon
            .file_watcher
            .sync(ctx)
            .await?;
        early_timings.end_known_span();

        let mut user_data = self.make_user_computation_data(&cells_and_configs.root_config)?;
        user_data.set_mergebase(mergebase);

        Ok((ctx, user_data))
    }
}

impl DiceCommandUpdater<'_, '_> {
    fn make_user_computation_data(
        &self,
        root_config: &LegacyBuckConfig,
    ) -> buck2_error::Result<UserComputationData> {
        let config_threads = root_config
            .parse(BuckconfigKeyRef {
                section: "build",
                property: "threads",
            })?
            .unwrap_or(0);

        let concurrency = self
            .concurrency
            .or_else(|| parse_concurrency(config_threads))
            .unwrap_or_else(buck2_util::threads::available_parallelism_fresh);

        if let Some(max_lines) = root_config.parse(BuckconfigKeyRef {
            section: "ui",
            property: "thread_line_limit",
        })? {
            self.cmd_ctx
                .events()
                .instant_event(buck2_data::ConsolePreferences { max_lines });
        }

        let enable_miniperf = root_config
            .parse::<RolloutPercentage>(BuckconfigKeyRef {
                section: "buck2",
                property: "miniperf2",
            })?
            .unwrap_or_else(RolloutPercentage::always)
            .roll();

        let log_action_keys = root_config
            .parse::<RolloutPercentage>(BuckconfigKeyRef {
                section: "buck2",
                property: "log_action_keys",
            })?
            .unwrap_or_else(RolloutPercentage::always)
            .roll();

        let log_configured_graph_size = root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "log_configured_graph_size",
            })?
            .unwrap_or(false);

        let persistent_worker_shutdown_timeout_s = root_config
            .parse::<u32>(BuckconfigKeyRef {
                section: "build",
                property: "persistent_worker_shutdown_timeout_s",
            })?
            .or(Some(10));

        let re_cancel_on_estimated_queue_time_exceeds = root_config
            .parse::<u64>(BuckconfigKeyRef {
                section: "build",
                property: "remote_execution_cancel_on_estimated_queue_time_exceeds_s",
            })?
            .map(Duration::from_secs);
        let re_fallback_on_estimated_queue_time_exceeds = root_config
            .parse::<u64>(BuckconfigKeyRef {
                section: "build",
                property: "remote_execution_fallback_on_estimated_queue_time_exceeds_s",
            })?
            .map(Duration::from_secs);

        let executor_global_knobs = ExecutorGlobalKnobs {
            enable_miniperf,
            log_action_keys,
            re_cancel_on_estimated_queue_time_exceeds,
            re_fallback_on_estimated_queue_time_exceeds,
        };

        let host_sharing_broker = HostSharingBroker::new_with_named_semaphores(
            HostSharingStrategy::SmallerTasksFirst,
            concurrency,
            self.cmd_ctx
                .base_context
                .daemon
                .named_semaphores_for_run_actions
                .dupe(),
        );

        // We use the job count for the low pass filter too. The low pass filter prevents sending
        // RE-eligile tasks to local if their concurrency is higher than our threshold. While it
        // doesn't *have* to be the same as the concurrency we give the actual executor, it's a
        // reasonable pick, because if we send more tasks than our concurrency limit allows, we
        // would expect to start losing out to RE in terms of perf.
        let low_pass_filter = LowPassFilter::new(concurrency);

        let mut data = DiceData::new();
        data.set(self.cmd_ctx.events().dupe());
        data.set(HasResourceControl(
            self.cmd_ctx.base_context.daemon.memory_tracker.is_some(),
        ));

        let cycle_detector = if root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "build",
                property: "lazy_cycle_detector",
            })?
            .unwrap_or(true)
        {
            Some(create_cycle_detector())
        } else {
            None
        };
        let has_cycle_detector = cycle_detector.is_some();

        let mut run_action_knobs = self.run_action_knobs.dupe();
        run_action_knobs.use_network_action_output_cache |= root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "use_network_action_output_cache",
            })?
            .unwrap_or(false);
        run_action_knobs.default_allow_cache_upload |= root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "default_allow_cache_upload",
            })?
            .unwrap_or(false);

        if root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "share_action_paths",
            })?
            .unwrap_or(false)
        {
            run_action_knobs.action_paths_interner = Some(DashMapDirectoryInterner::new());
        }

        run_action_knobs.deduplicate_get_digests_ttl_calls |= root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "deduplicate_get_digests_ttl_calls",
            })?
            .unwrap_or(false);

        run_action_knobs.re_outputs_required |= root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "re_outputs_required",
            })?
            .unwrap_or(false);

        let output_trees_download_semaphore_size = root_config.parse::<u32>(BuckconfigKeyRef {
            section: "buck2",
            property: "output_trees_download_semaphore_size",
        })?;

        let fingerprint_re_output_trees_eagerly = root_config
            .parse::<bool>(BuckconfigKeyRef {
                section: "buck2",
                property: "fingerprint_re_output_trees_eagerly",
            })?
            .unwrap_or(true);

        let output_trees_download_config = OutputTreesDownloadConfig::new(
            output_trees_download_semaphore_size,
            fingerprint_re_output_trees_eagerly,
        );

        buck2_core::faster_directories::VALUE.store(
            root_config
                .parse::<bool>(BuckconfigKeyRef {
                    section: "buck2",
                    property: "faster_directories",
                })?
                .unwrap_or(true),
            std::sync::atomic::Ordering::Relaxed,
        );

        let mut data = UserComputationData {
            data,
            tracker: Arc::new(BuckDiceTracker::new(self.cmd_ctx.events().dupe())?),
            cycle_detector,
            activation_tracker: Some(self.build_signals.activation_tracker.dupe()),
            ..Default::default()
        };
        data.set_detailed_aggregated_metrics_events_holder();

        let worker_pool = Arc::new(WorkerPool::new(persistent_worker_shutdown_timeout_s));

        let critical_path_backend = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "critical_path_backend2",
            })?
            .unwrap_or(CriticalPathBackendName::LongestPathGraph);

        let override_use_case = root_config.parse::<RemoteExecutorUseCase>(BuckconfigKeyRef {
            section: "buck2_re_client",
            property: "override_use_case",
        })?;

        let (materializer_fast_rollout_tag, materializer_fast_rollout_config) = {
            let spawn_cfg: Option<bool> = root_config.parse(BuckconfigKeyRef {
                section: "buck2",
                property: "materializer_fast_rollout_spawn_override",
            })?;
            let unconstrained_cfg: Option<bool> = root_config.parse(BuckconfigKeyRef {
                section: "buck2",
                property: "materializer_fast_rollout_unconstrained_override",
            })?;
            let materializer_fast_rollout_cfg = root_config.parse(BuckconfigKeyRef {
                section: "buck2",
                property: "materializer_fast_rollout",
            })?;

            let materializer_fast_enabled = materializer_fast_rollout_cfg.unwrap_or(false);
            let spawn = spawn_cfg.unwrap_or(materializer_fast_enabled);
            let unconstrained = unconstrained_cfg.unwrap_or(materializer_fast_enabled);

            let tag = if spawn_cfg.is_some() || unconstrained_cfg.is_some() {
                "materializer_fast_rollout=custom"
            } else if materializer_fast_enabled {
                "materializer_fast_rollout=enabled"
            } else {
                "materializer_fast_rollout=disabled"
            };

            (
                tag,
                MaterializerFastRolloutConfig {
                    spawn,
                    unconstrained,
                },
            )
        };

        data.set_materializer_fast_rollout_config(materializer_fast_rollout_config);

        set_fallback_executor_config(&mut data.data, self.executor_config.dupe());
        // This client is only used in places that do not use the RE use case specified in the executor config.
        // They currently use either a usecase specified in actions (cas_artifact), or a global default (buck2.default_remote_execution_use_case).
        // We should not override the cas_artifact usecase or else the ttl may not match the action declaration.
        data.set_re_client(self.re_connection.get_client());
        if let Some(v) = &self.profile_event_listener {
            SetProfileEventListener::set(&mut data, v.clone());
        }
        data.set_command_executor(Box::new(CommandExecutorFactory::new(
            self.re_connection.dupe(),
            host_sharing_broker,
            low_pass_filter,
            self.cmd_ctx.base_context.daemon.materializer.dupe(),
            self.cmd_ctx.base_context.daemon.blocking_executor.dupe(),
            self.execution_strategy,
            executor_global_knobs,
            self.upload_all_actions,
            self.cmd_ctx.base_context.daemon.forkserver.dupe(),
            self.skip_cache_read,
            self.skip_cache_write,
            self.cmd_ctx.base_context.daemon.io.project_root().dupe(),
            worker_pool,
            self.cmd_ctx.base_context.daemon.paranoid.dupe(),
            self.materialize_failed_inputs,
            self.materialize_failed_outputs,
            override_use_case,
            self.cmd_ctx.base_context.daemon.memory_tracker.dupe(),
            self.cmd_ctx.base_context.daemon.incremental_db_state.dupe(),
            run_action_knobs.deduplicate_get_digests_ttl_calls,
            output_trees_download_config.dupe(),
            self.cmd_ctx.base_context.daemon.daemon_id.dupe(),
        )));
        data.set_blocking_executor(self.cmd_ctx.base_context.daemon.blocking_executor.dupe());
        data.set_http_client(self.cmd_ctx.base_context.daemon.http_client.dupe());
        data.set_materializer(self.cmd_ctx.base_context.daemon.materializer.dupe());
        data.init_materialization_queue_tracker();
        data.set_build_signals(self.build_signals.build_signals.dupe());
        data.set_run_action_knobs(run_action_knobs);
        data.set_create_unhashed_symlink_lock(
            self.cmd_ctx
                .base_context
                .daemon
                .create_unhashed_outputs_lock
                .dupe(),
        );
        data.set_starlark_debugger_handle(
            self.cmd_ctx
                .debugger_handle
                .clone()
                .map(|v| Box::new(v) as _),
        );
        data.set_keep_going(self.keep_going);
        data.set_critical_path_backend(critical_path_backend);
        data.init_local_resource_registry();
        data.init_bxl_streaming_tracker();
        initialize_read_dir_cache(&mut data);
        data.spawner = self.cmd_ctx.base_context.daemon.spawner.dupe();

        let tags = vec![
            format!("lazy-cycle-detector:{}", has_cycle_detector),
            format!("miniperf:{}", enable_miniperf),
            format!("log-configured-graph-size:{}", log_configured_graph_size),
            materializer_fast_rollout_tag.to_owned(),
        ];
        self.cmd_ctx
            .events()
            .instant_event(buck2_data::TagEvent { tags });

        self.cmd_ctx
            .events()
            .instant_event(buck2_data::CommandOptions {
                configured_parallelism: concurrency as _,
                available_parallelism: buck2_util::threads::available_parallelism() as _,
            });

        collect_config_metadata_into(root_config, &mut data);

        Ok(data)
    }
}

struct ConfigMetadataHolder(HashMap<String, String>);

fn collect_config_metadata_into(config: &LegacyBuckConfig, data: &mut UserComputationData) {
    // Facebook only: metadata collection for Scribe writes
    facebook_only();

    fn add_config(
        map: &mut HashMap<String, String>,
        cfg: &LegacyBuckConfig,
        key: BuckconfigKeyRef<'static>,
        field_name: &'static str,
    ) {
        if let Some(value) = cfg.get(key) {
            map.insert(field_name.to_owned(), value.to_owned());
        }
    }

    fn extract_scuba_defaults(
        config: &LegacyBuckConfig,
    ) -> Option<serde_json::Map<String, serde_json::Value>> {
        let config = config.get(BuckconfigKeyRef {
            section: "scuba",
            property: "defaults",
        })?;
        let unescaped_config = shlex::split(config)?.join("");
        let sample_json: serde_json::Value = serde_json::from_str(&unescaped_config).ok()?;
        sample_json.get("normals")?.as_object().cloned()
    }

    let mut metadata = HashMap::new();

    add_config(
        &mut metadata,
        config,
        BuckconfigKeyRef {
            section: "log",
            property: "repository",
        },
        "repository",
    );

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

    if let Some(normals_obj) = extract_scuba_defaults(config) {
        for (key, value) in normals_obj.iter() {
            if let Some(value) = value.as_str() {
                metadata.insert(key.clone(), value.to_owned());
            }
        }
    }

    // TODO(pbergen): Remove this when we desupport client.id in config.
    add_config(
        &mut metadata,
        config,
        BuckconfigKeyRef {
            section: "client",
            property: "id",
        },
        "client",
    );

    // Soft error if client.id is set in buckconfig (deprecated, will become hard error)
    if let Some(client_id) = config.get(BuckconfigKeyRef {
        section: "client",
        property: "id",
    }) {
        use buck2_core::soft_error;

        soft_error!(
            "client_id_in_buckconfig",
            buck2_error::buck2_error!(
                buck2_error::ErrorTag::Input,
                "Setting `client.id` via config (`-c|--config client.id={}`) is deprecated \
                 because it invalidates the DICE graph which causes performance loss. \
                 Please migrate to `--client-metadata=id={}` instead. \
                 This will become a hard error in a future Buck2 release. \
                 For more information, see: https://internalfb.com/intern/staticdocs/buck2/docs/rule_authors/client_metadata/",
                client_id,
                client_id
            ),
            quiet: false,
            deprecation: true,
        ).ok();
    }

    if let Ok(schedule_type) = SandcastleScheduleType::new() {
        if let Some(schedule_type_str) = schedule_type.as_str() {
            metadata.insert("schedule_type".to_owned(), schedule_type_str.to_owned());
        }
    }

    data.data.set(ConfigMetadataHolder(metadata));
}

impl Drop for ServerCommandContext<'_> {
    fn drop(&mut self) {
        // Ensure we cancel the heartbeat guard first.
        std::mem::drop(self.heartbeat_guard_handle.take());
    }
}

#[async_trait]
impl ServerCommandContextTrait for ServerCommandContext<'_> {
    fn working_dir(&self) -> &ProjectRelativePath {
        &self.working_dir
    }

    fn working_dir_abs(&self) -> &AbsWorkingDir {
        &self.working_dir_abs
    }

    fn command_name(&self) -> &str {
        &self.command_name
    }

    fn isolation_prefix(&self) -> &FileName {
        &self.isolation_prefix
    }

    fn cert_state(&self) -> CertState {
        self.cert_state.dupe()
    }

    fn project_root(&self) -> &ProjectRoot {
        &self.base_context.project_root
    }

    fn materializer(&self) -> Arc<dyn Materializer> {
        self.base_context.daemon.materializer.dupe()
    }

    /// Provides a DiceTransaction, initialized on first use and shared after initialization.
    async fn dice_accessor<'s>(
        &'s self,
        _private: PrivateStruct,
    ) -> buck2_error::Result<DiceAccessor<'s>> {
        let (build_signals_installer, deferred_build_signals) = create_build_signals();

        let is_nested_invocation = if let Some(uuid) = &self.daemon_uuid_from_client {
            uuid == &self.base_context.daemon.daemon_id.to_string()
        } else {
            false
        };

        Ok(DiceAccessor {
            dice_handler: self.base_context.daemon.dice_manager.dupe(),
            setup: Box::new(self.dice_updater(build_signals_installer).await?),
            is_nested_invocation,
            sanitized_argv: self.sanitized_argv.clone(),
            preemptible: self.preemptible,
            build_signals: deferred_build_signals,
            exit_when: self.exit_when,
        })
    }

    fn events(&self) -> &EventDispatcher {
        &self.base_context.events
    }

    fn previous_command_data(&self) -> Arc<LockedPreviousCommandData> {
        self.base_context.daemon.previous_command_data.clone()
    }

    fn stderr(&self) -> buck2_error::Result<StderrOutputGuard<'_>> {
        Ok(StderrOutputGuard {
            _phantom: PhantomData,
            inner: BufWriter::with_capacity(
                // TODO(nga): no need to buffer here.
                4096,
                StderrOutputWriter::new(self)?,
            ),
        })
    }

    /// Create command start event with metadata
    async fn command_start_event(
        &self,
        data: buck2_data::command_start::Data,
    ) -> buck2_error::Result<buck2_data::CommandStart> {
        Ok(buck2_data::CommandStart {
            metadata: self.request_metadata().await?,
            data: Some(data),
            cli_args: self.sanitized_argv.clone(),
            tags: self.base_context.daemon.tags.clone(),
        })
    }

    /// Gathers metadata to attach to events for when a command starts and stops.
    async fn request_metadata(&self) -> buck2_error::Result<HashMap<String, String>> {
        // Facebook only: metadata collection for Scribe writes
        facebook_only();

        let mut metadata = metadata::collect(&self.base_context.daemon.daemon_id);

        metadata.insert(
            "io_provider".to_owned(),
            self.base_context.daemon.io.name().to_owned(),
        );

        metadata.insert(
            "materializer".to_owned(),
            self.base_context.daemon.materializer.name().to_owned(),
        );

        if let Some(oncall) = &self.oncall {
            metadata.insert("oncall".to_owned(), oncall.clone());
        }

        if let Some(client_id_from_client_metadata) = &self.client_id_from_client_metadata {
            metadata.insert("client".to_owned(), client_id_from_client_metadata.clone());
        }

        metadata.insert(
            "vpnless".to_owned(),
            self.base_context
                .daemon
                .http_client
                .supports_vpnless()
                .to_string(),
        );

        metadata.insert(
            "http_versions".to_owned(),
            match self.base_context.daemon.http_client.http2() {
                true => "1,2",
                false => "1",
            }
            .to_owned(),
        );

        Ok(metadata)
    }

    /// Gathers metadata from buckconfig to attach to events for when a command enters the critical
    /// section
    async fn config_metadata(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<HashMap<String, String>> {
        ctx.per_transaction_data()
            .data
            .get::<ConfigMetadataHolder>()
            .map(|holder| holder.0.clone())
            .map_err(|_| buck2_error::internal_error!("Config metadata not set"))
    }

    fn log_target_pattern(
        &self,
        providers_patterns: &[ParsedPattern<ConfiguredProvidersPatternExtra>],
    ) {
        let patterns = providers_patterns.map(|pat| buck2_data::TargetPattern {
            value: format!("{pat}"),
        });

        self.events()
            .instant_event(buck2_data::ParsedTargetPatterns {
                target_patterns: patterns,
            })
    }

    fn log_target_pattern_with_modifiers(
        &self,
        providers_patterns_with_modifiers: &[ParsedPatternWithModifiers<
            ConfiguredProvidersPatternExtra,
        >],
    ) {
        let seen_values = BTreeSet::from_iter(
            providers_patterns_with_modifiers.map(|pat| format!("{}", pat.parsed_pattern)),
        );

        let patterns = seen_values
            .into_iter()
            .map(|pat| buck2_data::TargetPattern { value: pat })
            .collect();

        self.events()
            .instant_event(buck2_data::ParsedTargetPatterns {
                target_patterns: patterns,
            })
    }

    fn cancellation_context(&self) -> &CancellationContext {
        self.cancellations
    }

    fn command_start(&self) -> Instant {
        self.command_start
    }
}
