/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::BufWriter;
use std::marker::PhantomData;
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::actions::build_listener::BuildSignalSender;
use buck2_build_api::actions::build_listener::SetBuildSignals;
use buck2_build_api::actions::run::knobs::HasRunActionKnobs;
use buck2_build_api::actions::run::knobs::RunActionKnobs;
use buck2_build_api::context::SetBuildContextData;
use buck2_build_api::execute::blocking::BlockingExecutor;
use buck2_build_api::execute::blocking::SetBlockingExecutor;
use buck2_build_api::execute::commands::dice_data::set_fallback_executor_config;
use buck2_build_api::execute::commands::dice_data::SetCommandExecutor;
use buck2_build_api::execute::commands::re::manager::ReConnectionHandle;
use buck2_build_api::execute::commands::re::manager::ReConnectionManager;
use buck2_build_api::execute::commands::re::manager::ReConnectionObserver;
use buck2_build_api::execute::commands::re::ReExecutorGlobalKnobs;
use buck2_build_api::execute::materializer::Materializer;
use buck2_build_api::execute::materializer::SetMaterializer;
use buck2_build_api::interpreter::context::configure_build_file_globals;
use buck2_build_api::interpreter::context::configure_extension_file_globals;
use buck2_build_api::interpreter::context::prelude_path;
use buck2_build_api::interpreter::context::BuildInterpreterConfiguror;
use buck2_build_api::spawner::BuckSpawner;
use buck2_bxl::bxl::starlark_defs::configure_bxl_file_globals;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::io::IoProvider;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::result::SharedResult;
use buck2_core::async_once_cell::AsyncOnceCell;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::rollout_percentage::RolloutPercentage;
use buck2_forkserver::client::ForkserverClient;
use buck2_interpreter::dice::interpreter_setup::setup_interpreter;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_node::execute::config::CommandExecutorConfig;
use buck2_node::execute::config::CommandExecutorKind;
use buck2_node::execute::config::LocalExecutorOptions;
use cli_proto::client_context::HostPlatformOverride;
use cli_proto::common_build_options::ExecutionStrategy;
use cli_proto::ClientContext;
use cli_proto::CommonBuildOptions;
use cli_proto::ConfigOverride;
use dice::data::DiceData;
use dice::Dice;
use dice::DiceTransaction;
use dice::UserComputationData;
use events::dispatch::EventDispatcher;
use gazebo::dupe::Dupe;
use gazebo::prelude::VecExt;
use host_sharing::HostSharingBroker;
use host_sharing::HostSharingStrategy;

use crate::configs::parse_legacy_cells;
use crate::daemon::common::get_executor_config_for_strategy;
use crate::daemon::common::parse_concurrency;
use crate::daemon::common::parse_patterns_from_cli_args;
use crate::daemon::common::CommandExecutorFactory;
use crate::daemon::server::active_commands::ActiveCommandDropGuard;
use crate::daemon::server::dice_tracker::BuckDiceTracker;
use crate::daemon::server::file_watcher::FileWatcher;
use crate::daemon::server::heartbeat_guard::HeartbeatGuard;
use crate::daemon::server::host_info;
use crate::daemon::server::raw_output::RawOuputGuard;
use crate::daemon::server::raw_output::RawOutputWriter;

#[derive(Debug, thiserror::Error)]
enum DaemonCommunicationError {
    #[error("Got invalid working directory `{0}`")]
    InvalidWorkingDirectory(String),
}

/// BaseCommandContext provides access to the global daemon state and information specific to a command (like the
/// EventDispatcher). Most commands use a ServerCommandContext which has more command/client-specific information.
pub(crate) struct BaseServerCommandContext {
    /// An fbinit token for using things that require fbinit. fbinit is initialized on daemon startup.
    pub _fb: fbinit::FacebookInit,
    /// Absolute path to the project root.
    pub project_root: AbsPathBuf,
    /// A reference to the dice graph. Most interesting things are accessible from this (and new interesting things should be
    /// added there rather than as fields here). This has some per-request setup done already (like attaching a per-request
    /// event dispatcher).
    pub(crate) dice: Arc<Dice>,
    /// A reference to the I/O provider.
    pub(crate) io: Arc<dyn IoProvider>,
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
    pub(crate) _drop_guard: ActiveCommandDropGuard,
    /// The file watcher that keeps buck2 up to date with disk changes.
    pub(crate) file_watcher: Arc<dyn FileWatcher>,
}

impl BaseServerCommandContext {
    pub(crate) fn file_system(&self) -> ProjectFilesystem {
        ProjectFilesystem::new(self.project_root.clone())
    }

    /// Provides a DiceComputations. This may be missing some data or injected keys that
    /// we normally expect. To get a full dice context, use a ServerCommandContext.
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
                CommandExecutorConfig::new_with_default_path_separator(CommandExecutorKind::Local(
                    LocalExecutorOptions {},
                )),
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
pub(crate) struct ServerCommandContext {
    pub(crate) base_context: BaseServerCommandContext,

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
    pub starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,

    record_target_call_stacks: bool,
    disable_starlark_types: bool,

    buck_out_dir: ProjectRelativePathBuf,

    /// Common build options associated with this command.
    build_options: Option<CommonBuildOptions>,

    /// The DiceTransaction to use when servicing computations triggered by this command.
    dice: AsyncOnceCell<SharedResult<DiceTransaction>>,

    /// Keep emitting heartbeat events while the ServerCommandContext is alive  We put this in an
    /// Option so that we can ensure heartbeat events are cancelled before everything else is
    /// dropped.
    heartbeat_guard_handle: Option<HeartbeatGuard>,
}

impl ServerCommandContext {
    pub(crate) fn new(
        base_context: BaseServerCommandContext,
        client_context: &ClientContext,
        build_signals: BuildSignalSender,
        starlark_profiler_instrumentation_override: StarlarkProfilerConfiguration,
        build_options: Option<&CommonBuildOptions>,
        buck_out_dir: ProjectRelativePathBuf,
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

        let heartbeat_guard_handle = HeartbeatGuard::new(&base_context);

        Ok(ServerCommandContext {
            base_context,
            working_dir: project_path.to_buf().into(),
            config_overrides: client_context.config_overrides.clone(),
            host_platform_override: client_context.host_platform(),
            oncall,
            _re_connection_handle: re_connection_handle,
            build_signals,
            starlark_profiler_instrumentation_override,
            buck_out_dir,
            build_options: build_options.cloned(),
            dice: AsyncOnceCell::new(),
            record_target_call_stacks,
            disable_starlark_types: client_context.disable_starlark_types,
            heartbeat_guard_handle: Some(heartbeat_guard_handle),
        })
    }

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
    pub(crate) async fn canonicalize_patterns_for_logging(
        &self,
        patterns: &[buck2_data::TargetPattern],
    ) -> anyhow::Result<Vec<buck2_data::TargetPattern>> {
        let dice_txn = self.dice_ctx().await?;
        let providers_patterns = parse_patterns_from_cli_args::<ProvidersPattern>(
            patterns,
            &dice_txn.get_cell_resolver().await?,
            &dice_txn.get_legacy_configs().await?,
            &self.working_dir,
        )?;
        let patterns = providers_patterns.into_map(|pat| buck2_data::TargetPattern {
            value: format!("{}", pat),
        });

        Ok(patterns)
    }

    /// Provides a DiceTransaction, initialized on first use and shared after initialization.
    pub(crate) async fn dice_ctx(&self) -> SharedResult<DiceTransaction> {
        self.dice
            .get_or_init(self.construct_dice_ctx())
            .await
            .dupe()
    }

    async fn construct_dice_ctx(&self) -> SharedResult<DiceTransaction> {
        let execution_strategy = self
            .build_options
            .as_ref()
            .map(|opts| opts.execution_strategy)
            .map_or(ExecutionStrategy::LocalOnly, |strategy| {
                ExecutionStrategy::from_i32(strategy).expect("execution strategy should be valid")
            });

        let fs = self.file_system();
        let cwd = &self.working_dir;
        let (cell_resolver, legacy_configs) =
            parse_legacy_cells(self.config_overrides.iter(), &fs.resolve(cwd), &fs)?;
        // TODO(cjhopman): The CellResolver and the legacy configs shouldn't be leaves on the graph. This should
        // just be setting the config overrides and host platform override as leaves on the graph.
        let (interpreter_platform, interpreter_architecture) =
            host_info::get_host_info(self.host_platform_override);
        let configuror = BuildInterpreterConfiguror::new(
            Some(prelude_path(&cell_resolver)),
            interpreter_platform,
            interpreter_architecture,
            self.record_target_call_stacks,
            configure_build_file_globals,
            configure_extension_file_globals,
            configure_bxl_file_globals,
        );

        let root_config = legacy_configs
            .get(cell_resolver.root_cell())
            .context("No config for root cell")?;

        let hash_all_commands = root_config
            .parse::<RolloutPercentage>("buck2", "hash_all_commands")?
            .unwrap_or_else(RolloutPercentage::never)
            .roll();

        let mut run_action_knobs = RunActionKnobs {
            hash_all_commands,
            ..Default::default()
        };

        if let Some(build_options) = self.build_options.as_ref() {
            run_action_knobs.eager_dep_files = build_options.eager_dep_files;
        }

        let config_threads = root_config.parse("build", "threads")?.unwrap_or(0);

        let concurrency = self
            .build_options
            .as_ref()
            .and_then(|opts| opts.concurrency.as_ref())
            .map_or_else(
                || parse_concurrency(config_threads),
                |obj| parse_concurrency(obj.concurrency),
            )?;

        let re_global_knobs = {
            ReExecutorGlobalKnobs {
                always_check_ttls: root_config
                    .parse("buck2", "re_always_check_ttls")?
                    .unwrap_or(false),
            }
        };

        let executor_config =
            get_executor_config_for_strategy(execution_strategy, self.host_platform_override);
        let blocking_executor: Arc<_> = self.base_context.blocking_executor.dupe();
        let materializer = self.base_context.materializer.dupe();
        let re_connection = self.get_re_connection();
        let build_signals = self.build_signals.dupe();
        let host_sharing_broker =
            HostSharingBroker::new(HostSharingStrategy::SmallerTasksFirst, concurrency);
        let forkserver = self.base_context.forkserver.dupe();

        let upload_all_actions = self
            .build_options
            .as_ref()
            .map_or(false, |opts| opts.upload_all_actions);

        let dice_ctx = self
            .base_context
            .unsafe_dice_ctx_with_more_data(move |mut data| {
                set_fallback_executor_config(&mut data.data, executor_config);
                data.set_command_executor(box CommandExecutorFactory::new(
                    re_connection,
                    host_sharing_broker,
                    materializer.dupe(),
                    blocking_executor.dupe(),
                    execution_strategy,
                    re_global_knobs,
                    upload_all_actions,
                    forkserver,
                ));
                data.set_blocking_executor(blocking_executor);
                data.set_materializer(materializer);
                data.set_build_signals(build_signals);
                data.set_run_action_knobs(run_action_knobs);
                data.spawner = Arc::new(BuckSpawner::default());
                data
            });

        // this sync call my clear the dice ctx, but that's okay as we reset everything below.
        let dice_ctx = self
            .base_context
            .file_watcher
            .sync(dice_ctx, &self.base_context.events)
            .await?;

        dice_ctx.set_buck_out_path(Some(self.buck_out_dir.clone()))?;

        setup_interpreter(
            &dice_ctx,
            cell_resolver,
            configuror,
            legacy_configs,
            self.starlark_profiler_instrumentation_override.dupe(),
            self.disable_starlark_types,
        )?;

        self.base_context
            .events
            .instant_event(buck2_data::TagEvent {
                tags: vec![format!("hash-all-commands:{}", hash_all_commands)],
            });

        Ok(dice_ctx.commit())
    }

    pub(crate) fn file_system(&self) -> ProjectFilesystem {
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

impl Drop for ServerCommandContext {
    fn drop(&mut self) {
        // Ensure we cancel the heartbeat guard first.
        std::mem::drop(self.heartbeat_guard_handle.take());
    }
}
