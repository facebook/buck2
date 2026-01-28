/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;
use std::sync::OnceLock;

use buck2_build_api::actions::execute::dice_data::CommandExecutorResponse;
use buck2_build_api::actions::execute::dice_data::HasCommandExecutor;
use buck2_cli_proto::client_context::HostPlatformOverride;
use buck2_cli_proto::common_build_options::ExecutionStrategy;
use buck2_core::buck2_env;
use buck2_core::execution_types::executor_config::CacheUploadBehavior;
use buck2_core::execution_types::executor_config::CommandExecutorConfig;
use buck2_core::execution_types::executor_config::CommandGenerationOptions;
use buck2_core::execution_types::executor_config::Executor;
use buck2_core::execution_types::executor_config::HybridExecutionLevel;
use buck2_core::execution_types::executor_config::LocalExecutorOptions;
use buck2_core::execution_types::executor_config::MetaInternalExtraParams;
use buck2_core::execution_types::executor_config::PathSeparatorKind;
use buck2_core::execution_types::executor_config::ReGangWorker;
use buck2_core::execution_types::executor_config::RePlatformFields;
use buck2_core::execution_types::executor_config::RemoteEnabledExecutor;
use buck2_core::execution_types::executor_config::RemoteEnabledExecutorOptions;
use buck2_core::execution_types::executor_config::RemoteExecutorDependency;
use buck2_core::execution_types::executor_config::RemoteExecutorOptions;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::project::ProjectRoot;
use buck2_error::BuckErrorContext;
use buck2_events::daemon_id::DaemonId;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::cache_uploader::NoOpCacheUploader;
use buck2_execute::execute::cache_uploader::force_cache_upload;
use buck2_execute::execute::prepared::NoOpCommandOptionalExecutor;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::prepared::PreparedCommandOptionalExecutor;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_execute::re::manager::ReConnectionHandle;
use buck2_execute::re::output_trees_download_config::OutputTreesDownloadConfig;
use buck2_execute_impl::executors::action_cache::ActionCacheChecker;
use buck2_execute_impl::executors::action_cache::RemoteDepFileCacheChecker;
use buck2_execute_impl::executors::action_cache_upload_permission_checker::ActionCacheUploadPermissionChecker;
use buck2_execute_impl::executors::caching::CacheUploader;
use buck2_execute_impl::executors::hybrid::FallbackTracker;
use buck2_execute_impl::executors::hybrid::HybridExecutor;
use buck2_execute_impl::executors::local::ForkserverAccess;
use buck2_execute_impl::executors::local::LocalExecutor;
use buck2_execute_impl::executors::re::ReExecutor;
use buck2_execute_impl::executors::stacked::StackedExecutor;
use buck2_execute_impl::executors::to_re_platform::RePlatformFieldsToRePlatform;
use buck2_execute_impl::executors::worker::WorkerPool;
use buck2_execute_impl::low_pass_filter::LowPassFilter;
use buck2_execute_impl::re::paranoid_download::ParanoidDownloader;
use buck2_execute_impl::sqlite::incremental_state_db::IncrementalDbState;
use buck2_resource_control::memory_tracker::MemoryTrackerHandle;
use dupe::Dupe;
use host_sharing::HostSharingBroker;

/// For each buck invocations, we'll have a single CommandExecutorFactory. This contains shared
/// state used by all command executor strategies.
pub struct CommandExecutorFactory {
    re_connection: Arc<ReConnectionHandle>,
    // TODO(cjhopman): This should probably be a global limit, otherwise simultaneous commands may
    // use more resources than intended (this might no longer be accurate since only instances
    // sharing the same DICE context should be allowed to proceed concurrently, and we only have
    // one CommandExecutorFactory per DICE context).
    host_sharing_broker: Arc<HostSharingBroker>,
    low_pass_filter: Arc<LowPassFilter>,
    materializer: Arc<dyn Materializer>,
    blocking_executor: Arc<dyn BlockingExecutor>,
    strategy: ExecutionStrategy,
    executor_global_knobs: ExecutorGlobalKnobs,
    upload_all_actions: bool,
    forkserver: ForkserverAccess,
    skip_cache_read: bool,
    skip_cache_write: bool,
    project_root: ProjectRoot,
    worker_pool: Arc<WorkerPool>,
    paranoid: Option<ParanoidDownloader>,
    materialize_failed_inputs: bool,
    materialize_failed_outputs: bool,
    /// Cache permission checks per command.
    cache_upload_permission_checker: Arc<ActionCacheUploadPermissionChecker>,
    fallback_tracker: Arc<FallbackTracker>,
    re_use_case_override: Option<RemoteExecutorUseCase>,
    memory_tracker: Option<MemoryTrackerHandle>,
    incremental_db_state: Arc<IncrementalDbState>,
    deduplicate_get_digests_ttl_calls: bool,
    output_trees_download_config: OutputTreesDownloadConfig,
    daemon_id: DaemonId,
}

impl CommandExecutorFactory {
    pub fn new(
        re_connection: Arc<ReConnectionHandle>,
        host_sharing_broker: HostSharingBroker,
        low_pass_filter: LowPassFilter,
        materializer: Arc<dyn Materializer>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        strategy: ExecutionStrategy,
        executor_global_knobs: ExecutorGlobalKnobs,
        upload_all_actions: bool,
        forkserver: ForkserverAccess,
        skip_cache_read: bool,
        skip_cache_write: bool,
        project_root: ProjectRoot,
        worker_pool: Arc<WorkerPool>,
        paranoid: Option<ParanoidDownloader>,
        materialize_failed_inputs: bool,
        materialize_failed_outputs: bool,
        re_use_case_override: Option<RemoteExecutorUseCase>,
        memory_tracker: Option<MemoryTrackerHandle>,
        incremental_db_state: Arc<IncrementalDbState>,
        deduplicate_get_digests_ttl_calls: bool,
        output_trees_download_config: OutputTreesDownloadConfig,
        daemon_id: DaemonId,
    ) -> Self {
        let cache_upload_permission_checker = Arc::new(ActionCacheUploadPermissionChecker::new());

        Self {
            re_connection,
            host_sharing_broker: Arc::new(host_sharing_broker),
            low_pass_filter: Arc::new(low_pass_filter),
            materializer,
            blocking_executor,
            strategy,
            executor_global_knobs,
            upload_all_actions,
            forkserver,
            skip_cache_read,
            skip_cache_write,
            project_root,
            worker_pool,
            paranoid,
            materialize_failed_inputs,
            materialize_failed_outputs,
            cache_upload_permission_checker,
            fallback_tracker: Arc::new(FallbackTracker::new()),
            re_use_case_override,
            memory_tracker,
            incremental_db_state,
            deduplicate_get_digests_ttl_calls,
            output_trees_download_config,
            daemon_id,
        }
    }

    fn get_prepared_re_client(
        &self,
        use_case: RemoteExecutorUseCase,
    ) -> ManagedRemoteExecutionClient {
        let use_case = self.re_use_case_override.unwrap_or(use_case);
        self.re_connection.get_client().with_use_case(use_case)
    }
}

impl HasCommandExecutor for CommandExecutorFactory {
    fn get_command_executor(
        &self,
        artifact_fs: &ArtifactFs,
        executor_config: &CommandExecutorConfig,
    ) -> buck2_error::Result<CommandExecutorResponse> {
        // 30GB is the max RE can currently support.
        const DEFAULT_RE_MAX_INPUT_FILE_BYTES: u64 = 30 * 1024 * 1024 * 1024;

        let local_executor_new = |options: &LocalExecutorOptions| {
            let worker_pool = if options.use_persistent_workers {
                Some(self.worker_pool.dupe())
            } else {
                None
            };
            LocalExecutor::new(
                artifact_fs.clone(),
                self.materializer.dupe(),
                self.incremental_db_state.dupe(),
                self.blocking_executor.dupe(),
                self.host_sharing_broker.dupe(),
                self.project_root.root().to_owned(),
                self.forkserver.dupe(),
                self.executor_global_knobs.dupe(),
                worker_pool,
                self.memory_tracker.dupe(),
                self.daemon_id.dupe(),
            )
        };

        if !buck2_core::is_open_source() && !cfg!(fbcode_build) {
            static WARN: OnceLock<()> = OnceLock::new();
            WARN.get_or_init(|| {
                tracing::warn!("Cargo build detected: disabling remote execution and caching!")
            });

            if self.strategy.ban_local() {
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "The desired execution strategy (`{:?}`) is incompatible with the local executor",
                    self.strategy,
                ));
            }

            return Ok(CommandExecutorResponse {
                executor: Arc::new(local_executor_new(&LocalExecutorOptions::default())),
                platform: Default::default(),
                action_cache_checker: Arc::new(NoOpCommandOptionalExecutor {}),
                remote_dep_file_cache_checker: Arc::new(NoOpCommandOptionalExecutor {}),
                cache_uploader: Arc::new(NoOpCacheUploader {}),
                output_trees_download_config: self.output_trees_download_config.dupe(),
            });
        }

        let remote_executor_new =
            |options: &RemoteExecutorOptions,
             re_use_case: &RemoteExecutorUseCase,
             re_action_key: &Option<String>,
             remote_cache_enabled: bool,
             dependencies: &[RemoteExecutorDependency],
             gang_workers: &[ReGangWorker]| {
                ReExecutor {
                    artifact_fs: artifact_fs.clone(),
                    project_fs: self.project_root.clone(),
                    materializer: self.materializer.dupe(),
                    incremental_db_state: self.incremental_db_state.dupe(),
                    re_client: self.get_prepared_re_client(*re_use_case),
                    re_action_key: re_action_key.clone(),
                    re_max_queue_time: options.re_max_queue_time,
                    re_resource_units: options.re_resource_units,
                    knobs: self.executor_global_knobs.dupe(),
                    skip_cache_read: self.skip_cache_read || !remote_cache_enabled,
                    skip_cache_write: self.skip_cache_write || !remote_cache_enabled,
                    paranoid: self.paranoid.dupe(),
                    materialize_failed_inputs: self.materialize_failed_inputs,
                    materialize_failed_outputs: self.materialize_failed_outputs,
                    dependencies: dependencies.to_vec(),
                    gang_workers: gang_workers.to_vec(),
                    deduplicate_get_digests_ttl_calls: self.deduplicate_get_digests_ttl_calls,
                    output_trees_download_config: self.output_trees_download_config.dupe(),
                }
            };

        let response = match &executor_config.executor {
            Executor::None => None,
            Executor::Local(local) => {
                if self.strategy.ban_local() {
                    None
                } else {
                    Some(CommandExecutorResponse {
                        executor: Arc::new(local_executor_new(local)),
                        platform: Default::default(),
                        action_cache_checker: Arc::new(NoOpCommandOptionalExecutor {}),
                        remote_dep_file_cache_checker: Arc::new(NoOpCommandOptionalExecutor {}),
                        cache_uploader: Arc::new(NoOpCacheUploader {}),
                        output_trees_download_config: self.output_trees_download_config.dupe(),
                    })
                }
            }
            Executor::RemoteEnabled(remote_options) => {
                // NOTE: While we now have a legit flag for this, we keep the env var. This has been used
                // in remediating prod incidents in the past, and this is the kind of thing that can easily
                // become tribal knowledge. Keeping this does not hurt us.
                let disable_caching =
                    buck2_env!("BUCK2_TEST_DISABLE_CACHING", type=bool, applicability=testing)?
                        .unwrap_or(self.skip_cache_read);

                let disable_caching = disable_caching
                    || (!remote_options.remote_cache_enabled
                        && !remote_options.remote_dep_file_cache_enabled);

                // This is for test only as in real life, it would be silly to only use the remote dep file cache and not the regular cache
                // This will only do anything if cache is not disabled and remote dep file cache is enabled
                let only_remote_dep_file_cache = buck2_env!(
                    "BUCK2_TEST_ONLY_REMOTE_DEP_FILE_CACHE",
                    bool,
                    applicability = testing
                )?;

                let cache_checker_new = || -> (Arc<dyn PreparedCommandOptionalExecutor>, Arc<dyn PreparedCommandOptionalExecutor>) {
                    if disable_caching {
                        return (
                            Arc::new(NoOpCommandOptionalExecutor {}) as _,
                            Arc::new(NoOpCommandOptionalExecutor {}) as _,
                        );
                    }

                    let remote_dep_file_cache_checker: Arc<dyn PreparedCommandOptionalExecutor> =
                        if remote_options.remote_dep_file_cache_enabled {
                            Arc::new(RemoteDepFileCacheChecker {
                                artifact_fs: artifact_fs.clone(),
                                materializer: self.materializer.dupe(),
                                incremental_db_state: self.incremental_db_state.dupe(),
                                re_client: self.get_prepared_re_client(remote_options.re_use_case),
                                re_action_key: remote_options.re_action_key.clone(),
                                upload_all_actions: self.upload_all_actions,
                                knobs: self.executor_global_knobs.dupe(),
                                paranoid: self.paranoid.dupe(),
                                deduplicate_get_digests_ttl_calls: self.deduplicate_get_digests_ttl_calls,
                                output_trees_download_config: self.output_trees_download_config.dupe(),
                            }) as _
                        } else {
                            Arc::new(NoOpCommandOptionalExecutor {}) as _
                        };

                    let action_cache_checker: Arc<dyn PreparedCommandOptionalExecutor> =
                        if only_remote_dep_file_cache {
                            Arc::new(NoOpCommandOptionalExecutor {}) as _
                        } else {
                            Arc::new(ActionCacheChecker {
                                artifact_fs: artifact_fs.clone(),
                                materializer: self.materializer.dupe(),
                                incremental_db_state: self.incremental_db_state.dupe(),
                                re_client: self.get_prepared_re_client(remote_options.re_use_case),
                                re_action_key: remote_options.re_action_key.clone(),
                                upload_all_actions: self.upload_all_actions,
                                knobs: self.executor_global_knobs.dupe(),
                                paranoid: self.paranoid.dupe(),
                                deduplicate_get_digests_ttl_calls: self.deduplicate_get_digests_ttl_calls,
                                output_trees_download_config: self.output_trees_download_config.dupe(),
                            }) as _
                        };

                    (action_cache_checker, remote_dep_file_cache_checker)
                };

                let executor: Option<Arc<dyn PreparedCommandExecutor>> =
                    match &remote_options.executor {
                        RemoteEnabledExecutor::Local(local) if !self.strategy.ban_local() => {
                            Some(Arc::new(local_executor_new(local)))
                        }
                        RemoteEnabledExecutor::Remote(remote) if !self.strategy.ban_remote() => {
                            Some(Arc::new(remote_executor_new(
                                remote,
                                &remote_options.re_use_case,
                                &remote_options.re_action_key,
                                remote_options.remote_cache_enabled,
                                &remote_options.dependencies,
                                &remote_options.gang_workers,
                            )))
                        }
                        RemoteEnabledExecutor::Hybrid {
                            local,
                            remote,
                            level,
                        } if !self.strategy.ban_hybrid() => {
                            let re_max_input_files_bytes = remote
                                .re_max_input_files_bytes
                                .unwrap_or(DEFAULT_RE_MAX_INPUT_FILE_BYTES);
                            let local = local_executor_new(local);
                            let remote = remote_executor_new(
                                remote,
                                &remote_options.re_use_case,
                                &remote_options.re_action_key,
                                remote_options.remote_cache_enabled,
                                &remote_options.dependencies,
                                &remote_options.gang_workers,
                            );
                            let executor_preference = self.strategy.hybrid_preference();
                            let low_pass_filter = self.low_pass_filter.dupe();
                            let fallback_tracker = self.fallback_tracker.dupe();

                            if self.paranoid.is_some() {
                                let executor_preference = executor_preference
                                    .and(ExecutorPreference::DefaultErasePreferences)?;

                                let (action_cache_checker, remote_dep_file_cache_checker) =
                                    cache_checker_new();
                                Some(Arc::new(HybridExecutor {
                                    local,
                                    remote: StackedExecutor {
                                        optional1: action_cache_checker,
                                        optional2: remote_dep_file_cache_checker,
                                        fallback: remote,
                                    },
                                    level: HybridExecutionLevel::Full {
                                        fallback_on_failure: true,
                                        low_pass_filter: false,
                                    },
                                    executor_preference,
                                    re_max_input_files_bytes,
                                    low_pass_filter,
                                    fallback_tracker,
                                }))
                            } else {
                                Some(Arc::new(HybridExecutor {
                                    local,
                                    remote,
                                    level: *level,
                                    executor_preference,
                                    re_max_input_files_bytes,
                                    low_pass_filter,
                                    fallback_tracker,
                                }))
                            }
                        }
                        _ => None,
                    };

                let (action_cache_checker, remote_dep_file_cache_checker) =
                    if self.paranoid.is_some() {
                        (
                            Arc::new(NoOpCommandOptionalExecutor {}) as _,
                            Arc::new(NoOpCommandOptionalExecutor {}) as _,
                        )
                    } else {
                        cache_checker_new()
                    };

                let cache_uploader = if force_cache_upload()? {
                    Arc::new(CacheUploader::new(
                        artifact_fs.clone(),
                        self.materializer.dupe(),
                        self.get_prepared_re_client(remote_options.re_use_case),
                        remote_options.re_properties.clone(),
                        None,
                        self.cache_upload_permission_checker.dupe(),
                        self.deduplicate_get_digests_ttl_calls,
                    )) as _
                } else if disable_caching {
                    Arc::new(NoOpCacheUploader {}) as _
                } else if let CacheUploadBehavior::Enabled { max_bytes } =
                    remote_options.cache_upload_behavior
                {
                    Arc::new(CacheUploader::new(
                        artifact_fs.clone(),
                        self.materializer.dupe(),
                        self.get_prepared_re_client(remote_options.re_use_case),
                        remote_options.re_properties.clone(),
                        max_bytes,
                        self.cache_upload_permission_checker.dupe(),
                        self.deduplicate_get_digests_ttl_calls,
                    )) as _
                } else {
                    Arc::new(NoOpCacheUploader {}) as _
                };

                executor.map(|executor| CommandExecutorResponse {
                    executor,
                    platform: remote_options.re_properties.to_re_platform(),
                    action_cache_checker,
                    remote_dep_file_cache_checker,
                    cache_uploader,
                    output_trees_download_config: self.output_trees_download_config.dupe(),
                })
            }
        };

        let response = response
            .with_buck_error_context(|| format!("The desired execution strategy (`{:?}`) is incompatible with the executor config that was selected: {:?}", self.strategy, executor_config)).tag(buck2_error::ErrorTag::Input)?;

        Ok(response)
    }
}

trait ExecutionStrategyExt {
    fn ban_local(&self) -> bool;
    fn ban_remote(&self) -> bool;
    fn ban_hybrid(&self) -> bool;
    fn hybrid_preference(&self) -> ExecutorPreference;
}

impl ExecutionStrategyExt for ExecutionStrategy {
    fn ban_local(&self) -> bool {
        match self {
            Self::RemoteOnly | Self::NoExecution => true,
            _ => false,
        }
    }

    fn ban_remote(&self) -> bool {
        match self {
            Self::LocalOnly | Self::NoExecution => true,
            _ => false,
        }
    }

    fn ban_hybrid(&self) -> bool {
        match self {
            Self::NoExecution => true,
            _ => false,
        }
    }

    fn hybrid_preference(&self) -> ExecutorPreference {
        match self {
            Self::HybridPreferLocal => ExecutorPreference::LocalPreferred,
            Self::HybridPreferRemote => ExecutorPreference::RemotePreferred,
            Self::LocalOnly => ExecutorPreference::LocalRequired,
            Self::RemoteOnly => ExecutorPreference::RemoteRequired,
            _ => ExecutorPreference::Default,
        }
    }
}

/// This is used when execution platforms are not configured.
pub fn get_default_executor_config(host_platform: HostPlatformOverride) -> CommandExecutorConfig {
    let executor = if buck2_core::is_open_source() {
        Executor::Local(LocalExecutorOptions::default())
    } else {
        Executor::RemoteEnabled(RemoteEnabledExecutorOptions {
            executor: RemoteEnabledExecutor::Hybrid {
                local: LocalExecutorOptions::default(),
                remote: RemoteExecutorOptions::default(),
                level: HybridExecutionLevel::Limited,
            },
            re_properties: get_default_re_properties(host_platform),
            re_use_case: RemoteExecutorUseCase::buck2_default(),
            re_action_key: None,
            cache_upload_behavior: CacheUploadBehavior::Disabled,
            remote_cache_enabled: true,
            remote_dep_file_cache_enabled: false,
            dependencies: vec![],
            gang_workers: vec![],
            custom_image: None,
            meta_internal_extra_params: MetaInternalExtraParams::default(),
        })
    };

    CommandExecutorConfig {
        executor,
        options: CommandGenerationOptions {
            path_separator: get_default_path_separator(host_platform),
            output_paths_behavior: Default::default(),
            use_bazel_protocol_remote_persistent_workers: false,
        },
    }
}

fn get_default_re_properties(host_platform: HostPlatformOverride) -> RePlatformFields {
    let linux = &[("platform", "linux-remote-execution")];
    let macos = &[("platform", "mac"), ("subplatform", "any")];
    let windows = &[("platform", "windows")];

    let props = match host_platform {
        HostPlatformOverride::Linux => linux.as_slice(),
        HostPlatformOverride::MacOs => macos.as_slice(),
        HostPlatformOverride::Windows => windows.as_slice(),
        HostPlatformOverride::DefaultPlatform => match std::env::consts::OS {
            "linux" => linux.as_slice(),
            "macos" => macos.as_slice(),
            "windows" => windows.as_slice(),
            v => unimplemented!("no support yet for operating system `{}`", v),
        },
    };

    RePlatformFields {
        properties: Arc::new(
            props
                .iter()
                .map(|(k, v)| ((*k).to_owned(), (*v).to_owned()))
                .collect(),
        ),
    }
}

fn get_default_path_separator(host_platform: HostPlatformOverride) -> PathSeparatorKind {
    match host_platform {
        HostPlatformOverride::Linux => PathSeparatorKind::Unix,
        HostPlatformOverride::MacOs => PathSeparatorKind::Unix,
        HostPlatformOverride::Windows => PathSeparatorKind::Windows,
        HostPlatformOverride::DefaultPlatform => PathSeparatorKind::system_default(),
    }
}
