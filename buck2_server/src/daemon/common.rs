/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context as _;
use buck2_common::executor_config::CacheUploadBehavior;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::executor_config::CommandExecutorKind;
use buck2_common::executor_config::HybridExecutionLevel;
use buck2_common::executor_config::LocalExecutorOptions;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::executor_config::RemoteExecutorOptions;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::dice_data::HasCommandExecutor;
use buck2_execute::execute::prepared::PreparedCommandExecutor;
use buck2_execute::execute::request::ExecutorPreference;
use buck2_execute::knobs::ExecutorGlobalKnobs;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::re::manager::ReConnectionHandle;
use buck2_execute_impl::executors::caching::CachingExecutor;
use buck2_execute_impl::executors::hybrid::HybridExecutor;
use buck2_execute_impl::executors::local::LocalExecutor;
use buck2_execute_impl::executors::re::ReExecutionPlatform;
use buck2_execute_impl::executors::re::ReExecutor;
use buck2_execute_impl::low_pass_filter::LowPassFilter;
use buck2_forkserver::client::ForkserverClient;
use cli_proto::client_context::HostPlatformOverride;
use cli_proto::common_build_options::ExecutionStrategy;
use gazebo::prelude::*;
use host_sharing::HostSharingBroker;
use once_cell::sync::OnceCell;

pub fn parse_concurrency(requested: u32) -> anyhow::Result<usize> {
    let mut ret = requested.try_into().context("Invalid concurrency")?;

    if ret == 0 {
        ret = num_cpus::get();
    }

    Ok(ret)
}

/// For each buck invocations, we'll have a single CommandExecutorFactory. This contains shared
/// state used by all command executor strategies.
pub struct CommandExecutorFactory {
    pub re_connection: ReConnectionHandle,
    // TODO(cjhopman): This should probably be a global limit, otherwise simultaneous commands may
    // use more resources than intended (this might no longer be accurate since only instances
    // sharing the same DICE context should be allowed to proceed concurrently, and we only have
    // one CommandExecutorFactory per DICE context).
    pub host_sharing_broker: Arc<HostSharingBroker>,
    pub low_pass_filter: Arc<LowPassFilter>,
    pub materializer: Arc<dyn Materializer>,
    pub blocking_executor: Arc<dyn BlockingExecutor>,
    pub strategy: ExecutionStrategy,
    pub executor_global_knobs: ExecutorGlobalKnobs,
    pub upload_all_actions: bool,
    pub forkserver: Option<ForkserverClient>,
    project_root: ProjectRoot,
}

impl CommandExecutorFactory {
    pub fn new(
        re_connection: ReConnectionHandle,
        host_sharing_broker: HostSharingBroker,
        low_pass_filter: LowPassFilter,
        materializer: Arc<dyn Materializer>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        strategy: ExecutionStrategy,
        executor_global_knobs: ExecutorGlobalKnobs,
        upload_all_actions: bool,
        forkserver: Option<ForkserverClient>,
        project_root: ProjectRoot,
    ) -> Self {
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
            project_root,
        }
    }
}

impl HasCommandExecutor for CommandExecutorFactory {
    fn get_command_executor(
        &self,
        artifact_fs: &ArtifactFs,
        executor_config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn PreparedCommandExecutor>> {
        let local_executor_new = |_options| {
            LocalExecutor::new(
                artifact_fs.clone(),
                self.materializer.dupe(),
                self.blocking_executor.dupe(),
                self.host_sharing_broker.dupe(),
                self.project_root.root().to_owned(),
                self.forkserver.dupe(),
                self.executor_global_knobs.dupe(),
            )
        };

        if !cfg!(fbcode_build) {
            static WARN: OnceCell<()> = OnceCell::new();
            WARN.get_or_init(|| {
                tracing::warn!("Cargo build detected: disabling remote execution and caching!")
            });

            if self.strategy.ban_local() {
                return Err(anyhow::anyhow!(
                    "The desired execution strategy (`{:?}`) is incompatible with the local executor",
                    self.strategy,
                ));
            }

            return Ok(Arc::new(local_executor_new(&LocalExecutorOptions {})));
        }

        let remote_executor_new = |options: &RemoteExecutorOptions| {
            let properties = options
                .re_properties
                .iter()
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();

            // 30GB is the max RE can currently support.
            const DEFAULT_RE_MAX_INPUT_FILE_BYTES: u64 = 30 * 1024 * 1024 * 1024;

            ReExecutor::new(
                artifact_fs.clone(),
                self.project_root.clone(),
                self.materializer.dupe(),
                self.re_connection.get_client(),
                properties,
                options.re_action_key.clone(),
                options
                    .re_max_input_files_bytes
                    .unwrap_or(DEFAULT_RE_MAX_INPUT_FILE_BYTES),
                options.re_use_case,
                self.executor_global_knobs.dupe(),
            )
        };

        let inner_executor: Arc<dyn PreparedCommandExecutor> = match &executor_config.executor_kind
        {
            CommandExecutorKind::Local(local) if !self.strategy.ban_local() => {
                Arc::new(local_executor_new(local))
            }
            CommandExecutorKind::Remote(remote) if !self.strategy.ban_remote() => {
                Arc::new(remote_executor_new(remote))
            }
            CommandExecutorKind::Hybrid {
                local,
                remote,
                level,
            } if !self.strategy.ban_hybrid() => Arc::new(HybridExecutor {
                local: local_executor_new(local),
                remote: remote_executor_new(remote),
                level: *level,
                executor_preference: self.strategy.hybrid_preference(),
                low_pass_filter: self.low_pass_filter.dupe(),
            }),
            config => {
                return Err(anyhow::anyhow!(
                    "The desired execution strategy (`{:?}`) is incompatible with the executor config that was selected: {:?}",
                    self.strategy,
                    config
                ));
            }
        };

        static DISABLE_CACHING: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_DISABLE_CACHING");

        if DISABLE_CACHING.get_copied()?.unwrap_or(false) {
            return Ok(inner_executor);
        }

        Ok(Arc::new(CachingExecutor::new(
            inner_executor,
            artifact_fs.clone(),
            self.materializer.dupe(),
            self.re_connection.get_client(),
            self.upload_all_actions,
            self.executor_global_knobs.dupe(),
            executor_config.cache_upload_behavior,
        )))
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
            Self::RemoteOnly | Self::NoExecution => true,
            _ => false,
        }
    }

    fn hybrid_preference(&self) -> ExecutorPreference {
        match self {
            Self::HybridPreferLocal => ExecutorPreference::LocalPreferred,
            Self::LocalOnly => ExecutorPreference::LocalRequired,
            _ => ExecutorPreference::Default,
        }
    }
}

pub fn get_executor_config_for_strategy(
    strategy: ExecutionStrategy,
    host_platform: HostPlatformOverride,
) -> CommandExecutorConfig {
    let re_execution_platform = get_re_execution_platform(host_platform);
    let executor_kind = match strategy {
        // NOTE: NoExecution here retunrs a default config, which is fine because the filter will
        // kick in later.
        ExecutionStrategy::Default | ExecutionStrategy::NoExecution => {
            CommandExecutorKind::Hybrid {
                local: LocalExecutorOptions {},
                remote: RemoteExecutorOptions {
                    re_properties: re_execution_platform.intrinsic_properties(),
                    ..Default::default()
                },
                level: HybridExecutionLevel::Limited,
            }
        }
        // NOTE: We don't differnetiate between the preferences for Hybrid here. This gets injected
        // later when we actually instantiate the Executor.
        ExecutionStrategy::Hybrid | ExecutionStrategy::HybridPreferLocal => {
            CommandExecutorKind::Hybrid {
                local: LocalExecutorOptions {},
                remote: RemoteExecutorOptions {
                    re_properties: re_execution_platform.intrinsic_properties(),
                    ..Default::default()
                },
                level: HybridExecutionLevel::Limited,
            }
        }
        ExecutionStrategy::LocalOnly => CommandExecutorKind::Local(LocalExecutorOptions {}),
        ExecutionStrategy::RemoteOnly => CommandExecutorKind::Remote(RemoteExecutorOptions {
            re_properties: re_execution_platform.intrinsic_properties(),
            ..Default::default()
        }),
    };

    CommandExecutorConfig {
        executor_kind,
        path_separator: PathSeparatorKind::system_default(),
        cache_upload_behavior: CacheUploadBehavior::Disabled,
    }
}

fn get_re_execution_platform(host_platform: HostPlatformOverride) -> ReExecutionPlatform {
    let linux = ReExecutionPlatform::Linux;
    // TODO(T110757645): The Xcode version should come from the execution platform
    let mac = ReExecutionPlatform::MacOS {
        xcode_version: "14.0".to_owned(),
    };

    let windows = ReExecutionPlatform::Windows;

    match host_platform {
        HostPlatformOverride::Linux => linux,
        HostPlatformOverride::MacOs => mac,
        HostPlatformOverride::Windows => windows,
        HostPlatformOverride::Default => match std::env::consts::OS {
            "linux" => linux,
            "macos" => mac,
            "windows" => windows,
            v => unimplemented!("no support yet for operating system `{}`", v),
        },
    }
}
