/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    convert::{TryFrom, TryInto},
    sync::Arc,
    time::Duration,
};

use anyhow::{anyhow, Context as _};
use buck2_build_api::execute::{
    commands::{
        dice_data::{CommandExecutorConfig, HasCommandExecutor},
        hybrid::HybridExecutor,
        local::LocalExecutor,
        re::{
            cache_check::CacheCheckingExecutor, manager::ReConnectionHandle, ExecutionPlatform,
            ReExecutor,
        },
        PreparedCommandExecutor,
    },
    materializer::Materializer,
    ActionExecutorConfig, LocalExecutorOptions, RemoteExecutorOptions,
};
use buck2_common::{
    dice::cells::HasCellResolver, file_ops::FileOps, legacy_configs::dice::HasLegacyConfigs,
    target_aliases::TargetAliasResolver,
};
use buck2_core::{
    cells::{CellInstance, CellResolver},
    env_helper::EnvHelper,
    fs::project::ProjectRelativePath,
    package::Package,
    target::TargetLabel,
};
use buck2_interpreter::pattern::{
    resolve_target_patterns, ParsedPattern, PatternType, ResolvedPattern,
};
use cli_proto::{
    client_context::HostPlatformOverride, common_build_options::ExecutionStrategy, ClientContext,
};
use dice::DiceTransaction;
use gazebo::prelude::*;
use host_sharing::HostSharingBroker;
use once_cell::sync::OnceCell;
use thiserror::Error;

use crate::daemon::server::ServerCommandContext;

pub trait ToProtoDuration {
    fn to_proto(&self) -> prost_types::Duration;
}

impl ToProtoDuration for Duration {
    fn to_proto(&self) -> prost_types::Duration {
        prost_types::Duration {
            seconds: self.as_secs() as i64,
            nanos: self.subsec_nanos() as i32,
        }
    }
}

pub enum ConfigType {
    Value = 0,
    File = 1,
}

impl TryFrom<i32> for ConfigType {
    type Error = anyhow::Error;

    fn try_from(v: i32) -> anyhow::Result<Self> {
        match v {
            x if x == ConfigType::Value as i32 => Ok(ConfigType::Value),
            x if x == ConfigType::File as i32 => Ok(ConfigType::File),
            _ => Err(anyhow!(
                "Unknown ConfigType enum value `{}` when trying to deserialize",
                v,
            )),
        }
    }
}

#[derive(Debug, Error)]
pub enum CommonCommandError {
    #[error("The working directory was not passed by the client")]
    MissingWorkingDirectory,
}

pub struct PatternParser {
    cell: CellInstance,
    cwd: Package,
    target_alias_resolver: TargetAliasResolver,
}

impl PatternParser {
    pub async fn new(ctx: &DiceTransaction, cwd: &ProjectRelativePath) -> anyhow::Result<Self> {
        let cell_resolver = ctx.get_cell_resolver().await;

        let cwd = Package::from_cell_path(&cell_resolver.get_cell_path(&cwd)?);
        let cell_name = cwd.as_cell_path().cell();

        // Targets with cell aliases should be resolved against the cell mapping
        // as defined the cell derived from the cwd.
        let cell = cell_resolver
            .get(cell_name)
            .with_context(|| format!("Cell does not exist: `{}`", cell_name))?
            .dupe();

        // The same goes for target aliases.
        let config = ctx
            .get_legacy_config_for_cell(cell_name)
            .await
            .with_context(|| format!("No configuration for cell: `{}`", cell_name))?;

        let target_alias_resolver = config.target_alias_resolver();

        Ok(Self {
            cell,
            cwd,
            target_alias_resolver,
        })
    }

    pub fn parse_pattern<T: PatternType>(&self, pattern: &str) -> anyhow::Result<ParsedPattern<T>> {
        ParsedPattern::parse_relaxed(
            &self.target_alias_resolver,
            self.cell.cell_alias_resolver(),
            &self.cwd,
            pattern,
        )
    }
}

/// Parse target patterns out of command line arguments.
///
/// The format allowed here is more relaxed than in build files and elsewhere, so only use this
/// with strings passed by the user on the CLI.
/// See `ParsedPattern::parse_relaxed` for details.
pub async fn parse_patterns_from_cli_args<T: PatternType>(
    target_patterns: &[buck2_data::TargetPattern],
    ctx: &DiceTransaction,
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<ParsedPattern<T>>> {
    let parser = PatternParser::new(ctx, cwd).await?;

    target_patterns.try_map(|value| parser.parse_pattern(&value.value))
}

pub async fn resolve_patterns<T: PatternType>(
    patterns: &[ParsedPattern<T>],
    cell_resolver: &CellResolver,
    file_ops: &dyn FileOps,
) -> anyhow::Result<ResolvedPattern<T>> {
    resolve_target_patterns(cell_resolver, patterns.iter(), file_ops).await
}

pub fn parse_concurrency(requested: u32) -> anyhow::Result<usize> {
    let mut ret = requested.try_into().context("Invalid concurrency")?;

    if ret == 0 {
        ret = num_cpus::get();
    }

    Ok(ret)
}

/// Extract target configuration (platform) label from [`ClientContext`].
pub async fn target_platform_from_client_context(
    client_context: Option<&ClientContext>,
    server_ctx: &ServerCommandContext,
) -> anyhow::Result<Option<TargetLabel>> {
    // TODO(cjhopman): This shouldn't be getting a new dice_ctx. We should move this
    // to be a function on the ServerCommandContext that takes in the CellResolver.
    let dice_ctx = server_ctx.unsafe_dice_ctx();
    let cell_resolver = dice_ctx.get_cell_resolver().await;
    let cwd = cell_resolver.get_cell_path(&server_ctx.working_dir)?;
    let cell_alias_resolver = cell_resolver.get(cwd.cell()).unwrap().cell_alias_resolver();

    Ok(match client_context {
        Some(client_context) => {
            let target_platform = &client_context.target_platform;
            if !target_platform.is_empty() {
                Some(
                    ParsedPattern::parse_precise(cell_alias_resolver, target_platform)?
                        .as_target_label(target_platform)?,
                )
            } else {
                None
            }
        }
        None => None,
    })
}

/// For each buck invocations, we'll have a single CommandExecutorFactory. This contains shared
/// state used by all command executor strategies.
pub struct CommandExecutorFactory {
    pub re_connection: ReConnectionHandle,
    // TODO(cjhopman): This should probably be a global limit, otherwise simultaneous commands
    // may use more resources than intended.
    pub host_sharing_broker: Arc<HostSharingBroker>,
    pub materializer: Arc<dyn Materializer>,
    /// The strategy that the user requested on the CLI, expressed as a filter. When execution
    /// platforms are enabled, this cannot control what execution platform we use, but it can only
    /// bias how a given executor works, or refuse to function entirely.
    pub filter: ExecutorFilter,
}

impl CommandExecutorFactory {
    pub fn new(
        re_connection: ReConnectionHandle,
        host_sharing_broker: HostSharingBroker,
        materializer: Arc<dyn Materializer>,
        filter: ExecutorFilter,
    ) -> Self {
        Self {
            re_connection,
            host_sharing_broker: Arc::new(host_sharing_broker),
            materializer,
            filter,
        }
    }
}

impl HasCommandExecutor for CommandExecutorFactory {
    fn get_command_executor(
        &self,
        config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn PreparedCommandExecutor>> {
        let local_executor_new = |_options| {
            LocalExecutor::new(
                config.artifact_fs.clone(),
                self.materializer.dupe(),
                self.host_sharing_broker.dupe(),
                config.project_fs.root.clone(),
            )
        };

        if !cfg!(fbcode_build) {
            static WARN: OnceCell<()> = OnceCell::new();
            WARN.get_or_init(|| {
                tracing::warn!("Cargo build detected: disabling remote execution and caching!")
            });

            if self.filter.ban_local {
                return Err(anyhow::anyhow!(
                    "The desired execution strategy (`{:?}`) is incompatible with the local executor",
                    self.filter,
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
                config.artifact_fs.clone(),
                config.project_fs.clone(),
                self.materializer.dupe(),
                self.re_connection.get_client(),
                properties,
                options.re_action_key.clone(),
                options
                    .re_max_input_files_bytes
                    .unwrap_or(DEFAULT_RE_MAX_INPUT_FILE_BYTES),
            )
        };

        let inner_executor: Arc<dyn PreparedCommandExecutor> = match (
            &config.executor_config,
            &self.filter,
        ) {
            (
                ActionExecutorConfig::Local(local),
                ExecutorFilter {
                    ban_local: false, ..
                },
            ) => Arc::new(local_executor_new(local)),
            (
                ActionExecutorConfig::Remote(remote),
                ExecutorFilter {
                    ban_remote: false, ..
                },
            ) => Arc::new(remote_executor_new(remote)),
            (
                ActionExecutorConfig::Hybrid {
                    local,
                    remote,
                    is_limited,
                },
                ExecutorFilter {
                    ban_local: false,
                    ban_remote: false,
                    prefer_local_for_hybrid,
                },
            ) => Arc::new(HybridExecutor {
                local: local_executor_new(local),
                remote: remote_executor_new(remote),
                limited: *is_limited,
                prefer_local: *prefer_local_for_hybrid,
            }),
            (config, strategy) => {
                return Err(anyhow::anyhow!(
                    "The desired execution strategy (`{:?}`) is incompatible with the executor config that was selected: {:?}",
                    strategy,
                    config
                ));
            }
        };

        static DISABLE_CACHING: EnvHelper<bool> = EnvHelper::new("BUCK2_TEST_DISABLE_CACHING");

        if DISABLE_CACHING.get()?.unwrap_or(false) {
            return Ok(inner_executor);
        }

        Ok(Arc::new(CacheCheckingExecutor::new(
            inner_executor,
            self.materializer.dupe(),
            self.re_connection.get_client(),
        )))
    }
}

/// Translate an ExecutionStrategy to a filter that will be checked when instantiating executors.
/// This allows the execution strategy to have some limited influence over execution when execution
/// platforms are enabled.
#[derive(Debug)]
pub struct ExecutorFilter {
    ban_local: bool,
    ban_remote: bool,
    prefer_local_for_hybrid: bool,
}

impl From<ExecutionStrategy> for ExecutorFilter {
    fn from(strategy: ExecutionStrategy) -> Self {
        let mut base = Self {
            ban_local: false,
            ban_remote: false,
            prefer_local_for_hybrid: false,
        };

        match strategy {
            ExecutionStrategy::Default => {}
            ExecutionStrategy::LocalOnly => {
                base.ban_remote = true;
            }
            ExecutionStrategy::RemoteOnly => base.ban_local = true,
            ExecutionStrategy::Hybrid => {}
            ExecutionStrategy::HybridPreferLocal => {
                base.prefer_local_for_hybrid = true;
            }
            ExecutionStrategy::NoExecution => {
                base.ban_local = true;
                base.ban_remote = true;
            }
        };

        base
    }
}

pub fn get_executor_config_for_strategy(
    strategy: ExecutionStrategy,
    host_platform: HostPlatformOverride,
) -> ActionExecutorConfig {
    let execution_platform = get_execution_platform(host_platform);
    match strategy {
        // NOTE: NoExecution here retunrs a default config, which is fine because the filter will
        // kick in later.
        ExecutionStrategy::Default | ExecutionStrategy::NoExecution => {
            ActionExecutorConfig::Hybrid {
                local: LocalExecutorOptions {},
                remote: RemoteExecutorOptions {
                    re_properties: execution_platform.intrinsic_properties(),
                    re_action_key: None,
                    re_max_input_files_bytes: None,
                },
                is_limited: true,
            }
        }
        // NOTE: We don't differnetiate between the preferences for Hybrid here. This gets injected
        // later when we actually instantiate the Executor.
        ExecutionStrategy::Hybrid | ExecutionStrategy::HybridPreferLocal => {
            ActionExecutorConfig::Hybrid {
                local: LocalExecutorOptions {},
                remote: RemoteExecutorOptions {
                    re_properties: execution_platform.intrinsic_properties(),
                    re_action_key: None,
                    re_max_input_files_bytes: None,
                },
                is_limited: false,
            }
        }
        ExecutionStrategy::LocalOnly => ActionExecutorConfig::Local(LocalExecutorOptions {}),
        ExecutionStrategy::RemoteOnly => ActionExecutorConfig::Remote(RemoteExecutorOptions {
            re_properties: execution_platform.intrinsic_properties(),
            re_action_key: None,
            re_max_input_files_bytes: None,
        }),
    }
}

fn get_execution_platform(host_platform: HostPlatformOverride) -> ExecutionPlatform {
    let linux = ExecutionPlatform::Linux;
    // TODO(T110757645): The xcode version should come from the execution platform or toolchain
    let mac = ExecutionPlatform::MacOS {
        xcode_version: "13.2".to_owned(),
    };

    let windows = ExecutionPlatform::Windows;

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
