/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;

use anyhow::anyhow;
use anyhow::Context as _;
use buck2_build_api::actions::artifact::ArtifactFs;
use buck2_build_api::build::MaterializationContext;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::execute::blocking::BlockingExecutor;
use buck2_build_api::execute::commands::dice_data::HasCommandExecutor;
use buck2_build_api::execute::commands::hybrid::HybridExecutor;
use buck2_build_api::execute::commands::local::LocalExecutor;
use buck2_build_api::execute::commands::re::caching_executor::CachingExecutor;
use buck2_build_api::execute::commands::re::manager::ReConnectionHandle;
use buck2_build_api::execute::commands::re::ReExecutionPlatform;
use buck2_build_api::execute::commands::re::ReExecutor;
use buck2_build_api::execute::commands::re::ReExecutorGlobalKnobs;
use buck2_build_api::execute::commands::ExecutorPreference;
use buck2_build_api::execute::commands::PreparedCommandExecutor;
use buck2_build_api::execute::materializer::Materializer;
use buck2_common::file_ops::FileOps;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::target_aliases::BuckConfigTargetAliasResolver;
use buck2_core::cells::CellInstance;
use buck2_core::cells::CellResolver;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::package::Package;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::PatternType;
use buck2_core::target::TargetLabel;
use buck2_forkserver::client::ForkserverClient;
use buck2_interpreter::common::BxlFilePath;
use buck2_interpreter::parse_import::parse_import_with_config;
use buck2_interpreter::parse_import::ParseImportOptions;
use buck2_node::execute::config::CommandExecutorConfig;
use buck2_node::execute::config::CommandExecutorKind;
use buck2_node::execute::config::HybridExecutionLevel;
use buck2_node::execute::config::LocalExecutorOptions;
use buck2_node::execute::config::RemoteExecutorOptions;
use cli_proto::build_request::Materializations;
use cli_proto::client_context::HostPlatformOverride;
use cli_proto::common_build_options::ExecutionStrategy;
use cli_proto::ClientContext;
use dashmap::DashMap;
use gazebo::prelude::*;
use host_sharing::HostSharingBroker;
use once_cell::sync::OnceCell;
use thiserror::Error;

pub(crate) trait ToProtoDuration {
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

pub(crate) enum ConfigType {
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

pub(crate) struct PatternParser {
    cell: CellInstance,
    cwd: Package,
    target_alias_resolver: BuckConfigTargetAliasResolver,
}

impl PatternParser {
    pub(crate) fn new(
        cell_resolver: &CellResolver,
        config: &LegacyBuckConfigs,
        cwd: &ProjectRelativePath,
    ) -> anyhow::Result<Self> {
        let cwd = Package::from_cell_path(&cell_resolver.get_cell_path(&cwd)?);
        let cell_name = cwd.as_cell_path().cell();

        // Targets with cell aliases should be resolved against the cell mapping
        // as defined the cell derived from the cwd.
        let cell = cell_resolver
            .get(cell_name)
            .with_context(|| format!("Cell does not exist: `{}`", cell_name))?
            .dupe();

        let target_alias_resolver = config.get(cell_name)?.target_alias_resolver();

        Ok(Self {
            cell,
            cwd,
            target_alias_resolver,
        })
    }

    pub(crate) fn parse_pattern<T: PatternType>(
        &self,
        pattern: &str,
    ) -> anyhow::Result<ParsedPattern<T>> {
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
pub(crate) fn parse_patterns_from_cli_args<T: PatternType>(
    target_patterns: &[buck2_data::TargetPattern],
    cell_resolver: &CellResolver,
    configs: &LegacyBuckConfigs,
    cwd: &ProjectRelativePath,
) -> anyhow::Result<Vec<ParsedPattern<T>>> {
    let parser = PatternParser::new(cell_resolver, configs, cwd)?;

    target_patterns.try_map(|value| parser.parse_pattern(&value.value))
}

pub(crate) async fn resolve_patterns<T: PatternType>(
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
pub(crate) async fn target_platform_from_client_context(
    client_context: Option<&ClientContext>,
    cell_resolver: &CellResolver,
    working_dir: &ProjectRelativePathBuf,
) -> anyhow::Result<Option<TargetLabel>> {
    let cwd = cell_resolver.get_cell_path(working_dir)?;
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
    pub blocking_executor: Arc<dyn BlockingExecutor>,
    pub strategy: ExecutionStrategy,
    pub re_global_knobs: ReExecutorGlobalKnobs,
    pub upload_all_actions: bool,
    pub forkserver: Option<ForkserverClient>,
}

impl CommandExecutorFactory {
    pub fn new(
        re_connection: ReConnectionHandle,
        host_sharing_broker: HostSharingBroker,
        materializer: Arc<dyn Materializer>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        strategy: ExecutionStrategy,
        re_global_knobs: ReExecutorGlobalKnobs,
        upload_all_actions: bool,
        forkserver: Option<ForkserverClient>,
    ) -> Self {
        Self {
            re_connection,
            host_sharing_broker: Arc::new(host_sharing_broker),
            materializer,
            blocking_executor,
            strategy,
            re_global_knobs,
            upload_all_actions,
            forkserver,
        }
    }
}

impl HasCommandExecutor for CommandExecutorFactory {
    fn get_command_executor(
        &self,
        artifact_fs: &ArtifactFs,
        project_fs: &ProjectFilesystem,
        executor_config: &CommandExecutorConfig,
    ) -> anyhow::Result<Arc<dyn PreparedCommandExecutor>> {
        let local_executor_new = |_options| {
            LocalExecutor::new(
                artifact_fs.clone(),
                self.materializer.dupe(),
                self.blocking_executor.dupe(),
                self.host_sharing_broker.dupe(),
                project_fs.root.clone(),
                self.forkserver.dupe(),
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
                project_fs.clone(),
                self.materializer.dupe(),
                self.re_connection.get_client(),
                properties,
                options.re_action_key.clone(),
                options
                    .re_max_input_files_bytes
                    .unwrap_or(DEFAULT_RE_MAX_INPUT_FILE_BYTES),
                options.re_use_case.clone(),
                self.re_global_knobs.dupe(),
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

        if DISABLE_CACHING.get()?.unwrap_or(false) {
            return Ok(inner_executor);
        }

        Ok(Arc::new(CachingExecutor::new(
            inner_executor,
            self.materializer.dupe(),
            self.re_connection.get_client(),
            Default::default(), // TODO: This should probably use the RemoteExecutor's use case.
            self.upload_all_actions,
            self.re_global_knobs.dupe(),
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

pub(crate) fn get_executor_config_for_strategy(
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
    CommandExecutorConfig::new_with_default_path_separator(executor_kind)
}

fn get_re_execution_platform(host_platform: HostPlatformOverride) -> ReExecutionPlatform {
    let linux = ReExecutionPlatform::Linux;
    // TODO(T110757645): The xcode version should come from the execution platform or toolchain
    let mac = ReExecutionPlatform::MacOS {
        xcode_version: "13.4".to_owned(),
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

#[derive(Debug, Error)]
#[error("bxl label should be of format `<cell>//path/to/file.bxl:function_name`, but got `{0}`")]
struct BxlLabelError(String);

/// Parse the bxl function label out of cli pattern
pub fn parse_bxl_label_from_cli(
    cwd: &ProjectRelativePath,
    bxl_label: &str,
    cell_resolver: &CellResolver,
) -> anyhow::Result<BxlFunctionLabel> {
    let current_cell = cell_resolver.get_cell_path(cwd)?;

    // Targets with cell aliases should be resolved against the cell mapping
    // as defined the cell derived from the cwd.
    let cell_alias_resolver = cell_resolver
        .get(current_cell.cell())
        .unwrap()
        .cell_alias_resolver();

    let (bxl_path, bxl_fn) = bxl_label
        .rsplit_once(':')
        .ok_or_else(|| BxlLabelError(bxl_label.to_owned()))?;

    const OPTS: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: true,
        allow_relative_imports: true,
    };
    let import_path =
        parse_import_with_config(cell_alias_resolver, &current_cell, bxl_path, &OPTS)?;

    Ok(BxlFunctionLabel {
        bxl_path: BxlFilePath::new(import_path)?,
        name: bxl_fn.to_owned(),
    })
}

pub(crate) trait ConvertMaterializationContext {
    fn from(self) -> MaterializationContext;
}

impl ConvertMaterializationContext for Materializations {
    fn from(self) -> MaterializationContext {
        match self {
            Materializations::Skip => MaterializationContext::Skip,
            Materializations::Default => MaterializationContext::Materialize {
                map: Arc::new(DashMap::new()),
                force: false,
            },
            Materializations::Materialize => MaterializationContext::Materialize {
                map: Arc::new(DashMap::new()),
                force: true,
            },
        }
    }
}
