//! Common utilities for bxl

use std::sync::Arc;

use buck2_common::target_aliases::TargetAliasResolver;
use buck2_core::{cells::CellAliasResolver, configuration::Configuration, package::Package};
use once_cell::sync::Lazy;

use crate::{
    configuration::execution::{ExecutionPlatform, ExecutionPlatformResolution},
    execute::{CommandExecutorConfig, CommandExecutorKind, LocalExecutorOptions},
};

// TODO(bobyf) this should be configured by the bxl function similar to
// execution groups, which we don't have yet. Currently we hard code this to
// local since IDE will mostly be invoking local tools anyways
pub static EXECUTION_PLATFORM: Lazy<ExecutionPlatformResolution> = Lazy::new(|| {
    ExecutionPlatformResolution::new(
        Some(Arc::new(ExecutionPlatform::LegacyExecutionPlatform {
            executor_config: CommandExecutorConfig::new_with_default_path_separator(
                CommandExecutorKind::Local(LocalExecutorOptions {}),
            ),
            cfg: Configuration::unspecified(),
        })),
        vec![],
    )
});

pub struct CliResolutionCtx {
    pub target_alias_resolver: TargetAliasResolver,
    pub cell_resolver: CellAliasResolver,
    pub relative_dir: Package,
}
