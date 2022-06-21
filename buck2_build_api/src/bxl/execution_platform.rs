//! Common utilities for bxl

use std::sync::Arc;

use buck2_core::configuration::Configuration;
use once_cell::sync::Lazy;

use crate::configuration::execution::ExecutionPlatform;
use crate::configuration::execution::ExecutionPlatformResolution;
use crate::execute::CommandExecutorConfig;
use crate::execute::CommandExecutorKind;
use crate::execute::LocalExecutorOptions;

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
