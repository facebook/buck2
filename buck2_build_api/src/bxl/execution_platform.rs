//! Common utilities for bxl

use std::sync::Arc;

use buck2_core::configuration::Configuration;
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_node::execute::config::CommandExecutorConfig;
use buck2_node::execute::config::CommandExecutorKind;
use buck2_node::execute::config::LocalExecutorOptions;
use once_cell::sync::Lazy;

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
