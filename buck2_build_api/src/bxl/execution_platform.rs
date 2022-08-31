//! Common utilities for bxl
use buck2_core::configuration::Configuration;
use buck2_node::configuration::execution::ExecutionPlatform;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_node::execute::config::CacheUploadBehavior;
use buck2_node::execute::config::CommandExecutorConfig;
use buck2_node::execute::config::CommandExecutorKind;
use buck2_node::execute::config::LocalExecutorOptions;
use buck2_node::execute::config::PathSeparatorKind;
use once_cell::sync::Lazy;

// TODO(bobyf) this should be configured by the bxl function similar to
// execution groups, which we don't have yet. Currently we hard code this to
// local since IDE will mostly be invoking local tools anyways
pub static EXECUTION_PLATFORM: Lazy<ExecutionPlatformResolution> = Lazy::new(|| {
    ExecutionPlatformResolution::new(
        Some(ExecutionPlatform::legacy_execution_platform(
            CommandExecutorConfig {
                executor_kind: CommandExecutorKind::Local(LocalExecutorOptions {}),
                path_separator: PathSeparatorKind::system_default(),
                cache_upload_behavior: CacheUploadBehavior::Disabled,
            },
            Configuration::unspecified(),
        )),
        vec![],
    )
});
