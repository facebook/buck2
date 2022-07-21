/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::configuration::Configuration;
use buck2_core::target::TargetLabel;
use gazebo::prelude::*;
use itertools::Itertools;
use thiserror::Error;

use crate::compatibility::IncompatiblePlatformReason;
use crate::execute::config::CommandExecutorConfig;

/// An execution platform is used for the execution deps of a target, those dependencies that
/// need to be invoked as part of a build action or otherwise need to be configured against the
/// platform that execution happens on (as opposed to the normal case where things are configured
/// against the target platform).
///
/// It consists of that platform `Configuration` and configuration of how to do the execution
/// (e.g. local, remote, etc.).
#[derive(Debug, Eq, PartialEq, Hash, Dupe, Clone)]
pub struct ExecutionPlatform(Arc<ExecutionPlatformData>);

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum ExecutionPlatformData {
    /// A user-defined platform.
    Platform {
        target: TargetLabel,
        cfg: Configuration,
        executor_config: CommandExecutorConfig,
    },
    /// When users haven't configured execution platforms, we will use old legacy behavior where
    /// execution deps inherit the target platform configuration.
    LegacyExecutionPlatform {
        executor_config: CommandExecutorConfig,
        cfg: Configuration,
    },
}

impl ExecutionPlatform {
    pub fn platform(
        target: TargetLabel,
        cfg: Configuration,
        executor_config: CommandExecutorConfig,
    ) -> Self {
        Self(Arc::new(ExecutionPlatformData::Platform {
            target,
            cfg,
            executor_config,
        }))
    }

    pub fn legacy_execution_platform(
        executor_config: CommandExecutorConfig,
        cfg: Configuration,
    ) -> Self {
        Self(Arc::new(ExecutionPlatformData::LegacyExecutionPlatform {
            executor_config,
            cfg,
        }))
    }

    pub fn cfg(&self) -> Configuration {
        match &*self.0 {
            ExecutionPlatformData::Platform { cfg, .. } => cfg.dupe(),
            ExecutionPlatformData::LegacyExecutionPlatform { cfg, .. } => cfg.dupe(),
        }
    }

    pub fn id(&self) -> String {
        match &*self.0 {
            ExecutionPlatformData::Platform { target, .. } => target.to_string(),
            ExecutionPlatformData::LegacyExecutionPlatform { .. } => {
                "<legacy_global_exec_platform>".to_owned()
            }
        }
    }

    pub fn executor_config(&self) -> &CommandExecutorConfig {
        match &*self.0 {
            ExecutionPlatformData::Platform {
                executor_config, ..
            } => executor_config,
            ExecutionPlatformData::LegacyExecutionPlatform {
                executor_config, ..
            } => executor_config,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum ExecutionPlatformIncompatibleReason {
    ConstraintNotSatisfied(TargetLabel),
    ExecutionDependencyIncompatible(Arc<IncompatiblePlatformReason>),
    ToolchainDependencyIncompatible,
}

impl std::fmt::Display for ExecutionPlatformIncompatibleReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionPlatformIncompatibleReason::ConstraintNotSatisfied(v) => write!(
                f,
                "exec_compatible_with requires `{}` but it was not satisfied",
                v
            ),
            ExecutionPlatformIncompatibleReason::ExecutionDependencyIncompatible(v) => v.fmt(f),
            ExecutionPlatformIncompatibleReason::ToolchainDependencyIncompatible => {
                write!(f, "toolchain dependency was incompatible")
            }
        }
    }
}

#[derive(Debug, Error)]
enum ExecutionPlatformError {
    #[error("No compatible execution platform.\n{}", .0.iter().map(|(id, reason)| format!("  `{}` skipped because\n:   {}", id, reason)).join("\n"))]
    NoCompatiblePlatform(Arc<Vec<(String, ExecutionPlatformIncompatibleReason)>>),
}

/// Represents the result of performing execution platform resolution. It stores both the
/// resolved platform (if there is one) and the reasons why higher priority platforms were
/// skipped.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub struct ExecutionPlatformResolution {
    /// The platform if resolution found one. If all platforms are skipped, this will be `None`.
    platform: Option<ExecutionPlatform>,
    /// The human readable names of skipped platforms and the reason they were skipped.
    skipped_platforms: Arc<Vec<(String, ExecutionPlatformIncompatibleReason)>>,
}

impl ExecutionPlatformResolution {
    pub fn unspecified() -> Self {
        Self {
            platform: None,
            skipped_platforms: Arc::new(Vec::new()),
        }
    }

    pub fn new(
        platform: Option<ExecutionPlatform>,
        skipped: Vec<(String, ExecutionPlatformIncompatibleReason)>,
    ) -> Self {
        Self {
            platform,
            skipped_platforms: Arc::new(skipped),
        }
    }

    // TODO(cjhopman): Should this be an anyhow::Result and never return an invalid configuration?
    pub fn cfg(&self) -> Configuration {
        match &self.platform {
            Some(v) => v.cfg(),
            None => Configuration::unspecified_exec().dupe(),
        }
    }

    pub fn platform(&self) -> anyhow::Result<&ExecutionPlatform> {
        match &self.platform {
            Some(v) => Ok(v),
            None => Err(ExecutionPlatformError::NoCompatiblePlatform(
                self.skipped_platforms.dupe(),
            )
            .into()),
        }
    }

    pub fn skipped(&self) -> &[(String, ExecutionPlatformIncompatibleReason)] {
        &*self.skipped_platforms
    }

    pub fn executor_config(&self) -> anyhow::Result<&CommandExecutorConfig> {
        Ok(self.platform()?.executor_config())
    }
}
