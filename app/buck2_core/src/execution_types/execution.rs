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

use allocative::Allocative;
use dupe::Dupe;
use indent_write::indentable::Indentable;
use itertools::Itertools;
use pagable::Pagable;

use crate::configuration::compatibility::IncompatiblePlatformReason;
use crate::configuration::compatibility::IncompatiblePlatformReasonCause;
use crate::configuration::data::ConfigurationData;
use crate::configuration::pair::ConfigurationNoExec;
use crate::execution_types::executor_config::CommandExecutorConfig;
use crate::provider::label::ProvidersLabel;
use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::label::label::TargetLabel;

/// An execution platform is used for the execution deps of a target, those dependencies that
/// need to be invoked as part of a build action or otherwise need to be configured against the
/// platform that execution happens on (as opposed to the normal case where things are configured
/// against the target platform).
///
/// It consists of that platform `Configuration` and configuration of how to do the execution
/// (e.g. local, remote, etc.).
#[derive(Debug, Eq, PartialEq, Hash, Dupe, Clone, Allocative, Pagable)]
pub struct ExecutionPlatform(Arc<ExecutionPlatformData>);

#[derive(Debug, Eq, PartialEq, Hash, Allocative, Pagable)]
pub enum ExecutionPlatformData {
    /// A user-defined platform.
    Platform {
        target: TargetLabel,
        cfg: ConfigurationNoExec,
        executor_config: Arc<CommandExecutorConfig>,
    },
    /// When users haven't configured execution platforms, we will use old legacy behavior where
    /// execution deps inherit the target platform configuration.
    LegacyExecutionPlatform {
        executor_config: Arc<CommandExecutorConfig>,
        cfg: ConfigurationNoExec,
    },
}

impl ExecutionPlatform {
    pub fn platform(
        target: TargetLabel,
        cfg: ConfigurationData,
        executor_config: Arc<CommandExecutorConfig>,
    ) -> Self {
        Self(Arc::new(ExecutionPlatformData::Platform {
            target,
            cfg: ConfigurationNoExec::new(cfg),
            executor_config,
        }))
    }

    pub fn legacy_execution_platform(
        executor_config: Arc<CommandExecutorConfig>,
        cfg: ConfigurationNoExec,
    ) -> Self {
        Self(Arc::new(ExecutionPlatformData::LegacyExecutionPlatform {
            executor_config,
            cfg,
        }))
    }

    #[inline]
    pub fn cfg_pair_no_exec(&self) -> &ConfigurationNoExec {
        match &*self.0 {
            ExecutionPlatformData::Platform { cfg, .. } => cfg,
            ExecutionPlatformData::LegacyExecutionPlatform { cfg, .. } => cfg,
        }
    }

    #[inline]
    pub fn cfg(&self) -> &ConfigurationData {
        self.cfg_pair_no_exec().cfg()
    }

    pub fn id(&self) -> String {
        match &*self.0 {
            ExecutionPlatformData::Platform { target, .. } => target.to_string(),
            ExecutionPlatformData::LegacyExecutionPlatform { .. } => {
                "<legacy_global_exec_platform>".to_owned()
            }
        }
    }

    pub fn executor_config(&self) -> &Arc<CommandExecutorConfig> {
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

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative, Pagable)]
pub enum ExecutionPlatformIncompatibleReason {
    ConstraintNotSatisfied(ProvidersLabel),
    ExecutionDependencyIncompatible(Arc<IncompatiblePlatformReason>),
}

impl ExecutionPlatformIncompatibleReason {
    pub fn into_incompatible_platform_reason(
        self,
        target: ConfiguredTargetLabel,
    ) -> IncompatiblePlatformReason {
        match self {
            Self::ConstraintNotSatisfied(unsatisfied_config) => IncompatiblePlatformReason {
                target,
                cause: IncompatiblePlatformReasonCause::UnsatisfiedConfig(unsatisfied_config),
            },
            Self::ExecutionDependencyIncompatible(previous) => IncompatiblePlatformReason {
                target,
                cause: IncompatiblePlatformReasonCause::Dependency(previous),
            },
        }
    }
}

impl std::fmt::Display for ExecutionPlatformIncompatibleReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionPlatformIncompatibleReason::ConstraintNotSatisfied(v) => write!(
                f,
                "exec_compatible_with requires `{v}` but it was not satisfied"
            ),
            ExecutionPlatformIncompatibleReason::ExecutionDependencyIncompatible(v) => v.fmt(f),
        }
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
pub enum ExecutionPlatformError {
    // .indented() losing the alternate flag that we want to use to format the reason so we need to explicitly do that.
    #[error("No compatible execution platform.\n{}", .0.iter().map(|(id, reason)| format!("  `{}` skipped because:\n{}", id, format!("{reason:#}").indented("    "))).join("\n"))]
    NoCompatiblePlatform(Arc<Vec<(String, ExecutionPlatformIncompatibleReason)>>),
}

/// Represents the result of performing execution platform resolution. It stores both the
/// resolved platform (if there is one) and the reasons why higher priority platforms were
/// skipped.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
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

    // TODO(cjhopman): Should this be an buck2_error::Result and never return an invalid configuration?
    #[inline]
    pub fn cfg(&self) -> ConfigurationNoExec {
        match &self.platform {
            Some(v) => v.cfg_pair_no_exec().dupe(),
            None => ConfigurationNoExec::unspecified_exec().dupe(),
        }
    }

    pub fn platform(&self) -> buck2_error::Result<&ExecutionPlatform> {
        match &self.platform {
            Some(v) => Ok(v),
            None => Err(ExecutionPlatformError::NoCompatiblePlatform(
                self.skipped_platforms.dupe(),
            )
            .into()),
        }
    }

    pub fn skipped(&self) -> &[(String, ExecutionPlatformIncompatibleReason)] {
        &self.skipped_platforms
    }

    pub fn executor_config(&self) -> buck2_error::Result<&Arc<CommandExecutorConfig>> {
        Ok(self.platform()?.executor_config())
    }
}
