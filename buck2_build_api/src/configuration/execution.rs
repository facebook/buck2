/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::{configuration::Configuration, target::TargetLabel};
use derive_more::Display;
use gazebo::prelude::*;
use itertools::Itertools;
use thiserror::Error;

use crate::{execute::ActionExecutorConfig, nodes::compatibility::IncompatiblePlatformReason};

/// An execution platform is used for the execution deps of a target, those dependencies that
/// need to be invoked as part of a build action or otherwise need to be configured against the
/// platform that execution happens on (as opposed to the normal case where things are configured
/// against the target platform).
///
/// It consists of that platform `Configuration` and configuration of how to do the execution
/// (e.g. local, remote, etc.).
#[derive(Debug, Eq, PartialEq, Hash)]
pub enum ExecutionPlatform {
    /// A user-defined platform.
    Platform {
        target: TargetLabel,
        cfg: Configuration,
        executor_config: ActionExecutorConfig,
    },
    /// When users haven't configured execution platforms, we will use old legacy behavior where
    /// execution deps inherit the target platform configuration.
    LegacyExecutionPlatform {
        executor_config: ActionExecutorConfig,
        cfg: Configuration,
    },
    /// An invalid execution platform.
    Unspecified,
    /// There are some contexts where we need an ExecutionPlatform but cannot first perform
    /// execution platform resolution. This is a placeholder for those places, but it will
    /// be an error if it is ever used for actual execution.
    Unbound,
}

impl ExecutionPlatform {
    pub fn unspecified() -> Self {
        Self::Unspecified
    }

    pub fn cfg(&self) -> Configuration {
        match self {
            ExecutionPlatform::Platform { cfg, .. } => cfg.dupe(),
            ExecutionPlatform::LegacyExecutionPlatform { cfg, .. } => cfg.dupe(),
            ExecutionPlatform::Unspecified => Configuration::unspecified_exec(),
            ExecutionPlatform::Unbound => {
                panic!("configuration of the unbound execution platform should never be accessed.")
            }
        }
    }

    pub fn id(&self) -> String {
        match self {
            ExecutionPlatform::Platform { target, .. } => target.to_string(),
            ExecutionPlatform::LegacyExecutionPlatform { .. } => {
                "<legacy_global_exec_platform>".to_owned()
            }
            ExecutionPlatform::Unspecified => "<unspecified_exec_platform>".to_owned(),
            ExecutionPlatform::Unbound => "<unbound_exec_platform>".to_owned(),
        }
    }

    pub fn executor_config(&self) -> anyhow::Result<&ActionExecutorConfig> {
        match self {
            ExecutionPlatform::Platform {
                executor_config, ..
            } => Ok(executor_config),
            ExecutionPlatform::LegacyExecutionPlatform {
                executor_config, ..
            } => Ok(executor_config),
            ExecutionPlatform::Unspecified => {
                Err(ExecutionPlatformError::UnspecifiedPlatformCantExecute.into())
            }
            ExecutionPlatform::Unbound => {
                Err(ExecutionPlatformError::UnboundPlatformCantExecute.into())
            }
        }
    }
}

#[derive(Display, Debug, Eq, PartialEq, Hash)]
pub enum ExecutionPlatformIncompatibleReason {
    #[display(
        fmt = "exec_compatible_with requires `{}` but it was not satisfied",
        _0
    )]
    ConstraintNotSatisfied(TargetLabel),
    #[display(fmt = "{} incompatible due to {}", _0, _1)]
    ExecutionDependencyIncompatible(TargetLabel, Arc<IncompatiblePlatformReason>),
}

#[derive(Debug, Error)]
pub enum ExecutionPlatformError {
    #[error("No compatible execution platform.\n{}", .0.iter().map(|(id, reason)| format!("  `{}` skipped because\n:   {}", id, reason)).join("\n"))]
    NoCompatiblePlatform(Arc<Vec<(String, ExecutionPlatformIncompatibleReason)>>),
    #[error(
        "The <unspecified> execution platform has no executor config and shouldn't be used in a context that requires one."
    )]
    UnspecifiedPlatformCantExecute,
    #[error(
        "The <unbound> execution platform is used when no execution platform can be found for an action and shouldn't be used in a context that requires actually executing."
    )]
    UnboundPlatformCantExecute,
}

/// Represents the result of performing execution platform resolution. It stores both the
/// resolved platform (if there is one) and the reasons why higher priority platforms were
/// skipped.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub struct ExecutionPlatformResolution {
    /// The platform if resolution found one. If all platforms are skipped, this will be `None`.
    platform: Option<Arc<ExecutionPlatform>>,
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
        platform: Option<Arc<ExecutionPlatform>>,
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
            Some(v) => Ok(&*v),
            None => Err(ExecutionPlatformError::NoCompatiblePlatform(
                self.skipped_platforms.dupe(),
            )
            .into()),
        }
    }

    pub fn skipped(&self) -> &[(String, ExecutionPlatformIncompatibleReason)] {
        &*self.skipped_platforms
    }

    pub fn executor_config(&self) -> anyhow::Result<&ActionExecutorConfig> {
        self.platform()?.executor_config()
    }
}
