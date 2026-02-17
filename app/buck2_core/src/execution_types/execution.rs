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
use once_cell::sync::Lazy;
use once_cell::sync::OnceCell;
use pagable::Pagable;
use starlark_map::ordered_map::OrderedMap;

use crate::configuration::compatibility::IncompatiblePlatformReason;
use crate::configuration::compatibility::IncompatiblePlatformReasonCause;
use crate::configuration::data::ConfigurationData;
use crate::configuration::pair::ConfigurationNoExec;
use crate::execution_types::executor_config::CommandExecutorConfig;
use crate::provider::label::ProvidersLabel;
use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::label::label::TargetLabel;

/// Whether to apply execution modifiers to exec_deps.
/// This flag gates the behavior of applying cfg_constructor modifiers to exec_deps,
/// which affects action digests. Default is false.
pub static APPLY_EXEC_MODIFIERS: OnceCell<bool> = OnceCell::new();

pub fn init_apply_exec_modifiers(value: Option<bool>) -> buck2_error::Result<()> {
    let value = value.unwrap_or(false);
    APPLY_EXEC_MODIFIERS.set(value).map_err(|_| {
        buck2_error::buck2_error!(
            buck2_error::ErrorTag::Tier0,
            "APPLY_EXEC_MODIFIERS is already initialized"
        )
    })?;
    Ok(())
}

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

/// Base data shared between Partial and Resolved states.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Pagable)]
pub(crate) struct ExecutionPlatformResolutionBase {
    /// The platform if resolution found one. If all platforms are skipped, this will be `None`.
    platform: Option<ExecutionPlatform>,
    /// The human readable names of skipped platforms and the reason they were skipped.
    skipped_platforms: Arc<Vec<(String, ExecutionPlatformIncompatibleReason)>>,
}

impl ExecutionPlatformResolutionBase {
    fn new(
        platform: Option<ExecutionPlatform>,
        skipped: Vec<(String, ExecutionPlatformIncompatibleReason)>,
    ) -> Arc<Self> {
        Arc::new(Self {
            platform,
            skipped_platforms: Arc::new(skipped),
        })
    }

    #[inline]
    fn cfg(&self) -> ConfigurationNoExec {
        match &self.platform {
            Some(v) => v.cfg_pair_no_exec().dupe(),
            None => ConfigurationNoExec::unspecified_exec().dupe(),
        }
    }

    fn platform(&self) -> buck2_error::Result<&ExecutionPlatform> {
        match &self.platform {
            Some(v) => Ok(v),
            None => Err(ExecutionPlatformError::NoCompatiblePlatform(
                self.skipped_platforms.dupe(),
            )
            .into()),
        }
    }

    fn skipped(&self) -> &[(String, ExecutionPlatformIncompatibleReason)] {
        &self.skipped_platforms
    }

    fn executor_config(&self) -> buck2_error::Result<&Arc<CommandExecutorConfig>> {
        Ok(self.platform()?.executor_config())
    }
}

/// Transient result of execution platform resolution, before modifiers are applied.
///
/// Contains the first compatible execution platform from the candidates list.
/// Call `finalize(exec_dep_cfgs)` to produce the complete `ExecutionPlatformResolution`.
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
pub struct ExecutionPlatformResolutionPartial(Arc<ExecutionPlatformResolutionBase>);

impl ExecutionPlatformResolutionPartial {
    /// Create a new partial resolution with the given platform and skipped platforms.
    pub fn new(
        platform: Option<ExecutionPlatform>,
        skipped: Vec<(String, ExecutionPlatformIncompatibleReason)>,
    ) -> Self {
        Self(ExecutionPlatformResolutionBase::new(platform, skipped))
    }

    /// Get the execution platform configuration.
    #[inline]
    pub fn cfg(&self) -> ConfigurationNoExec {
        self.0.cfg()
    }

    /// Get the execution platform, if resolution found one.
    pub fn platform(&self) -> buck2_error::Result<&ExecutionPlatform> {
        self.0.platform()
    }

    /// Get the list of skipped platforms and reasons.
    pub fn skipped(&self) -> &[(String, ExecutionPlatformIncompatibleReason)] {
        self.0.skipped()
    }

    /// Get the executor config for the resolved platform.
    pub fn executor_config(&self) -> buck2_error::Result<&Arc<CommandExecutorConfig>> {
        self.0.executor_config()
    }

    /// Finalize the partial resolution with per-exec_dep configurations.
    /// This produces a complete `ExecutionPlatformResolution` that can be stored
    /// and used for attribute configuration.
    pub fn finalize(
        self,
        exec_dep_cfgs: OrderedMap<TargetLabel, ConfigurationData>,
    ) -> ExecutionPlatformResolution {
        ExecutionPlatformResolution::Resolved {
            base: self.0,
            exec_dep_cfgs: Arc::new(exec_dep_cfgs),
        }
    }
}

/// Complete execution platform resolution (stored in ConfiguredTargetNode).
///
/// This enum represents the two possible states:
/// - `Unspecified`: No execution platform configured (legacy/default behavior)
/// - `Resolved`: Fully resolved with per-exec_dep configurations computed
#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe, Allocative, Pagable)]
#[non_exhaustive]
#[allow(private_interfaces)]
pub enum ExecutionPlatformResolution {
    /// No execution platform specified (legacy/default behavior).
    /// Used during dependency gathering before execution platform is resolved,
    /// or for unbound configurations.
    Unspecified,
    /// Fully resolved with per-exec_dep configurations computed via modifiers.
    Resolved {
        #[doc(hidden)]
        base: Arc<ExecutionPlatformResolutionBase>,
        /// Mapping from exec_dep target labels to their actual configured cfgs.
        /// This is used when modifiers are applied to exec_deps, which may result
        /// in a different cfg than the default execution platform cfg.
        #[doc(hidden)]
        exec_dep_cfgs: Arc<OrderedMap<TargetLabel, ConfigurationData>>,
    },
}

impl ExecutionPlatformResolution {
    /// Create an unspecified resolution.
    /// This is used when execution platform resolution hasn't been done yet
    /// (e.g., during gather_deps before resolution, or for unbound configurations).
    pub fn unspecified() -> Self {
        Self::Unspecified
    }

    /// Create a new complete resolution directly for testing purposes.
    /// In production code, use `ExecutionPlatformResolutionPartial::new(...).finalize(...)`.
    pub fn new(
        platform: Option<ExecutionPlatform>,
        skipped: Vec<(String, ExecutionPlatformIncompatibleReason)>,
    ) -> Self {
        Self::Resolved {
            base: ExecutionPlatformResolutionBase::new(platform, skipped),
            exec_dep_cfgs: Arc::new(OrderedMap::new()),
        }
    }

    /// Create a new complete resolution with exec_dep_cfgs.
    /// Use this when you have the full exec_dep configuration mapping.
    pub fn new_with_exec_dep_cfgs(
        platform: Option<ExecutionPlatform>,
        skipped: Vec<(String, ExecutionPlatformIncompatibleReason)>,
        exec_dep_cfgs: OrderedMap<TargetLabel, ConfigurationData>,
    ) -> Self {
        Self::Resolved {
            base: ExecutionPlatformResolutionBase::new(platform, skipped),
            exec_dep_cfgs: Arc::new(exec_dep_cfgs),
        }
    }

    /// Get the base resolution data, if available.
    fn base(&self) -> Option<&ExecutionPlatformResolutionBase> {
        match self {
            Self::Unspecified => None,
            Self::Resolved { base, .. } => Some(base),
        }
    }

    // TODO(cjhopman): Should this be an buck2_error::Result and never return an invalid configuration?
    #[inline]
    pub fn base_cfg(&self) -> ConfigurationNoExec {
        match self.base() {
            Some(base) => base.cfg(),
            None => ConfigurationNoExec::unspecified_exec().dupe(),
        }
    }

    pub fn platform(&self) -> buck2_error::Result<&ExecutionPlatform> {
        match self.base() {
            Some(base) => base.platform(),
            None => Err(ExecutionPlatformError::NoCompatiblePlatform(Arc::new(Vec::new())).into()),
        }
    }

    pub fn skipped(&self) -> &[(String, ExecutionPlatformIncompatibleReason)] {
        match self.base() {
            Some(base) => base.skipped(),
            None => &[],
        }
    }

    pub fn executor_config(&self) -> buck2_error::Result<&Arc<CommandExecutorConfig>> {
        match self.base() {
            Some(base) => base.executor_config(),
            None => Err(ExecutionPlatformError::NoCompatiblePlatform(Arc::new(Vec::new())).into()),
        }
    }

    /// Get the per-exec_dep configurations.
    /// Returns an empty map for Unspecified state.
    pub fn exec_dep_cfgs(&self) -> &OrderedMap<TargetLabel, ConfigurationData> {
        static EMPTY: Lazy<OrderedMap<TargetLabel, ConfigurationData>> = Lazy::new(OrderedMap::new);
        match self {
            Self::Unspecified => &EMPTY,
            Self::Resolved { exec_dep_cfgs, .. } => exec_dep_cfgs,
        }
    }

    /// Returns true if this is in the Unspecified state (no execution platform resolution).
    /// This is used during dependency gathering before execution platform is resolved.
    pub fn is_unspecified(&self) -> bool {
        matches!(self, Self::Unspecified)
    }

    /// Get the configuration data for an exec_dep target.
    ///
    /// This is the correct way to configure exec_deps during analysis:
    /// - For `Unspecified`: returns the base execution platform cfg (fallback during gather_deps)
    /// - For `Resolved`: returns the cfg from `exec_dep_cfgs` if found, or error if not found
    ///
    /// If the target is not found in `Resolved` state, this indicates a bug where the
    /// exec_dep was not collected during dependency gathering.
    pub fn cfg_for_exec_dep(&self, target: &TargetLabel) -> buck2_error::Result<ConfigurationData> {
        if !*APPLY_EXEC_MODIFIERS.get().unwrap_or(&false) {
            return Ok(self.base_cfg().cfg().dupe());
        }
        match self {
            Self::Unspecified => {
                // During gather_deps, we use the base cfg as a placeholder.
                // This will be reconfigured later with the proper cfg.
                Ok(self.base_cfg().cfg().dupe())
            }
            Self::Resolved { exec_dep_cfgs, .. } => exec_dep_cfgs
                .get(target)
                .map(|cfg| cfg.dupe())
                .ok_or_else(|| {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::Tier0,
                        "exec_dep `{}` was not found in exec_dep_cfgs. \
                        This indicates a bug: this exec_dep was not collected during \
                        dependency gathering. Available exec_deps: [{}]",
                        target,
                        exec_dep_cfgs
                            .keys()
                            .map(|k| k.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )
                }),
        }
    }
}
