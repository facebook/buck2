/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_core::configuration::constraints::ConstraintKey;
use buck2_core::configuration::constraints::ConstraintValue;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::execution_types::execution::ExecutionPlatform;
use buck2_core::target::label::label::TargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::FreezeBranded;
use starlark::values::StarlarkPagable;
use starlark::values::Trace;
use starlark::values::ValueTyped;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::command_executor_config::StarlarkCommandExecutorConfig;
use crate::interpreter::rule_defs::provider::builtin::configuration_info::ConfigurationInfo;

/// Provider that signals that a target represents an execution platform.
#[internal_provider(info_creator)]
#[derive(
    Clone,
    Debug,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
#[repr(C)]
pub struct ExecutionPlatformInfo<'v> {
    /// label of the defining rule, used in informative messages
    label: ValueTyped<'v, StarlarkTargetLabel>,
    /// The configuration of the execution platform
    configuration: ValueTyped<'v, ConfigurationInfo<'v>>,
    /// The executor config
    executor_config: ValueTyped<'v, StarlarkCommandExecutorConfig>,
}

impl<'v> ExecutionPlatformInfo<'v> {
    pub fn to_execution_platform(&self) -> buck2_error::Result<ExecutionPlatform> {
        self.to_execution_platform_with_marker(None)
    }

    /// Convert to an ExecutionPlatform, optionally adding a marker constraint to the configuration.
    pub fn to_execution_platform_with_marker(
        &self,
        marker_constraint: Option<&(ConstraintKey, ConstraintValue)>,
    ) -> buck2_error::Result<ExecutionPlatform> {
        let target = self.label.label();
        let mut cfg = self.configuration.to_configuration_data()?;

        // Add the marker constraint if provided
        if let Some((key, value)) = marker_constraint {
            cfg.constraints.insert(key.clone(), value.clone());
        }

        let cfg = ConfigurationData::from_platform(
            TargetLabel::to_string(target),
            cfg,
            marker_constraint.is_some(),
        )?;
        Ok(ExecutionPlatform::platform(
            target.dupe(),
            cfg,
            self.executor_config.0.dupe(),
        ))
    }
}

#[starlark_module]
fn info_creator(globals: &mut GlobalsBuilder) {
    fn ExecutionPlatformInfo<'v>(
        #[starlark(require = named)] label: ValueTyped<'v, StarlarkTargetLabel>,
        #[starlark(require = named)] configuration: ValueTyped<'v, ConfigurationInfo<'v>>,
        #[starlark(require = named)] executor_config: ValueTyped<'v, StarlarkCommandExecutorConfig>,
    ) -> starlark::Result<ExecutionPlatformInfo<'v>> {
        let info = ExecutionPlatformInfo {
            label,
            configuration,
            executor_config,
        };
        // Surface a configuration that cannot be turned into an `ExecutionPlatform` here rather
        // than wherever the provider is eventually used.
        info.to_execution_platform()?;
        Ok(info)
    }
}
