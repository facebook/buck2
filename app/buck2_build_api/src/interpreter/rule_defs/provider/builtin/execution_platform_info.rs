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
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::command_executor_config::StarlarkCommandExecutorConfig;
use crate::interpreter::rule_defs::provider::builtin::configuration_info::ConfigurationInfo;
use crate::interpreter::rule_defs::provider::builtin::configuration_info::FrozenConfigurationInfo;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum ExecutionPlatformProviderErrors {
    #[error("expected a ConfigurationInfo, got `{0}` (type `{1}`)")]
    ExpectedConfigurationInfo(String, String),
    #[error("expected a CommandExecutorConfig, got `{0}` (type `{1}`)")]
    ExpectedCommandExecutorConfig(String, String),
}

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
    label: ValueOfUnchecked<'v, StarlarkTargetLabel>,
    /// The configuration of the execution platform
    configuration: ValueOfUnchecked<'v, FrozenConfigurationInfo>,
    /// The executor config
    executor_config: ValueOfUnchecked<'v, StarlarkCommandExecutorConfig>,
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
        let target = self.label.cast::<&StarlarkTargetLabel>().unpack()?.label();
        let mut cfg = ConfigurationInfo::from_value(self.configuration.get())
            .ok_or_else(|| {
                ExecutionPlatformProviderErrors::ExpectedConfigurationInfo(
                    self.configuration.get().to_repr(),
                    self.configuration.get().get_type().to_owned(),
                )
            })?
            .to_configuration_data()?;

        // Add the marker constraint if provided
        if let Some((key, value)) = marker_constraint {
            cfg.constraints.insert(key.clone(), value.clone());
        }

        let cfg = ConfigurationData::from_platform(
            TargetLabel::to_string(target),
            cfg,
            marker_constraint.is_some(),
        )?;
        let executor_config = StarlarkCommandExecutorConfig::from_value(self.executor_config.get())
            .ok_or_else(|| {
                ExecutionPlatformProviderErrors::ExpectedCommandExecutorConfig(
                    self.executor_config.get().to_repr(),
                    self.executor_config.get().get_type().to_owned(),
                )
            })?
            .0
            .dupe();
        Ok(ExecutionPlatform::platform(
            target.dupe(),
            cfg,
            executor_config,
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
            label: label.to_value_of_unchecked(),
            configuration: ValueOfUnchecked::new(configuration.to_value()),
            executor_config: executor_config.to_value_of_unchecked(),
        };
        // This checks that the values are valid.
        info.to_execution_platform()?;
        Ok(info)
    }
}
