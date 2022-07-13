/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_build_api_derive::internal_provider;
use buck2_core::configuration::Configuration;
use buck2_core::target::TargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::configuration::execution::ExecutionPlatform;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::Coerce;
use gazebo::prelude::*;
use starlark::environment::GlobalsBuilder;
use starlark::values::Freeze;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::interpreter::rule_defs::command_executor_config::FrozenStarlarkCommandExecutorConfig;
use crate::interpreter::rule_defs::command_executor_config::StarlarkCommandExecutorConfigLike;
use crate::interpreter::rule_defs::provider::builtin::configuration_info::ConfigurationInfo;

#[derive(Debug, Error)]
enum ExecutionPlatformProviderErrors {
    #[error("expected a label, got `{0}` (type `{1}`)")]
    ExpectedLabel(String, String),
    #[error("expected a ConfigurationInfo, got `{0}` (type `{1}`)")]
    ExpectedConfigurationInfo(String, String),
    #[error("expected a CommandExecutorConfig, got `{0}` (type `{1}`)")]
    ExpectedCommandExecutorConfig(String, String),
}

/// Provider that signals that a target represents an execution platform.
#[internal_provider(info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(C)]
pub(crate) struct ExecutionPlatformInfoGen<V> {
    /// label of the defining rule, used in informative messages
    #[provider(field_type = "StarlarkTargetLabel")]
    label: V,
    /// The configuration of the execution platform
    #[provider(field_type = "ConfigurationInfo")]
    configuration: V,
    /// The executor config
    #[provider(field_type = "FrozenStarlarkCommandExecutorConfig")]
    executor_config: V,
}

impl<'v, V: ValueLike<'v>> ExecutionPlatformInfoGen<V> {
    pub fn to_execution_platform(&self) -> anyhow::Result<ExecutionPlatform> {
        let target = StarlarkTargetLabel::from_value(self.label.to_value())
            .ok_or_else(|| {
                ExecutionPlatformProviderErrors::ExpectedLabel(
                    self.label.to_value().to_repr(),
                    self.label.to_value().get_type().to_owned(),
                )
            })?
            .label()
            .dupe();
        let cfg = ConfigurationInfo::from_value(self.configuration.to_value())
            .ok_or_else(|| {
                ExecutionPlatformProviderErrors::ExpectedConfigurationInfo(
                    self.configuration.to_value().to_repr(),
                    self.configuration.to_value().get_type().to_owned(),
                )
            })?
            .to_configuration_data();
        let cfg = Configuration::from_platform(TargetLabel::to_string(&target), cfg)?;
        let executor_config =
            <dyn StarlarkCommandExecutorConfigLike>::from_value(self.executor_config.to_value())
                .ok_or_else(|| {
                    ExecutionPlatformProviderErrors::ExpectedCommandExecutorConfig(
                        self.configuration.to_value().to_repr(),
                        self.configuration.to_value().get_type().to_owned(),
                    )
                })?
                .command_executor_config()?;
        Ok(ExecutionPlatform::platform(
            target,
            cfg,
            executor_config.into_owned(),
        ))
    }
}

#[starlark_module]
fn info_creator(globals: &mut GlobalsBuilder) {
    fn ExecutionPlatformInfo<'v>(
        #[starlark(require = named)] label: Value<'v>,
        #[starlark(require = named)] configuration: Value<'v>,
        #[starlark(require = named)] executor_config: Value<'v>,
    ) -> anyhow::Result<ExecutionPlatformInfo<'v>> {
        let info = ExecutionPlatformInfo {
            label,
            configuration,
            executor_config,
        };
        // This checks that the values are valid.
        info.to_execution_platform()?;
        Ok(info)
    }
}
