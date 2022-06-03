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
use buck2_core::{configuration::Configuration, target::TargetLabel};
use gazebo::{any::ProvidesStaticType, coerce::Coerce, prelude::*};
use starlark::{
    environment::GlobalsBuilder,
    values::{Freeze, Trace, Value, ValueLike},
};
use thiserror::Error;

use crate::{
    configuration::execution::ExecutionPlatform,
    interpreter::rule_defs::{
        command_executor_config::StarlarkCommandExecutorConfigLike,
        provider::configuration_info::ConfigurationInfo, target_label::StarlarkTargetLabel,
    },
};

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
    label: V, // StarlarkTargetLabel
    /// The configuration of the execution platform
    configuration: V, // ConfigurationInfo
    /// The exedcutor config
    executor_config: V, // StarlarkCommandExecutorConfig
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
        Ok(ExecutionPlatform::Platform {
            target,
            cfg,
            executor_config: executor_config.into_owned(),
        })
    }
}

#[starlark_module]
fn info_creator(globals: &mut GlobalsBuilder) {
    fn ExecutionPlatformInfo<'v>(
        label: Value,
        configuration: Value,
        executor_config: Value,
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
