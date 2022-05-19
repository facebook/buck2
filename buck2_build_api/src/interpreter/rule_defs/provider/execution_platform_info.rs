/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use anyhow::Context as _;
use buck2_build_api_derive::internal_provider;
use buck2_core::{configuration::Configuration, target::TargetLabel};
use gazebo::{any::AnyLifetime, coerce::Coerce, dupe::Dupe};
use indexmap::IndexMap;
use starlark::{
    environment::GlobalsBuilder,
    values::{dict::Dict, Freeze, Trace, Value, ValueLike},
};
use thiserror::Error;

use crate::{
    configuration::execution::ExecutionPlatform,
    execute::{ActionExecutorConfig, LocalExecutorOptions, RemoteExecutorOptions},
    interpreter::rule_defs::{
        provider::configuration_info::ConfigurationInfo, target_label::StarlarkTargetLabel,
    },
};

#[derive(Debug, Error)]
enum ExecutionPlatformProviderErrors {
    #[error("expected a label, got `{0}` (type `{1}`)")]
    ExpectedLabel(String, String),
    #[error("expected a ConfigurationInfo, got `{0}` (type `{1}`)")]
    ExpectedConfigurationInfo(String, String),
    #[error("expected a dict, got `{0}` (type `{1}`)")]
    RePropertiesNotADict(String, String),
}

/// Provider that signals that a target represents an execution platform.
#[internal_provider(info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, AnyLifetime)]
#[repr(C)]
pub(crate) struct ExecutionPlatformInfoGen<V> {
    /// label of the defining rule, used in informative messages
    label: V, // StarlarkTargetLabel
    /// The configuration of the execution platform
    configuration: V, // ConfigurationInfo
    /// Whether to use remote execution for this execution platform
    remote_enabled: V, // bool
    /// Whether to use local execution for this execution platform. If both
    /// remote_enabled and local_enabled are `True`, we will use the hybrid executor.
    local_enabled: V, // bool
    /// properties for remote execution for this platform
    remote_execution_properties: V, // Dict<String, String>
    /// A component to inject into the action key. This should typically used to inject variability
    /// into the action key so that it's different across e.g. build modes (RE uses the action key
    /// for things like expected memory utilization).
    remote_execution_action_key: V, // [String, None]
    /// The maximum input file size (in bytes) that remote execution can support.
    remote_execution_max_input_files_mebibytes: V, // [Number, None]
    /// Whether to use the limited hybrid executor
    use_limited_hybrid: V, // bool
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
        let allow_full_hybrid = !self.use_limited_hybrid.to_value().to_bool();
        Ok(ExecutionPlatform::Platform {
            target,
            cfg,
            executor_config: {
                let local_options = if self.local_enabled.to_value().to_bool() {
                    Some(LocalExecutorOptions {})
                } else {
                    None
                };
                let remote_options = if self.remote_enabled.to_value().to_bool() {
                    let mut re_properties = IndexMap::new();
                    let as_dict = Dict::from_value(self.remote_execution_properties.to_value())
                        .ok_or_else(|| {
                            ExecutionPlatformProviderErrors::RePropertiesNotADict(
                                self.remote_execution_properties.to_value().to_repr(),
                                self.remote_execution_properties
                                    .to_value()
                                    .get_type()
                                    .to_owned(),
                            )
                        })?;

                    for (key, value) in as_dict.iter() {
                        re_properties.insert(key.to_str(), value.to_str());
                    }

                    let re_action_key = self.remote_execution_action_key.to_value();
                    let re_action_key = if re_action_key.is_none() {
                        None
                    } else {
                        Some(re_action_key.to_value().to_str())
                    };

                    let re_max_input_files_mebibytes =
                        self.remote_execution_max_input_files_mebibytes.to_value();
                    let re_max_input_files_bytes = if re_max_input_files_mebibytes.is_none() {
                        None
                    } else {
                        let re_max_input_files_mebibytes = re_max_input_files_mebibytes
                            .to_value()
                            .to_int()
                            .and_then(|v| {
                                u64::try_from(v).context(
                                    "remote_execution_max_input_files_mebibytes is negative",
                                )
                            })
                            .context("remote_execution_max_input_files_mebibytes is invalid")?;
                        Some(re_max_input_files_mebibytes * 1024 * 1024)
                    };

                    Some(RemoteExecutorOptions {
                        re_properties,
                        re_action_key,
                        re_max_input_files_bytes,
                    })
                } else {
                    None
                };
                ActionExecutorConfig::new(local_options, remote_options, allow_full_hybrid)?
            },
        })
    }
}

#[starlark_module]
fn info_creator(globals: &mut GlobalsBuilder) {
    fn ExecutionPlatformInfo<'v>(
        label: Value,
        configuration: Value,
        remote_enabled: Value,
        local_enabled: Value,
        remote_execution_properties: Value,
        remote_execution_action_key: Option<Value>,
        remote_execution_max_input_files_mebibytes: Option<Value>,
        use_limited_hybrid: Option<Value>,
    ) -> anyhow::Result<ExecutionPlatformInfo<'v>> {
        let use_limited_hybrid = use_limited_hybrid.unwrap_or_else(Value::new_none);
        let remote_execution_action_key =
            remote_execution_action_key.unwrap_or_else(Value::new_none);
        let remote_execution_max_input_files_mebibytes =
            remote_execution_max_input_files_mebibytes.unwrap_or_else(Value::new_none);
        let info = ExecutionPlatformInfo {
            label,
            configuration,
            remote_enabled,
            local_enabled,
            remote_execution_properties,
            remote_execution_action_key,
            remote_execution_max_input_files_mebibytes,
            use_limited_hybrid,
        };
        // This checks that the values are valid.
        info.to_execution_platform()?;
        Ok(info)
    }
}
