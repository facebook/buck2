/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use buck2_common::executor_config::CacheUploadBehavior;
use buck2_common::executor_config::CommandExecutorConfig;
use buck2_common::executor_config::CommandExecutorKind;
use buck2_common::executor_config::HybridExecutionLevel;
use buck2_common::executor_config::LocalExecutorOptions;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::executor_config::RemoteExecutorOptions;
use buck2_common::executor_config::RemoteExecutorUseCase;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictRef;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

#[derive(Debug, Error)]
enum CommandExecutorConfigErrors {
    #[error("expected a dict, got `{0}` (type `{1}`)")]
    RePropertiesNotADict(String, String),
}

#[derive(Debug, Display, NoSerialize, ProvidesStaticType, Allocative)]
#[display(fmt = "{:?}", _0)]
pub struct StarlarkCommandExecutorConfig(pub Arc<CommandExecutorConfig>);

starlark_simple_value!(StarlarkCommandExecutorConfig);

impl<'v> StarlarkValue<'v> for StarlarkCommandExecutorConfig {
    starlark_type!("command_executor_config");
}

#[starlark_module]
pub fn register_command_executor_config(builder: &mut GlobalsBuilder) {
    #[starlark(type = "command_executor_config")]
    fn CommandExecutorConfig<'v>(
        // Whether to use local execution for this execution platform. If both
        // remote_enabled and local_enabled are `True`, we will use the hybrid executor.
        local_enabled: bool,
        // Whether to use remote execution for this execution platform
        remote_enabled: bool,
        // properties for remote execution for this platform
        #[starlark(default = NoneType, require = named)] remote_execution_properties: Value<'v>,
        // A component to inject into the action key. This should typically used to inject variability
        // into the action key so that it's different across e.g. build modes (RE uses the action key
        // for things like expected memory utilization).
        #[starlark(default = NoneType, require = named)] remote_execution_action_key: Value<'v>,
        // The maximum input file size (in bytes) that remote execution can support.
        #[starlark(default = NoneOr::None, require = named)]
        remote_execution_max_input_files_mebibytes: NoneOr<i32>,
        // The use case to use when communicating with RE.
        #[starlark(default = NoneType, require = named)] remote_execution_use_case: Value<'v>,
        // Whether to use the limited hybrid executor
        #[starlark(default = false, require = named)] use_limited_hybrid: bool,
        // Whether to allow fallbacks
        #[starlark(default = false, require = named)] allow_limited_hybrid_fallbacks: bool,
        // Whether to allow fallbacks when the result is failure (i.e. the command failed on the
        // primary, but the infra worked).
        #[starlark(default = false, require = named)] allow_hybrid_fallbacks_on_failure: bool,
        // Whether to use Windows path separators in command line arguments.
        #[starlark(default = false, require = named)] use_windows_path_separators: bool,
        // Whether to upload local actions to the RE cache
        #[starlark(default = false, require = named)] allow_cache_uploads: bool,
        // Maximum size to upload in cache uploads
        #[starlark(default = NoneOr::None, require = named)] max_cache_upload_mebibytes: NoneOr<
            i32,
        >,
        // Whether to use the experimental low pass filter.
        #[starlark(default = false, require = named)] experimental_low_pass_filter: bool,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let command_executor_config = {
            let remote_execution_max_input_files_mebibytes =
                remote_execution_max_input_files_mebibytes.into_option();

            let max_cache_upload_mebibytes = max_cache_upload_mebibytes.into_option();

            let local_options = if local_enabled {
                Some(LocalExecutorOptions {})
            } else {
                None
            };
            let remote_options = if remote_enabled {
                let re_properties = DictRef::from_value(remote_execution_properties.to_value())
                    .ok_or_else(|| {
                        CommandExecutorConfigErrors::RePropertiesNotADict(
                            remote_execution_properties.to_value().to_repr(),
                            remote_execution_properties.to_value().get_type().to_owned(),
                        )
                    })?;
                let re_properties = re_properties
                    .iter()
                    .map(|(k, v)| (k.to_str(), v.to_str()))
                    .collect();

                let re_action_key = remote_execution_action_key.to_value();
                let re_action_key = if re_action_key.is_none() {
                    None
                } else {
                    Some(re_action_key.to_value().to_str())
                };

                let re_max_input_files_bytes = remote_execution_max_input_files_mebibytes
                    .map(u64::try_from)
                    .transpose()
                    .context("remote_execution_max_input_files_mebibytes is negative")?
                    .map(|b| b * 1024 * 1024);

                let re_use_case = remote_execution_use_case
                    .unpack_str()
                    .context("remote_execution_use_case is missing")?;
                let re_use_case = RemoteExecutorUseCase::new(re_use_case.to_owned());

                Some(RemoteExecutorOptions {
                    re_properties,
                    re_action_key,
                    re_max_input_files_bytes,
                    re_use_case,
                })
            } else {
                None
            };

            let fallback_on_failure = allow_hybrid_fallbacks_on_failure;

            let hybrid_level = match (use_limited_hybrid, allow_limited_hybrid_fallbacks) {
                (true, true) => HybridExecutionLevel::Fallback {
                    fallback_on_failure,
                },
                (true, false) => HybridExecutionLevel::Limited,
                (false, _) => HybridExecutionLevel::Full {
                    fallback_on_failure,
                    low_pass_filter: experimental_low_pass_filter,
                },
            };

            let max_cache_upload_bytes = max_cache_upload_mebibytes
                .map(u64::try_from)
                .transpose()
                .context("max_cache_upload_mebibytes is negative")?
                .map(|b| b * 1024 * 1024);

            CommandExecutorConfig {
                executor_kind: CommandExecutorKind::new(
                    local_options,
                    remote_options,
                    hybrid_level,
                )?,
                path_separator: if use_windows_path_separators {
                    PathSeparatorKind::Windows
                } else {
                    PathSeparatorKind::Unix
                },
                cache_upload_behavior: if allow_cache_uploads {
                    CacheUploadBehavior::Enabled {
                        max_bytes: max_cache_upload_bytes,
                    }
                } else {
                    CacheUploadBehavior::Disabled
                },
            }
        };

        Ok(heap.alloc_simple(StarlarkCommandExecutorConfig(Arc::new(
            command_executor_config,
        ))))
    }
}
