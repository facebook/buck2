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
use buck2_common::executor_config::CommandGenerationOptions;
use buck2_common::executor_config::Executor;
use buck2_common::executor_config::HybridExecutionLevel;
use buck2_common::executor_config::LocalExecutorOptions;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_common::executor_config::RemoteEnabledExecutor;
use buck2_common::executor_config::RemoteExecutorOptions;
use buck2_common::executor_config::RemoteExecutorUseCase;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::DictRef;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

#[derive(Debug, Error)]
enum CommandExecutorConfigErrors {
    #[error("expected a dict, got `{0}` (type `{1}`)")]
    RePropertiesNotADict(String, String),
    #[error("expected `{0}` to be set")]
    MissingField(&'static str),
    #[error("invalid value in `{0}`")]
    InvalidField(&'static str),
    #[error(
        "executor config must specify at least `local_enabled = True` or `remote_enabled = True`"
    )]
    NoExecutor,
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
    /// Contains configurations for how actions should be executed
    ///
    /// * `local_enabled` : Whether to use local execution for this execution platform.
    /// If both remote_enabled and local_enabled are `True`, we will use the hybrid executor
    /// * `remote_enabled`: Whether to use remote execution for this execution platform
    /// * `remote_cache_enabled`: Whether to query RE caches
    /// * `remote_execution_properties`: Properties for remote execution for this platform
    /// * `remote_execution_action_key`: A component to inject into the action key
    /// This should typically used to inject variability into the action key so that
    /// it's different across e.g. build modes (RE uses the action key for things like expected memory utilization)
    /// * `remote_execution_max_input_files_mebibytes`: The maximum input file size (in bytes) that remote execution can support
    /// * `remote_execution_queue_time_threshold_s`: The maximum time in seconds we are willing to wait
    /// in the RE queue for remote execution to start running our action
    /// * `remote_execution_use_case`: The use case to use when communicating with RE
    /// * `use_limited_hybrid`: Whether to use the limited hybrid executor
    /// * `allow_limited_hybrid_fallbacks`: Whether to allow fallbacks
    /// * `allow_hybrid_fallbacks_on_failure`: Whether to allow fallbacks when the result is failure (i.e. the command failed on the primary, but the infra worked)
    /// * `use_windows_path_separators`: Whether to use Windows path separators in command line arguments
    /// * `allow_cache_uploads`: Whether to upload local actions to the RE cache
    /// * `max_cache_upload_mebibytes`: Maximum size to upload in cache uploads
    /// * `experimental_low_pass_filter`: Whether to use the experimental low pass filter
    /// * `remote_output_paths`: How to express output paths to RE
    #[starlark(type = "command_executor_config")]
    fn CommandExecutorConfig<'v>(
        local_enabled: bool,
        remote_enabled: bool,
        #[starlark(default = NoneOr::None, require = named)] remote_cache_enabled: NoneOr<bool>,
        #[starlark(default = NoneType, require = named)] remote_execution_properties: Value<'v>,
        #[starlark(default = NoneType, require = named)] remote_execution_action_key: Value<'v>,
        #[starlark(default = NoneOr::None, require = named)]
        remote_execution_max_input_files_mebibytes: NoneOr<i32>,
        #[starlark(default = NoneOr::None, require = named)]
        remote_execution_queue_time_threshold_s: NoneOr<i32>,
        #[starlark(default = NoneType, require = named)] remote_execution_use_case: Value<'v>,
        #[starlark(default = false, require = named)] use_limited_hybrid: bool,
        #[starlark(default = false, require = named)] allow_limited_hybrid_fallbacks: bool,
        #[starlark(default = false, require = named)] allow_hybrid_fallbacks_on_failure: bool,
        #[starlark(default = false, require = named)] use_windows_path_separators: bool,
        #[starlark(default = false, require = named)] allow_cache_uploads: bool,
        #[starlark(default = NoneOr::None, require = named)] max_cache_upload_mebibytes: NoneOr<
            i32,
        >,
        #[starlark(default = false, require = named)] experimental_low_pass_filter: bool,
        #[starlark(default = NoneOr::None, require = named)] remote_output_paths: NoneOr<&str>,
    ) -> anyhow::Result<StarlarkCommandExecutorConfig> {
        let command_executor_config = {
            let remote_execution_max_input_files_mebibytes =
                remote_execution_max_input_files_mebibytes.into_option();

            let remote_execution_queue_time_threshold_s =
                remote_execution_queue_time_threshold_s.into_option();

            let max_cache_upload_mebibytes = max_cache_upload_mebibytes.into_option();

            let re_properties = if remote_execution_properties.is_none() {
                None
            } else {
                let re_properties = DictRef::from_value(remote_execution_properties.to_value())
                    .ok_or_else(|| {
                        CommandExecutorConfigErrors::RePropertiesNotADict(
                            remote_execution_properties.to_value().to_repr(),
                            remote_execution_properties.to_value().get_type().to_owned(),
                        )
                    })?;

                Some(
                    re_properties
                        .iter()
                        .map(|(k, v)| (k.to_str(), v.to_str()))
                        .collect(),
                )
            };

            let re_use_case = if remote_execution_use_case.is_none() {
                None
            } else {
                let re_use_case = remote_execution_use_case
                    .unpack_str()
                    .context("remote_execution_use_case is not a string")?;
                Some(RemoteExecutorUseCase::new(re_use_case.to_owned()))
            };

            let local_options = if local_enabled {
                Some(LocalExecutorOptions {})
            } else {
                None
            };
            let remote_options = if remote_enabled {
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

                let re_max_queue_time_ms = remote_execution_queue_time_threshold_s
                    .map(u64::try_from)
                    .transpose()
                    .context("remote_execution_queue_time_threshold_s is negative")?
                    .map(|t| t * 1000);

                Some(RemoteExecutorOptions {
                    re_action_key,
                    re_max_input_files_bytes,
                    re_max_queue_time_ms,
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

            let cache_upload_behavior = if allow_cache_uploads {
                CacheUploadBehavior::Enabled {
                    max_bytes: max_cache_upload_bytes,
                }
            } else {
                CacheUploadBehavior::Disabled
            };

            // FIXME: This should probably default to `remote_enabled` and not `true`, but
            // historically this has been `true`, so we should probably migrate our defs to set
            // `remote_cache_enabled = True` explicitly first.
            let remote_cache_default = if buck2_core::is_open_source() {
                remote_enabled
            } else {
                true
            };
            let remote_cache_enabled = remote_cache_enabled
                .into_option()
                .unwrap_or(remote_cache_default);

            let executor = match (local_options, remote_options, remote_cache_enabled) {
                (local, Some(remote), remote_cache_enabled) => {
                    let executor = match local {
                        Some(local) => RemoteEnabledExecutor::Hybrid {
                            local,
                            remote,
                            level: hybrid_level,
                        },
                        None => RemoteEnabledExecutor::Remote(remote),
                    };

                    Executor::RemoteEnabled {
                        executor,
                        re_properties: re_properties.context(
                            CommandExecutorConfigErrors::MissingField(
                                "remote_execution_properties",
                            ),
                        )?,
                        re_use_case: re_use_case
                            .context(CommandExecutorConfigErrors::MissingField("re_use_case"))?,
                        cache_upload_behavior,
                        remote_cache_enabled,
                    }
                }
                (Some(local), None, true) => Executor::RemoteEnabled {
                    executor: RemoteEnabledExecutor::Local(local),
                    // FIXME: We need a migration flip the default for remote_cache_enabled to
                    // remote_enabled first.
                    re_properties: re_properties.unwrap_or_default(),
                    re_use_case: re_use_case.unwrap_or_else(RemoteExecutorUseCase::buck2_default),
                    cache_upload_behavior,
                    remote_cache_enabled: true,
                },
                (Some(local), None, false) => Executor::Local(local),
                (None, None, _) => {
                    return Err(CommandExecutorConfigErrors::NoExecutor.into());
                }
            };

            let output_paths_behavior = remote_output_paths
                .into_option()
                .map(|s| s.parse())
                .transpose()
                .context(CommandExecutorConfigErrors::InvalidField(
                    "remote_output_paths",
                ))?
                .unwrap_or_default();

            CommandExecutorConfig {
                executor,
                options: CommandGenerationOptions {
                    path_separator: if use_windows_path_separators {
                        PathSeparatorKind::Windows
                    } else {
                        PathSeparatorKind::Unix
                    },
                    output_paths_behavior,
                },
            }
        };

        Ok(StarlarkCommandExecutorConfig(Arc::new(
            command_executor_config,
        )))
    }
}
