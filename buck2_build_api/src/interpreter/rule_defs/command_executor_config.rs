use std::borrow::Cow;
use std::fmt;

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
use gazebo::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::values::dict::Dict;
use starlark::values::none::NoneOr;
use starlark::values::none::NoneType;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

#[derive(Debug, Error)]
enum CommandExecutorConfigErrors {
    #[error("expected a dict, got `{0}` (type `{1}`)")]
    RePropertiesNotADict(String, String),
}

#[derive(Clone, Debug, Trace, ProvidesStaticType)]
#[derive(NoSerialize)]
struct StarlarkCommandExecutorConfig<'v> {
    /// Whether to use remote execution for this execution platform
    pub(super) remote_enabled: bool,
    /// Whether to use local execution for this execution platform. If both
    /// remote_enabled and local_enabled are `True`, we will use the hybrid executor.
    pub(super) local_enabled: bool,
    /// properties for remote execution for this platform
    pub(super) remote_execution_properties: Value<'v>,
    /// A component to inject into the action key. This should typically used to inject variability
    /// into the action key so that it's different across e.g. build modes (RE uses the action key
    /// for things like expected memory utilization).
    pub(super) remote_execution_action_key: Value<'v>, // [String, None]
    /// The maximum input file size (in bytes) that remote execution can support.
    pub(super) remote_execution_max_input_files_mebibytes: Option<i32>,
    /// The use case to use when communicating with RE.
    pub(super) remote_execution_use_case: Value<'v>, // String
    /// Whether to use the limited hybrid executor
    pub(super) use_limited_hybrid: bool,
    /// Whether to allow fallbacks
    pub(super) allow_limited_hybrid_fallbacks: bool,
    /// Whether to allow fallbacks when the result is failure (i.e. the command failed on the
    /// primary, but the infra worked).
    pub(super) allow_hybrid_fallbacks_on_failure: bool,
    /// Whether to use Windows path separators in command line arguments.
    pub(super) use_windows_path_separators: bool,
    /// Whether to upload local actions to the RE cache
    pub(super) allow_cache_uploads: bool,
}

impl<'v> fmt::Display for StarlarkCommandExecutorConfig<'v> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "CommandExecutorConfig(")?;
        write!(f, "remote_enabled = {}, ", self.remote_enabled)?;
        write!(f, "local_enabled = {}, ", self.local_enabled)?;
        write!(
            f,
            "remote_execution_properties = {}, ",
            self.remote_execution_properties
        )?;
        write!(
            f,
            "remote_execution_action_key = {}, ",
            self.remote_execution_action_key
        )?;
        write!(
            f,
            "remote_execution_max_input_files_mebibytes = {:?}, ",
            self.remote_execution_max_input_files_mebibytes
        )?;
        write!(
            f,
            "remote_execution_use_case = {}, ",
            self.remote_execution_use_case
        )?;
        write!(f, "use_limited_hybrid = {}, ", self.use_limited_hybrid)?;
        write!(
            f,
            "allow_limited_hybrid_fallbacks = {}, ",
            self.allow_limited_hybrid_fallbacks
        )?;
        write!(
            f,
            "use_windows_path_separators = {}",
            self.use_windows_path_separators
        )?;
        write!(f, ")")?;
        Ok(())
    }
}

impl<'v> StarlarkValue<'v> for StarlarkCommandExecutorConfig<'v> {
    starlark_type!("command_executor_config_builder");
}

impl<'v> StarlarkCommandExecutorConfig<'v> {
    pub fn to_command_executor_config(&self) -> anyhow::Result<CommandExecutorConfig> {
        let local_options = if self.local_enabled {
            Some(LocalExecutorOptions {})
        } else {
            None
        };
        let remote_options = if self.remote_enabled {
            let re_properties = Dict::from_value(self.remote_execution_properties.to_value())
                .ok_or_else(|| {
                    CommandExecutorConfigErrors::RePropertiesNotADict(
                        self.remote_execution_properties.to_value().to_repr(),
                        self.remote_execution_properties
                            .to_value()
                            .get_type()
                            .to_owned(),
                    )
                })?;
            let re_properties = re_properties
                .iter()
                .map(|(k, v)| (k.to_str(), v.to_str()))
                .collect();

            let re_action_key = self.remote_execution_action_key.to_value();
            let re_action_key = if re_action_key.is_none() {
                None
            } else {
                Some(re_action_key.to_value().to_str())
            };

            let re_max_input_files_bytes = self
                .remote_execution_max_input_files_mebibytes
                .map(u64::try_from)
                .transpose()
                .context("remote_execution_max_input_files_mebibytes is negative")?
                .map(|b| b * 1024 * 1024);

            let re_use_case = self
                .remote_execution_use_case
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

        let fallback_on_failure = self.allow_hybrid_fallbacks_on_failure;

        let hybrid_level = match (self.use_limited_hybrid, self.allow_limited_hybrid_fallbacks) {
            (true, true) => HybridExecutionLevel::Fallback {
                fallback_on_failure,
            },
            (true, false) => HybridExecutionLevel::Limited,
            (false, _) => HybridExecutionLevel::Full {
                fallback_on_failure,
            },
        };

        Ok(CommandExecutorConfig {
            executor_kind: CommandExecutorKind::new(local_options, remote_options, hybrid_level)?,
            path_separator: if self.use_windows_path_separators {
                PathSeparatorKind::Windows
            } else {
                PathSeparatorKind::Unix
            },
            cache_upload_behavior: if self.allow_cache_uploads {
                CacheUploadBehavior::Enabled
            } else {
                CacheUploadBehavior::Disabled
            },
        })
    }
}

impl<'v> Freeze for StarlarkCommandExecutorConfig<'v> {
    type Frozen = FrozenStarlarkCommandExecutorConfig;

    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let inner = self.to_command_executor_config()?;
        Ok(FrozenStarlarkCommandExecutorConfig(inner))
    }
}

#[derive(Debug, Display, NoSerialize, ProvidesStaticType)]
#[display(fmt = "{:?}", _0)]
pub(crate) struct FrozenStarlarkCommandExecutorConfig(CommandExecutorConfig);

starlark_simple_value!(FrozenStarlarkCommandExecutorConfig);

impl<'v> StarlarkValue<'v> for FrozenStarlarkCommandExecutorConfig {
    starlark_type!("command_executor_config");
}

pub trait StarlarkCommandExecutorConfigLike<'v> {
    fn command_executor_config(&'v self) -> anyhow::Result<Cow<'v, CommandExecutorConfig>>;
}

impl<'v> dyn StarlarkCommandExecutorConfigLike<'v> {
    pub fn from_value(v: Value<'v>) -> Option<&'v dyn StarlarkCommandExecutorConfigLike<'v>> {
        if let Some(r) = v.downcast_ref::<StarlarkCommandExecutorConfig<'v>>() {
            return Some(r as _);
        }

        if let Some(r) = v.downcast_ref::<FrozenStarlarkCommandExecutorConfig>() {
            return Some(r as _);
        }

        None
    }
}

impl<'v> StarlarkCommandExecutorConfigLike<'v> for StarlarkCommandExecutorConfig<'v> {
    fn command_executor_config(&'v self) -> anyhow::Result<Cow<'v, CommandExecutorConfig>> {
        Ok(Cow::Owned(self.to_command_executor_config()?))
    }
}

impl<'v> StarlarkCommandExecutorConfigLike<'v> for FrozenStarlarkCommandExecutorConfig {
    fn command_executor_config(&'v self) -> anyhow::Result<Cow<'v, CommandExecutorConfig>> {
        Ok(Cow::Borrowed(&self.0))
    }
}

#[starlark_module]
pub fn register_command_executor_config(builder: &mut GlobalsBuilder) {
    #[starlark(type = "command_executor_config")]
    fn CommandExecutorConfig<'v>(
        local_enabled: bool,
        remote_enabled: bool,
        #[starlark(default = NoneType, require = named)] remote_execution_properties: Value<'v>,
        #[starlark(default = NoneType, require = named)] remote_execution_action_key: Value<'v>,
        #[starlark(default = NoneOr::None, require = named)]
        remote_execution_max_input_files_mebibytes: NoneOr<i32>,
        #[starlark(default = NoneType, require = named)] remote_execution_use_case: Value<'v>,
        #[starlark(default = false, require = named)] use_limited_hybrid: bool,
        #[starlark(default = false, require = named)] allow_limited_hybrid_fallbacks: bool,
        #[starlark(default = false, require = named)] allow_hybrid_fallbacks_on_failure: bool,
        #[starlark(default = false, require = named)] use_windows_path_separators: bool,
        #[starlark(default = false, require = named)] allow_cache_uploads: bool,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let config = StarlarkCommandExecutorConfig {
            remote_enabled,
            local_enabled,
            remote_execution_properties,
            remote_execution_action_key,
            remote_execution_max_input_files_mebibytes: remote_execution_max_input_files_mebibytes
                .into_option(),
            remote_execution_use_case,
            use_limited_hybrid,
            allow_limited_hybrid_fallbacks,
            allow_hybrid_fallbacks_on_failure,
            use_windows_path_separators,
            allow_cache_uploads,
        };
        // This checks that the values are valid.
        config.to_command_executor_config()?;
        Ok(heap.alloc_complex(config))
    }
}
