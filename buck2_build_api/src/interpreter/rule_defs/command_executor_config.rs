use std::fmt;

use anyhow::Context as _;
use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::Coerce,
};
use indexmap::IndexMap;
use starlark::{
    environment::GlobalsBuilder,
    values::{
        dict::Dict, none::NoneType, Freeze, NoSerialize, StarlarkValue, Trace, Value, ValueLike,
    },
};
use thiserror::Error;

use crate::execute::{
    CommandExecutorConfig, LocalExecutorOptions, RemoteExecutorOptions, RemoteExecutorUseCase,
};

#[derive(Debug, Error)]
enum CommandExecutorConfigErrors {
    #[error("expected a dict, got `{0}` (type `{1}`)")]
    RePropertiesNotADict(String, String),
}

#[derive(Clone, Debug, Trace, Coerce, Freeze, AnyLifetime)]
#[derive(NoSerialize)]
#[repr(C)]
pub(crate) struct StarlarkCommandExecutorConfigGen<V> {
    /// Whether to use remote execution for this execution platform
    pub(super) remote_enabled: V, // bool
    /// Whether to use local execution for this execution platform. If both
    /// remote_enabled and local_enabled are `True`, we will use the hybrid executor.
    pub(super) local_enabled: V, // bool
    /// properties for remote execution for this platform
    pub(super) remote_execution_properties: V, // Dict<String, String>
    /// A component to inject into the action key. This should typically used to inject variability
    /// into the action key so that it's different across e.g. build modes (RE uses the action key
    /// for things like expected memory utilization).
    pub(super) remote_execution_action_key: V, // [String, None]
    /// The maximum input file size (in bytes) that remote execution can support.
    pub(super) remote_execution_max_input_files_mebibytes: V, // [Number, None]
    /// The use case to use when communicating with RE.
    pub(super) remote_execution_use_case: V, // [String, None]
    /// Whether to use the limited hybrid executor
    pub(super) use_limited_hybrid: V, // bool
}

impl<'v, V: ValueLike<'v>> fmt::Display for StarlarkCommandExecutorConfigGen<V> {
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
            "remote_execution_max_input_files_mebibytes = {}, ",
            self.remote_execution_max_input_files_mebibytes
        )?;
        write!(
            f,
            "remote_execution_use_case = {}",
            self.remote_execution_use_case
        )?;
        write!(f, "use_limited_hybrid = {}", self.use_limited_hybrid)?;
        write!(f, ")")?;
        Ok(())
    }
}

starlark_complex_value!(pub(crate) StarlarkCommandExecutorConfig);

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkCommandExecutorConfigGen<V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
{
    starlark_type!("command_executor_config");
}

impl<'v, V: ValueLike<'v>> StarlarkCommandExecutorConfigGen<V> {
    pub fn to_command_executor_config(&self) -> anyhow::Result<CommandExecutorConfig> {
        let allow_full_hybrid = !self.use_limited_hybrid.to_value().to_bool();
        let local_options = if self.local_enabled.to_value().to_bool() {
            Some(LocalExecutorOptions {})
        } else {
            None
        };
        let remote_options = if self.remote_enabled.to_value().to_bool() {
            let mut re_properties = IndexMap::new();
            let as_dict = Dict::from_value(self.remote_execution_properties.to_value())
                .ok_or_else(|| {
                    CommandExecutorConfigErrors::RePropertiesNotADict(
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
                        u64::try_from(v)
                            .context("remote_execution_max_input_files_mebibytes is negative")
                    })
                    .context("remote_execution_max_input_files_mebibytes is invalid")?;
                Some(re_max_input_files_mebibytes * 1024 * 1024)
            };

            let re_use_case = self.remote_execution_use_case.to_value();
            let re_use_case = if re_use_case.is_none() {
                Default::default()
            } else {
                RemoteExecutorUseCase::new(re_use_case.to_str())
            };

            Some(RemoteExecutorOptions {
                re_properties,
                re_action_key,
                re_max_input_files_bytes,
                re_use_case,
            })
        } else {
            None
        };

        CommandExecutorConfig::new(local_options, remote_options, allow_full_hybrid)
    }
}

#[starlark_module]
pub fn register_command_executor_config(builder: &mut GlobalsBuilder) {
    #[starlark(type = "command_executor_config")]
    fn CommandExecutorConfig<'v>(
        local_enabled: Value<'v>,
        remote_enabled: Value<'v>,
        #[starlark(default = NoneType, require = named)] remote_execution_properties: Value<'v>,
        #[starlark(default = NoneType, require = named)] remote_execution_action_key: Value<'v>,
        #[starlark(default = NoneType, require = named)] remote_execution_max_input_files_mebibytes: Value<'v>,
        #[starlark(default = NoneType, require = named)] remote_execution_use_case: Value<'v>,
        #[starlark(default = NoneType, require = named)] use_limited_hybrid: Value<'v>,
    ) -> anyhow::Result<StarlarkCommandExecutorConfig<'v>> {
        let config = StarlarkCommandExecutorConfig {
            remote_enabled,
            local_enabled,
            remote_execution_properties,
            remote_execution_action_key,
            remote_execution_max_input_files_mebibytes,
            remote_execution_use_case,
            use_limited_hybrid,
        };
        // This checks that the values are valid.
        config.to_command_executor_config()?;
        Ok(config)
    }
}
