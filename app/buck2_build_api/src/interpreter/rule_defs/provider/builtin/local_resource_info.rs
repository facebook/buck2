/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api_derive::internal_provider;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_interpreter::types::label::Label;
use dupe::Dupe;
use indexmap::IndexMap;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::dict::DictRef;
use starlark::values::type_repr::DictType;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::Trace;
use starlark::values::Value;

use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::StarlarkCommandLine;
use crate::starlark::values::ValueLike;

#[internal_provider(local_resource_info_creator)]
#[derive(Clone, Debug, Freeze, Coerce, Trace, ProvidesStaticType, Allocative)]
#[freeze(validator = validate_local_resource_info, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct LocalResourceInfoGen<V> {
    /// Configured target that is providing this local resource.
    #[provider(field_type = "Label")]
    source_target: V,
    /// Command to run to initialize a local resource.
    /// Running this command writes a JSON to stdout.
    /// This JSON represents a pool of local resources which are ready to be used.
    /// Example JSON would be:
    /// {
    ///   "pid": 42,
    ///   "resources": [
    ///     {"socket_address": "foo:1"},
    ///     {"socket_address": "bar:2"}
    ///   ]
    /// }
    /// Where '"pid"` maps to a PID of a process which should be sent SIGTERM to release the pool of resources
    /// when they are no longer needed. `"resources"` maps to the pool of resources.
    /// When a local resource from this particular pool is needed for an execution command, single entity
    /// will be reserved from the pool, for example `{"socket_address": "bar:2"}` and environment variable with
    /// name resolved using mapping in `resource_env_vars` field and `"socket_address"` key will be added to
    /// execution command.
    #[provider(field_type = "StarlarkCommandLine")]
    setup: V,
    /// Mapping from environment variable (appended to an execution command which is dependent on this local resource)
    /// to keys in setup command JSON output.
    #[provider(field_type = "DictType<String, String>")]
    resource_env_vars: V,
}

fn validate_local_resource_info<'v, V>(info: &LocalResourceInfoGen<V>) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    _ = Label::from_value(info.source_target.to_value()).with_context(|| {
        format!(
            "Value for `source_target` field is not a label: `{}`",
            info.source_target
        )
    })?;
    let setup = StarlarkCommandLine::try_from_value(info.setup.to_value()).with_context(|| {
        format!(
            "Value for `setup` field is not a command line: `{}`",
            info.setup
        )
    })?;
    if setup.is_empty() {
        return Err(anyhow::anyhow!(
            "Value for `setup` field is an empty command line: `{}`",
            info.setup
        ));
    }

    let env_vars = DictRef::from_value(info.resource_env_vars.to_value()).with_context(|| {
        format!(
            "Value for `resource_env_vars` field is not a dictionary: `{}`",
            info.resource_env_vars
        )
    })?;

    if env_vars.iter().count() == 0 {
        return Err(anyhow::anyhow!(
            "Value for `resource_env_vars` field is an empty dictionary: `{}`",
            info.resource_env_vars
        ));
    }

    let validation_iter = env_vars.iter().map(|(key, value)| {
        _ = key.unpack_str().with_context(|| {
            format!(
                "Invalid key in `resource_env_vars`: Expected a str, got: `{}`",
                key
            )
        })?;

        _ = value.unpack_str().with_context(|| {
            format!(
                "Invalid value in `resource_env_vars`: Expected a str, got: `{}`",
                value
            )
        })?;

        Ok::<(), anyhow::Error>(())
    });

    for validation_item in validation_iter {
        validation_item?;
    }

    Ok(())
}

#[starlark_module]
fn local_resource_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(dot_type = "LocalResourceInfo")]
    fn LocalResourceInfo<'v>(
        #[starlark(require = named)] source_target: Value<'v>,
        #[starlark(require = named)] setup: Value<'v>,
        #[starlark(require = named)] resource_env_vars: Value<'v>,
        _eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<LocalResourceInfo<'v>> {
        let result = LocalResourceInfo {
            source_target,
            setup,
            resource_env_vars,
        };
        validate_local_resource_info(&result)?;
        Ok(result)
    }
}

impl FrozenLocalResourceInfo {
    /// Configured target that is providing this local resource.
    pub fn source_target_label(&self) -> ConfiguredTargetLabel {
        self.source_target
            .downcast_ref::<Label>()
            .unwrap()
            .label()
            .target()
            .dupe()
    }

    /// Mapping from keys in setup command JSON output to environment variables keys which
    /// should be appended to execution commands dependent on this local resource.
    pub fn env_var_mapping(&self) -> IndexMap<String, String> {
        let env_vars = DictRef::from_value(self.resource_env_vars.to_value()).unwrap();
        env_vars
            .iter()
            .map(|(k, v)| {
                (
                    k.unpack_str().unwrap().to_owned(),
                    v.unpack_str().unwrap().to_owned(),
                )
            })
            .collect()
    }

    pub fn setup_command_line(&self) -> &dyn CommandLineArgLike {
        self.setup.to_value().as_command_line().unwrap()
    }
}
