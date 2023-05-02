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
    #[starlark(type = "LocalResourceInfo")]
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
    pub fn env_var_mapping(&self) -> IndexMap<&str, &str> {
        let env_vars = DictRef::from_value(self.resource_env_vars.to_value()).unwrap();
        env_vars
            .iter()
            .map(|(k, v)| (k.unpack_str().unwrap(), v.unpack_str().unwrap()))
            .collect()
    }

    pub fn setup_command_line(&self) -> &dyn CommandLineArgLike {
        self.setup.to_value().as_command_line().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::bzl::ImportPath;
    use buck2_interpreter_for_build::interpreter::testing::expect_error;
    use buck2_interpreter_for_build::interpreter::testing::Tester;
    use buck2_interpreter_for_build::label::testing::label_creator;
    use indoc::indoc;

    use crate::interpreter::rule_defs::register_rule_defs;

    fn new_tester() -> Tester {
        let mut tester = Tester::new().unwrap();
        tester.additional_globals(label_creator);
        tester.additional_globals(register_rule_defs);
        tester
    }

    #[test]
    fn test_construction() -> anyhow::Result<()> {
        let mut tester = new_tester();
        let test = indoc!(
            r#"
            def test():
                target = label("//:foobar")
                LocalResourceInfo(source_target=target, setup=["/foo", "--resource"], resource_env_vars={"RESOURCE_ENV_VAR": "json_key"})
                LocalResourceInfo(source_target=target, setup=cmd_args(["/foo", "--resource"]), resource_env_vars={"RESOURCE_ENV_VAR": "json_key"})
            "#
        );
        tester.run_starlark_bzl_test(test)?;
        Ok(())
    }

    #[test]
    fn test_missing_fields_validation() -> anyhow::Result<()> {
        let mut tester = new_tester();
        {
            let test = indoc!(
                r#"
                def test():
                    LocalResourceInfo(setup=cmd_args(), resource_env_vars={})
                "#
            );
            expect_error(tester.run_starlark_bzl_test(test), test, "`source_target`");
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    LocalResourceInfo(source_target=target, resource_env_vars={})
                "#
            );
            expect_error(tester.run_starlark_bzl_test(test), test, "`setup`");
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    LocalResourceInfo(source_target=target, setup=cmd_args())
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "`resource_env_vars`",
            );
        }
        Ok(())
    }

    #[test]
    fn test_validation() -> anyhow::Result<()> {
        let mut tester = new_tester();

        {
            let test = indoc!(
                r#"
                def test():
                    wrong_target = 42
                    LocalResourceInfo(source_target=wrong_target, setup=["/foo", "--resource"], resource_env_vars={"RESOURCE_ENV_VAR": "json_key"})
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Value for `source_target` field is not a label",
            );
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    wrong_setup = {5:6}
                    LocalResourceInfo(source_target=target, setup=wrong_setup, resource_env_vars={"RESOURCE_ENV_VAR": "json_key"})
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Value for `setup` field is not a command line",
            );
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    wrong_setup = []
                    LocalResourceInfo(source_target=target, setup=wrong_setup, resource_env_vars={"RESOURCE_ENV_VAR": "json_key"})
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Value for `setup` field is an empty command line",
            );
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    wrong_env_vars = "baz"
                    LocalResourceInfo(source_target=target, setup=["/foo", "--resource"], resource_env_vars=wrong_env_vars)
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Value for `resource_env_vars` field is not a dictionary",
            );
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    wrong_env_vars = {}
                    LocalResourceInfo(source_target=target, setup=["/foo", "--resource"], resource_env_vars=wrong_env_vars)
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Value for `resource_env_vars` field is an empty dictionary",
            );
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    wrong_env_vars = {1:"one"}
                    LocalResourceInfo(source_target=target, setup=["/foo", "--resource"], resource_env_vars=wrong_env_vars)
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Invalid key in `resource_env_vars`: Expected a str, got",
            );
        }
        {
            let test = indoc!(
                r#"
                def test():
                    target = label("//:foobar")
                    wrong_env_vars = {"one":1}
                    LocalResourceInfo(source_target=target, setup=["/foo", "--resource"], resource_env_vars=wrong_env_vars)
                "#
            );
            expect_error(
                tester.run_starlark_bzl_test(test),
                test,
                "Invalid value in `resource_env_vars`: Expected a str, got",
            );
        }
        Ok(())
    }

    #[test]
    fn test_validation_at_freeze() -> anyhow::Result<()> {
        let mut tester = new_tester();
        let test = indoc!(
            r#"
            def make_info():
                resource_env_vars = {"RESOURCE_ENV_VAR": "json_key"}
                info = LocalResourceInfo(source_target=target, setup=["/foo", "--resource"], resource_env_vars=resource_env_vars)
                resource_env_vars["ONE"] = 1
                return info

            exported_info = make_info()
            "#
        );
        let res = tester.add_import(&ImportPath::testing_new("root//:defs2.bzl"), test);
        assert!(res.is_err());
        Ok(())
    }
}
