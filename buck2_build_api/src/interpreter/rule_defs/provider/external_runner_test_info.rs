/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter::{empty, once};

use anyhow::Context as _;
use buck2_build_api_derive::internal_provider;
use either::Either;
use gazebo::{any::AnyLifetime, coerce::Coerce};
use starlark::{
    environment::GlobalsBuilder,
    values::{
        dict::Dict,
        list::List,
        none::{NoneOr, NoneType},
        tuple::Tuple,
        Freeze, Trace, UnpackValue, Value, ValueLike,
    },
};

use crate::{
    attrs::attr_type::arg::value::ResolvedStringWithMacros,
    interpreter::rule_defs::{
        cmd_args::{
            CommandLineArgLike, CommandLineArtifactVisitor, CommandLineBuilder,
            ValueAsCommandLineLike,
        },
        command_executor_config::StarlarkCommandExecutorConfig,
    },
};

/// Provider that signals that a rule can be tested using an external runner. This is the
/// Buck1-compatible API for tests.
#[internal_provider(external_runner_test_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, AnyLifetime)]
#[freeze(validator = validate_external_runner_test_info, bounds = "V: ValueLike<'freeze>")]
#[repr(C)]
pub struct ExternalRunnerTestInfoGen<V> {
    /// A Starlark value representing the type of this test.
    /// This is of type str.type
    test_type: V,

    /// A Starlark value representing the command for this test. The external test runner is what
    /// gives meaning to this command.
    /// This is of type [[str.type, "_arglike"]]
    command: V,

    /// A Starlark value representing the environment for this test. Here again, the external test
    /// runner is what will this meaning.
    /// This is of type {str.type: _arglike}
    env: V,

    /// A starlark value representing the labels for this test.
    /// This is of type [str.type]
    labels: V,

    /// A starlark value representing the contacts for this test. This is largely expected to be an
    /// oncall, though it's not validated in any way.
    /// This is of type [str.type]
    contacts: V,

    /// Whether this test should use relative paths. The default is not to.
    /// This is of type [bool.type]
    use_project_relative_paths: V,

    /// Whether this test should run from the project root, as opposed to the cell root. The
    /// default is not to.
    /// This is of type [bool.type]
    run_from_project_root: V,

    /// Executors that Tpx can use to override the default executor.
    /// This is of type {str.type: CommandExecutorConfig}
    executor_overrides: V,
}

// NOTE: All the methods here unwrap because we validate at freeze time.
impl FrozenExternalRunnerTestInfo {
    pub fn test_type(&self) -> &str {
        self.test_type.to_value().unpack_str().unwrap()
    }

    pub fn command(&self) -> impl Iterator<Item = TestCommandMember<'_>> {
        unwrap_all(iter_test_command(self.command.to_value()))
    }

    pub fn env(&self) -> impl Iterator<Item = (&str, &dyn CommandLineArgLike)> {
        unwrap_all(iter_test_env(self.env.to_value()))
    }

    pub fn labels(&self) -> impl Iterator<Item = &str> {
        unwrap_all(iter_opt_str_list(self.labels.to_value(), "labels"))
    }

    pub fn contacts(&self) -> impl Iterator<Item = &str> {
        unwrap_all(iter_opt_str_list(self.contacts.to_value(), "contacts"))
    }

    pub fn use_project_relative_paths(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.use_project_relative_paths.to_value())
            .unwrap()
            .into_option()
            .unwrap_or_default()
    }

    pub fn run_from_project_root(&self) -> bool {
        NoneOr::<bool>::unpack_value(self.run_from_project_root.to_value())
            .unwrap()
            .into_option()
            .unwrap_or_default()
    }

    /// Access a specific executor override.
    pub(crate) fn executor_override(
        &self,
        key: &str,
    ) -> Option<&StarlarkCommandExecutorConfig<'static>> {
        let executor_overrides = Dict::from_value(self.executor_overrides.to_value()).unwrap();
        executor_overrides
            .get_str(key)
            .map(|v| StarlarkCommandExecutorConfig::from_value(v.to_value()).unwrap())
    }

    pub fn visit_artifacts(
        &self,
        visitor: &mut dyn CommandLineArtifactVisitor,
    ) -> anyhow::Result<()> {
        for member in self.command() {
            match member {
                TestCommandMember::Literal(..) => {}
                TestCommandMember::Arglike(arglike) => {
                    arglike.visit_artifacts(visitor)?;
                }
            }
        }

        for (_, arglike) in self.env() {
            arglike.visit_artifacts(visitor)?;
        }

        Ok(())
    }
}

pub enum TestCommandMember<'v> {
    Literal(&'v str),
    Arglike(&'v dyn CommandLineArgLike),
}

impl<'v> TestCommandMember<'v> {
    pub fn add_to_command_line(&self, cli: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        match self {
            Self::Literal(literal) => literal.add_to_command_line(cli),
            Self::Arglike(arglike) => arglike.add_to_command_line(cli),
        }
    }
}

fn iter_value<'v>(value: Value<'v>) -> anyhow::Result<impl Iterator<Item = Value<'v>> + 'v> {
    match (List::from_value(value), Tuple::from_value(value)) {
        (Some(list), None) => Ok(Either::Left(list.iter())),
        (None, Some(tuple)) => Ok(Either::Right(tuple.iter())),
        _ => Err(anyhow::anyhow!(
            "Expected a list or a tuple, got `{}`",
            value
        )),
    }
}

fn iter_test_command<'v>(
    command: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<TestCommandMember<'v>>> {
    if command.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let iterable = match iter_value(command) {
        Ok(v) => v,
        Err(e) => {
            return Either::Left(Either::Right(once(Err(e.context("Invalid `command`")))));
        }
    };

    Either::Right(iterable.map(|item| {
        if let Some(s) = item.unpack_str() {
            return Ok(TestCommandMember::Literal(s));
        }

        if let Some(s) = item.downcast_ref::<ResolvedStringWithMacros>() {
            if let Some(s) = s.downcast_str() {
                return Ok(TestCommandMember::Literal(s));
            }
        }

        let arglike = item
            .as_command_line_err()
            .context("Invalid item in `command`")?;

        Ok(TestCommandMember::Arglike(arglike))
    }))
}

fn iter_test_env<'v>(
    env: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<(&'v str, &'v dyn CommandLineArgLike)>> {
    if env.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let env = match Dict::from_value(env) {
        Some(env) => env,
        None => {
            return Either::Left(Either::Right(once(Err(anyhow::anyhow!(
                "Invalid `env`: Expected a dict, got: `{}`",
                env
            )))));
        }
    };

    // TODO: In an ideal world this wouldnt be necessary, but env's lifetime is bound by this
    // function.
    #[allow(clippy::needless_collect)]
    let env = env.iter().collect::<Vec<_>>();

    Either::Right(env.into_iter().map(|(key, value)| {
        let key = key
            .unpack_str()
            .with_context(|| format!("Invalid key in `env`: Expected a str, got: `{}`", key))?;

        let arglike = value
            .as_command_line_err()
            .with_context(|| format!("Invalid value in `env` for key `{}`", key))?;

        Ok((key, arglike))
    }))
}

fn iter_opt_str_list<'v>(
    list: Value<'v>,
    name: &'static str,
) -> impl Iterator<Item = anyhow::Result<&'v str>> {
    if list.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let iterable = match iter_value(list) {
        Ok(v) => v,
        Err(e) => {
            return Either::Left(Either::Right(once(Err(
                e.context(format!("Invalid `{}`", name))
            ))));
        }
    };

    Either::Right(iterable.map(move |item| {
        let item = item
            .unpack_str()
            .with_context(|| format!("Invalid item in `{}`: {}", name, item))?;

        Ok(item)
    }))
}

fn iter_executor_overrides<'v>(
    executor_overrides: Value<'v>,
) -> impl Iterator<Item = anyhow::Result<(&'v str, &'v StarlarkCommandExecutorConfig<'v>)>> {
    if executor_overrides.is_none() {
        return Either::Left(Either::Left(empty()));
    }

    let executor_overrides = match Dict::from_value(executor_overrides) {
        Some(executor_overrides) => executor_overrides,
        None => {
            return Either::Left(Either::Right(once(Err(anyhow::anyhow!(
                "Invalid `executor_overrides`: Expected a dict, got: `{}`",
                executor_overrides
            )))));
        }
    };

    // TODO: In an ideal world this wouldnt be necessary, but executor_overrides's lifetime is
    // bound by this function.
    #[allow(clippy::needless_collect)]
    let executor_overrides = executor_overrides.iter().collect::<Vec<_>>();

    Either::Right(executor_overrides.into_iter().map(|(key, value)| {
        let key = key.unpack_str().with_context(|| {
            format!(
                "Invalid key in `executor_overrides`: Expected a str, got: `{}`",
                key
            )
        })?;

        let config = StarlarkCommandExecutorConfig::from_value(value)
            .with_context(|| format!("Invalid value in `executor_overrides` for key `{}`", key))?;

        Ok((key, config))
    }))
}

fn check_all<I, T>(it: I) -> anyhow::Result<()>
where
    I: Iterator<Item = anyhow::Result<T>>,
{
    for e in it {
        e?;
    }
    Ok(())
}

fn unwrap_all<I, T>(it: I) -> impl Iterator<Item = T>
where
    I: Iterator<Item = anyhow::Result<T>>,
{
    it.map(|e| e.unwrap())
}

fn validate_external_runner_test_info<'v, V>(
    info: &ExternalRunnerTestInfoGen<V>,
) -> anyhow::Result<()>
where
    V: ValueLike<'v>,
{
    check_all(iter_test_command(info.command.to_value()))?;
    check_all(iter_test_env(info.env.to_value()))?;
    check_all(iter_opt_str_list(info.labels.to_value(), "labels"))?;
    check_all(iter_opt_str_list(info.contacts.to_value(), "contacts"))?;
    check_all(iter_executor_overrides(info.executor_overrides.to_value()))?;
    NoneOr::<bool>::unpack_value(info.use_project_relative_paths.to_value())
        .context("`use_project_relative_paths` must be a bool if provided")?;
    NoneOr::<bool>::unpack_value(info.run_from_project_root.to_value())
        .context("`run_from_project_root` must be a bool if provided")?;
    info.test_type
        .to_value()
        .unpack_str()
        .context("`type` must be a str")?;
    Ok(())
}

#[starlark_module]
fn external_runner_test_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(type = "ExternalRunnerTestInfo")]
    fn ExternalRunnerTestInfo<'v>(
        r#type: Value<'v>,
        #[starlark(default = NoneType)] command: Value<'v>,
        #[starlark(default = NoneType)] env: Value<'v>,
        #[starlark(default = NoneType)] labels: Value<'v>,
        #[starlark(default = NoneType)] contacts: Value<'v>,
        #[starlark(default = NoneType)] use_project_relative_paths: Value<'v>,
        #[starlark(default = NoneType)] run_from_project_root: Value<'v>,
        #[starlark(default = NoneType)] executor_overrides: Value<'v>,
    ) -> anyhow::Result<ExternalRunnerTestInfo<'v>> {
        let res = ExternalRunnerTestInfo {
            test_type: r#type,
            command,
            env,
            labels,
            contacts,
            use_project_relative_paths,
            run_from_project_root,
            executor_overrides,
        };
        validate_external_runner_test_info(&res)?;
        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::interpreter::testing::{
        import, run_starlark_bzl_test, run_starlark_bzl_test_expecting_error, Tester,
    };

    #[test]
    fn test_construction() -> anyhow::Result<()> {
        let test = indoc!(
            r#"
            def test():
                ExternalRunnerTestInfo(type = "foo")
                ExternalRunnerTestInfo(type = "foo", command = ["cmd"])
                ExternalRunnerTestInfo(type = "foo", command = ["cmd", cmd_args()])
                ExternalRunnerTestInfo(type = "foo", command = ("cmd",))
                ExternalRunnerTestInfo(type = "foo", env = {"foo": "bar" })
                ExternalRunnerTestInfo(type = "foo", env = {"foo": cmd_args() })
                ExternalRunnerTestInfo(type = "foo", labels = ["foo"])
                ExternalRunnerTestInfo(type = "foo", contacts = ["foo"])
                ExternalRunnerTestInfo(type = "foo", labels = ("foo",))
                ExternalRunnerTestInfo(type = "foo", use_project_relative_paths = True)
                ExternalRunnerTestInfo(type = "foo", run_from_project_root = True)
            "#
        );
        run_starlark_bzl_test(test)?;
        Ok(())
    }

    #[test]
    fn test_validation() -> anyhow::Result<()> {
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo()
            "#
            ),
            "`type`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = 123)
            "#
            ),
            "`type`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", command = "foo")
            "#
            ),
            "`command`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", command = [123])
            "#
            ),
            "`command`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", env = "foo")
            "#
            ),
            "`env`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", env = {"foo": 123})
            "#
            ),
            "`env`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", labels = "foo")
            "#
            ),
            "`labels`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", labels = [123])
            "#
            ),
            "`labels`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", contacts = "foo")
            "#
            ),
            "`contacts`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", contacts = [123])
            "#
            ),
            "`contacts`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", use_project_relative_paths = "foo")
            "#
            ),
            "`use_project_relative_paths`",
        );

        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ExternalRunnerTestInfo(type = "foo", run_from_project_root = "foo")
            "#
            ),
            "`run_from_project_root`",
        );

        Ok(())
    }

    #[test]
    fn test_validation_at_freeze() -> anyhow::Result<()> {
        let mut tester = Tester::new()?;

        let res = tester.add_import(
            &import("root", "test", "def1.bzl"),
            indoc!(
                r#"
            def make_info():
                contacts = []
                info = ExternalRunnerTestInfo(type = "foo", contacts = contacts)
                contacts.append(123)
                return info

            exported_info = make_info()
            "#
            ),
        );

        assert!(res.is_err());

        Ok(())
    }
}
