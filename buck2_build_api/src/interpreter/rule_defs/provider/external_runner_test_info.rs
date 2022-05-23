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
        dict::Dict, list::List, none::NoneType, tuple::Tuple, Freeze, Trace, Value, ValueLike,
    },
};

use crate::{
    attrs::attr_type::arg::value::ResolvedStringWithMacros,
    interpreter::rule_defs::cmd_args::{
        CommandLineArgLike, CommandLineArtifactVisitor, CommandLineBuilder, ValueAsCommandLineLike,
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
    ) -> anyhow::Result<ExternalRunnerTestInfo<'v>> {
        let res = ExternalRunnerTestInfo {
            test_type: r#type,
            command,
            env,
            labels,
            contacts,
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
