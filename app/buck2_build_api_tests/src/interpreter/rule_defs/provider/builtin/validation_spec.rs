/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::validation_spec;
use buck2_interpreter_for_build::interpreter::testing::expect_error;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

fn new_tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(validation_spec::register_validation_spec);
    tester.additional_globals(artifactory);
    tester
}

#[test]
fn test_construction() -> anyhow::Result<()> {
    let mut tester = new_tester();
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationSpec(name="foo", validation_result=a)
            "#
        );
        tester.run_starlark_bzl_test(test)?;
    }
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationSpec(name="foo", validation_result=a, optional=True)
            "#
        );
        tester.run_starlark_bzl_test(test)?;
    }
    Ok(())
}

#[test]
fn test_missing_fields_validation() -> anyhow::Result<()> {
    let mut tester = new_tester();
    {
        let test = indoc!(
            r#"
            def test():
                ValidationSpec(name="foo")
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Missing required parameter `validation_result`",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationSpec(validation_result=a)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Missing required parameter `name`",
        );
    }
    Ok(())
}

#[test]
fn test_validation_failure() -> anyhow::Result<()> {
    let mut tester = new_tester();
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationSpec(name=1, validation_result=a)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Expected type `str` but got `int`",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationSpec(name="", validation_result=a)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Name of validation spec should not be empty",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                ValidationSpec(name="foo", validation_result="bar")
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Expected type `artifact` but got `str`",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                bar = declared_artifact("baz/quz.h")
                ValidationSpec(name="foo", validation_result=bar)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Validation result artifact should be bound",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                bar = source_artifact("foo/bar", "baz/quz.h")
                ValidationSpec(name="foo", validation_result=bar)
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Validation result artifact should be a build artifact",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationSpec(name="test", validation_result=a, optional="invalid")
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Expected type `bool` but got `str`",
        );
    }
    Ok(())
}

#[test]
fn test_attributes() -> anyhow::Result<()> {
    let mut tester = new_tester();
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                s = ValidationSpec(name="foo", validation_result=a)
                assert_eq(s.name, "foo")
                assert_eq_ignore_hash("<build artifact baz/quz.h bound to root//foo:bar (<testing>#<HASH>)>", repr(s.validation_result))
                assert_eq(s.optional, False)
            "#
        );
        tester.run_starlark_bzl_test(test)?;
    }
    Ok(())
}
