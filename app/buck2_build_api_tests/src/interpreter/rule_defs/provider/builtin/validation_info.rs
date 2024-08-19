/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_build_api::interpreter::rule_defs::validation_spec;
use buck2_interpreter_for_build::interpreter::testing::expect_error;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

fn new_tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);
    tester.additional_globals(validation_spec::register_validation_spec);
    tester.additional_globals(artifactory);
    tester
}

#[test]
fn test_construction() -> anyhow::Result<()> {
    let mut tester = new_tester();
    let test = indoc!(
        r#"
        def test():
            a = declared_bound_artifact("//foo:bar", "baz/quz.h")
            ValidationInfo(validations=[ValidationSpec(name="foo", validation_result=a)])
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
                ValidationInfo()
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Missing required parameter `validations`",
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
                ValidationInfo(validations=[1, 2])
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Expected type `list[ValidationSpec]` but got `list[int]`",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                a = declared_bound_artifact("//foo:bar", "baz/quz.h")
                ValidationInfo(validations=[ValidationSpec(name="foo", validation_result=a), ValidationSpec(name="foo", validation_result=a)])
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "Multiple specs with same name `foo` which is not allowed.",
        );
    }
    {
        let test = indoc!(
            r#"
            def test():
                ValidationInfo(validations=[])
            "#
        );
        expect_error(
            tester.run_starlark_bzl_test(test),
            test,
            "`ValidationInfo` should contain at least one validation.",
        );
    }
    Ok(())
}
