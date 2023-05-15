/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_core::bzl::ImportPath;
use buck2_interpreter_for_build::interpreter::testing::expect_error;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::label::testing::label_creator;
use indoc::indoc;

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
