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
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

fn tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);
    tester
}

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
    let mut tester = tester();
    tester.run_starlark_bzl_test(test)?;
    Ok(())
}

#[test]
fn test_validation() -> anyhow::Result<()> {
    let mut tester = tester();
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo()
        "#
        ),
        "`type`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = 123)
        "#
        ),
        "`type`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", command = "foo")
        "#
        ),
        "`command`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", command = [123])
        "#
        ),
        "`command`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", env = "foo")
        "#
        ),
        "`env`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", env = {"foo": 123})
        "#
        ),
        "`env`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", labels = "foo")
        "#
        ),
        "`labels`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", labels = [123])
        "#
        ),
        "`labels`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", contacts = "foo")
        "#
        ),
        "`contacts`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", contacts = [123])
        "#
        ),
        "`contacts`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", use_project_relative_paths = "foo")
        "#
        ),
        "`use_project_relative_paths`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", run_from_project_root = "foo")
        "#
        ),
        "`run_from_project_root`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", default_executor = "foo")
        "#
        ),
        "`default_executor`",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ExternalRunnerTestInfo(type = "foo", executor_overrides = {"foo": "bar" })
        "#
        ),
        "`executor_overrides`",
    );

    Ok(())
}

#[test]
fn test_validation_at_freeze() -> anyhow::Result<()> {
    let mut tester = tester();

    let res = tester.add_import(
        &ImportPath::testing_new("root//test:def1.bzl"),
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
