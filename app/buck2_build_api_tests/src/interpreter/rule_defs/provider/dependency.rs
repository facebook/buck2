/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

#[test]
fn dependency_type_check_get_ok() -> buck2_error::Result<()> {
    let mut tester = Tester::new()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
            def check(dep: Dependency):
                prov = dep.get(DefaultInfo)
                if prov:
                    x = prov.default_outputs[0]
            def test(): pass
            "#
    ))?;
    Ok(())
}

#[test]
#[ignore = "starlark can't check None yet"]
fn dependency_type_check_get_fail_none() -> buck2_error::Result<()> {
    let mut tester = Tester::new()?;
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
            def check(dep: Dependency):
                prov = dep.get(DefaultInfo)
                x = prov.default_outputs[0]
            def test(): pass
            "#
        ),
        "The attribute `default_outputs` is not available on the type `None | DefaultInfo`",
    );
    Ok(())
}

#[test]
fn dependency_type_check_get_fail_field() -> buck2_error::Result<()> {
    let mut tester = Tester::new()?;
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
            def check(dep: Dependency):
                prov = dep.get(DefaultInfo)
                if prov:
                    x = prov.xxxx
            def test(): pass
            "#
        ),
        // TODO: this error message could be better, we ruled out None
        "The attribute `xxxx` is not available on the type `None | DefaultInfo`",
    );
    Ok(())
}

#[test]
fn dependency_type_check_index_ok() -> buck2_error::Result<()> {
    let mut tester = Tester::new()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
            def check(dep: Dependency):
                prov = dep[DefaultInfo]
                x = prov.default_outputs[0]

            def test(): pass
            "#
    ))?;
    Ok(())
}

#[test]
fn dependency_type_check_index_fail() -> buck2_error::Result<()> {
    let mut tester = Tester::new()?;
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
            def check(dep: Dependency):
                prov = dep[DefaultInfo]
                x = prov.xxxx
            def test(): pass
            "#
        ),
        "The attribute `xxxx` is not available on the type `DefaultInfo`",
    );
    Ok(())
}

#[test]
fn dependency_type_check_binop_in() -> buck2_error::Result<()> {
    let mut tester = Tester::new()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
            def check(dep: Dependency):
                return DefaultInfo in dep
            def test(): pass
            "#
    ))?;
    Ok(())
}
