/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter_for_build::interpreter::functions::host_info::register_host_info;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

#[test]
fn test_host_info() -> anyhow::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_host_info);
    tester.run_starlark_test(indoc!(
        r#"
            def test():
                assert_eq(True, host_info().os.is_linux)
                assert_eq(False, host_info().os.is_macos)
                assert_eq(False, host_info().os.is_macos)

                assert_eq(True, host_info().arch.is_x86_64)
                assert_eq(False, host_info().arch.is_arm)
                assert_eq(False, host_info().arch.is_mipsel64)

            "#
    ))?;
    Ok(())
}

#[test]
fn test_buck_v2() -> anyhow::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_host_info);
    tester.run_starlark_test(indoc!(
        r#"
            def test():
                assert_eq(True, hasattr(host_info(), "buck2"))
                assert_eq(False, hasattr(host_info(), "buck1"))
        "#
    ))?;
    Ok(())
}
