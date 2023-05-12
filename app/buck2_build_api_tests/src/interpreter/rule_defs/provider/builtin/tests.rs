/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg(test)]

use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::label::testing::label_creator;
use indoc::indoc;

/// Test `equals` in generated code for providers.
#[test]
fn test_equals() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;

    tester.additional_globals(register_builtin_providers);
    tester.additional_globals(label_creator);

    tester.run_starlark_bzl_test(indoc!(
        r#"
            def test():
                x = target_label("root//bar:baz")
                y = target_label("root//quux:corge")
                a0 = ConstraintSettingInfo(label = x)
                a1 = ConstraintSettingInfo(label = x)
                b = ConstraintSettingInfo(label = y)
                assert_eq(a0, a1)
                assert_ne(a0, b)
        "#
    ))?;

    Ok(())
}
