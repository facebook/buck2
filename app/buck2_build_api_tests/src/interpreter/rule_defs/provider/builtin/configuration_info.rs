/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_common::result::SharedResult;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

#[test]
fn configuration_info_validates_buckconfigs() -> SharedResult<()> {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
        def test():
            ConfigurationInfo(
                constraints = {},
                values = {
                    "applekey": "value",
                }
            )
        "#
        ),
        "Could not find section separator (`.`) in pair `applekey`",
    );

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def test():
            ConfigurationInfo(
                constraints = {},
                values = {
                    "apple.key": "value",
                }
            )
        "#
    ))
}
