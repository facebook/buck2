/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter_for_build::interpreter::functions::read_config::register_read_config;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

#[test]
fn test_read_config() -> anyhow::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_read_config);
    tester.run_starlark_test(indoc!(
        r#"
            def test():
                assert_eq("default", read_config("missing_section", "key", "default"))
                assert_eq("default", read_config("section", "missing_key", "default"))
                assert_eq(1, read_config("section", "missing_key", 1))
                assert_eq(None, read_config("section", "missing_key", None))

                assert_eq("value", read_config("section", "key", "default"))
                assert_eq("value", read_config("section", "key"))

                assert_eq("1", read_config("section", "other"))
                assert_eq("hello world!", read_config("section", "multiline"))
                assert_eq("okay", read_config("config", "key"))
            "#
    ))?;
    Ok(())
}
