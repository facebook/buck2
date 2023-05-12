/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::functions::regex::register_regex;
use buck2_interpreter_for_build::interpreter::testing::Tester;

#[test]
fn test_regex() -> anyhow::Result<()> {
    let mut t = Tester::new()?;
    t.additional_globals(register_regex);
    t.run_starlark_test(
        r#"
def test():
    assert_eq(regex_match("abc|def|ghi", "abc"), True)
    assert_eq(regex_match("abc|def|ghi", "xyz"), False)
    assert_eq(regex_match("^((?!abc).)*$", "abc"), False)
    assert_eq(regex_match("^((?!abc).)*$", "xyz"), True)
"#,
    )?;
    Ok(())
}
