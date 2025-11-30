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

#[test]
fn test_attr_display() -> buck2_error::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.run_starlark_bzl_test(r#"
assert_eq(repr(attrs.bool(default = True)), "attrs.bool(default=True)")
assert_eq(repr(attrs.string()), "attrs.string()")
assert_eq(repr(attrs.list(attrs.string())), "attrs.list(attrs.string())")
assert_eq(repr(attrs.dict(attrs.string(), attrs.string())), "attrs.dict(attrs.string(), attrs.string(), sorted=False)")
assert_eq(repr(attrs.one_of(attrs.string())), "attrs.one_of(attrs.string())")
assert_eq(repr(attrs.tuple(attrs.string())), "attrs.tuple(attrs.string())")
assert_eq(repr(attrs.option(attrs.string())), "attrs.option(attrs.string())")

def test(): pass
"#)?;
    Ok(())
}
