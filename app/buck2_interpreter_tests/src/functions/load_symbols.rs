/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::bzl::ImportPath;
use buck2_interpreter::functions::load_symbols::register_load_symbols;
use buck2_interpreter_for_build::interpreter::testing::Tester;

#[test]
fn test_load_symbols() -> anyhow::Result<()> {
    let mut t = Tester::new()?;
    t.additional_globals(register_load_symbols);
    let defines = ImportPath::testing_new("root//pkg:test.bzl");
    t.add_import(
        &defines,
        r#"
y = 2
load_symbols({'x': 1, 'z': 3})
"#,
    )?;
    t.run_starlark_test(
        r#"
load("@root//pkg:test.bzl", "x", "y", "z")
def test():
    assert_eq(x + y + z, 6)"#,
    )?;
    Ok(())
}
