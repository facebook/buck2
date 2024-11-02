/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter_for_build::interpreter::testing::expect_error;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::label::testing::label_creator;
use indoc::indoc;

#[test]
fn labels_are_usable() -> buck2_error::Result<()> {
    fn new_tester() -> buck2_error::Result<Tester> {
        let mut tester = Tester::new()?;
        tester.additional_globals(label_creator);
        Ok(tester)
    }

    let mut tester = new_tester()?;
    tester.run_starlark_bzl_test(indoc!(
            r#"
            frozen_l_default = label("root//foo/bar:baz")
            frozen_l = label("root//foo/bar:baz[something]")
            def test():
                l_default = label("root//foo/bar:baz")
                l = label("root//foo/bar:baz[something]")

                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", repr(frozen_l_default))
                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", str(frozen_l_default))
                assert_eq("foo/bar", frozen_l_default.package)
                assert_eq("baz", frozen_l_default.name)
                assert_eq(None, frozen_l_default.sub_target)
                assert_eq("root", frozen_l_default.cell)

                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", repr(frozen_l))
                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", str(frozen_l))
                assert_eq("foo/bar", frozen_l.package)
                assert_eq("baz", frozen_l.name)
                assert_eq(["something"], frozen_l.sub_target)

                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", repr(l_default))
                assert_eq_ignore_hash("root//foo/bar:baz (<testing>#<HASH>)", str(l_default))
                assert_eq("foo/bar", l_default.package)
                assert_eq("baz", l_default.name)
                assert_eq(None, l_default.sub_target)

                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", repr(l))
                assert_eq_ignore_hash("root//foo/bar:baz[something] (<testing>#<HASH>)", str(l))
                assert_eq("foo/bar", l.package)
                assert_eq("baz", l.name)
                assert_eq(["something"], l.sub_target)
                assert_eq("root", l.cell)

            "#
        ))?;

    let mut tester = new_tester()?;
    let invalid_fields = indoc!(
        r#"
            l = label("root//foo:bar[baz]")
            def hide_type(v): return v
            def test():
                hide_type(l).invalid_field
            "#
    );
    expect_error(
        tester.run_starlark_test(invalid_fields),
        invalid_fields,
        "Object of type `label` has no attribute `invalid_field`",
    );
    Ok(())
}
