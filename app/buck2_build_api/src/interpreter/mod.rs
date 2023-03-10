/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod build_defs;
pub mod context;
pub mod rule_defs;

#[cfg(test)]
mod tests {
    use buck2_common::result::SharedResult;
    use buck2_core::bzl::ImportPath;
    use buck2_interpreter_for_build::interpreter::testing::Tester;
    use indoc::indoc;
    use starlark::environment::GlobalsBuilder;

    use crate::interpreter::rule_defs::register_rule_defs;

    #[test]
    fn cannot_register_target_twice() {
        let content = indoc!(
            r#"
            def _impl(ctx):
                pass
            export_file = rule(impl=_impl, attrs = {})
            def test():
                export_file(name="foo")
                export_file(name="foo")
        "#
        );
        let mut tester = Tester::new().unwrap();
        tester.additional_globals(register_rule_defs);
        let err = tester.run_starlark_test(content).expect_err("should fail");
        assert!(
            err.to_string()
                .contains("Attempted to register target root//some/package:foo twice"),
            "got `{}`",
            err
        );
    }

    // Dummy module just to make sure that our integration test framework is working...
    #[starlark_module]
    fn extra_provider_module(builder: &mut GlobalsBuilder) {
        fn add_one(i: i32) -> anyhow::Result<i32> {
            Ok(i + 1)
        }
    }

    #[test]
    fn tester_can_load_extra_modules() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.additional_globals(extra_provider_module);

        tester.run_starlark_test(indoc!(
            r#"
            x = add_one(1)
            def test():
                y = 2
                assert_eq(2, x)
                assert_eq(3, add_one(y))
            "#
        ))?;

        tester.run_starlark_bzl_test(indoc!(
            r#"
            x = add_one(1)
            def test():
                y = 2
                assert_eq(2, x)
                assert_eq(3, add_one(y))
            "#
        ))
    }

    #[test]
    fn tester_can_load_symbols_transitively() -> SharedResult<()> {
        fn new_tester() -> SharedResult<Tester> {
            let mut tester = Tester::new()?;
            tester.add_import(
                &ImportPath::testing_new("root//test:def1.bzl"),
                indoc!(
                    r#"
                l = [1,2,3]
                "#
                ),
            )?;
            tester.add_import(
                &ImportPath::testing_new("root//test:def2.bzl"),
                indoc!(
                    r#"
                load("//test:def1.bzl", "l")
                l2 = l + [4,5,6]
                "#
                ),
            )?;
            Ok(tester)
        }

        let mut tester = new_tester()?;
        tester.run_starlark_test(indoc!(
            r#"
            load("//test:def2.bzl", "l2")
            def test():
                assert_eq([1,2,3,4,5,6], l2)
            "#
        ))?;

        let mut tester = new_tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//test:def2.bzl", "l2")
            def test():
                assert_eq([1,2,3,4,5,6], l2)
            "#
        ))
    }
}
