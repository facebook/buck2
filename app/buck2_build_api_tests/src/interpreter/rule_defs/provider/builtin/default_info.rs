/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::build_defs::register_provider;
use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_common::result::SharedResult;
use buck2_core::bzl::ImportPath;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

#[test]
fn default_info_is_available() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    tester.additional_globals(register_rule_defs);
    tester.additional_globals(register_provider);
    tester.add_import(
        &ImportPath::testing_new("root//foo:defs.bzl"),
        r#"BarInfo = provider(fields=["bar"])"#,
    )?;
    tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//foo:defs.bzl", "BarInfo")

            def test():
                b1 = BarInfo(bar="bar1")
                artifact = source_artifact("foo", "bar.cpp")
                default_defaults = DefaultInfo()
                default1 = DefaultInfo(sub_targets={"foo": [b1]}, default_outputs=[artifact])
                default2 = DefaultInfo(sub_targets={"foo": [default1]}, default_outputs=[])

                assert_eq("DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[])", repr(default_defaults))
                assert_eq([], default_defaults.default_outputs);
                assert_eq({}, default_defaults.sub_targets);

                r = repr(artifact)
                expected_repr = (
                    'DefaultInfo(sub_targets={"foo": Providers([BarInfo(bar="bar1"), ' +
                     repr(default_defaults) + '])}, default_outputs=[' + r + '], other_outputs=[])'
                )
                assert_eq(expected_repr, repr(default1))
                assert_eq([artifact], default1.default_outputs);
                assert_eq([], default1.sub_targets["foo"][DefaultInfo].default_outputs);
                assert_eq("bar1", default1.sub_targets["foo"][BarInfo].bar);

                expected_repr = (
                    'DefaultInfo(sub_targets={"foo": Providers([' + repr(default1) + '])}, ' +
                    'default_outputs=[], other_outputs=[])'
                )
                assert_eq(expected_repr, repr(default2))
                assert_eq([], default2.default_outputs);
                assert_eq([artifact], default2.sub_targets["foo"][DefaultInfo].default_outputs);
                assert_eq("bar1", default2.sub_targets["foo"][DefaultInfo].sub_targets["foo"][BarInfo].bar);
            "#
        ))
}

#[test]
fn default_info_validates_types() -> SharedResult<()> {
    // TODO(nmj): More complex types
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);
    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
            def test():
                default_defaults = DefaultInfo()
                DefaultInfo(sub_targets={"foo": []}, default_outputs=1)
            "#
        ),
        "Type of parameter",
    );

    tester.run_starlark_bzl_test_expecting_error(
        indoc!(
            r#"
            def test():
                default_defaults = DefaultInfo()
                DefaultInfo(sub_targets=[], default_outputs=["foo"])
            "#
        ),
        "Type of parameter",
    );

    tester.run_starlark_bzl_test(indoc!(
        r#"
            def test():
                assert_eq(DefaultInfo.type, "DefaultInfo")
            "#
    ))
}

#[test]
fn test_to_json() {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_builtin_providers);
    tester
        .run_starlark_bzl_test(indoc!(
            r#"
            def test():
                default = DefaultInfo(
                    sub_targets={"foo": [DefaultInfo()]},
                )
                assert_eq(
                    '{"sub_targets":{"foo":{"DefaultInfo":{"sub_targets":{},"default_outputs":[],"other_outputs":[]}}},"default_outputs":[],"other_outputs":[]}',
                    default.to_json(),
                )
            "#
        ))
        .unwrap();
}
