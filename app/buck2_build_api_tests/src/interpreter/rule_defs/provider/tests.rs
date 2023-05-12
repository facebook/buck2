/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::build_defs::register_provider;
use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_core::bzl::ImportPath;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

fn provider_tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);
    tester.additional_globals(register_provider);
    tester
}

#[test]
fn creates_providers() -> anyhow::Result<()> {
    // TODO(nmj): Starlark doesn't let you call 'new_invoker()' on is_mutable types.
    //                 Once that's fixed, make sure we can call 'FooInfo' before the module is
    //                 frozen.
    let mut tester = provider_tester();

    tester.run_starlark_test(indoc!(
        r#"
    FooInfo = provider(fields=["bar", "baz"])
    FooInfo2 = FooInfo
    #frozen_foo_1 = FooInfo(bar="bar_f1", baz="baz_f1")
    #frozen_foo_2 = FooInfo(bar="bar_f2")

    assert_eq("unnamed provider", repr(provider(fields=["f1"])))
    assert_eq("FooInfo(bar, baz)", repr(FooInfo))
    assert_eq("FooInfo(bar, baz)", repr(FooInfo2))

    def test():
        assert_eq(FooInfo.type, "FooInfo")
        assert_eq("FooInfo(bar, baz)", repr(FooInfo))
        assert_eq("FooInfo(bar, baz)", repr(FooInfo2))

        #assert_eq("FooInfo(bar=\"bar_f1\", baz=\"baz_f1\")", repr(frozen_foo1))
        #assert_eq("bar_f1", frozen_foo1.bar)
        #assert_eq("baz_f1", frozen_foo1.baz)
        #assert_eq("FooInfo(bar=\"bar_f2\", baz=None)", repr(frozen_foo2))
        #assert_eq("bar_f2", frozen_foo2.bar)
        #assert_eq(None, frozen_foo2.baz)

        foo_1 = FooInfo(bar="bar_1", baz="baz_1")
        foo_2 = FooInfo(bar="bar_2")

        assert_eq("FooInfo(bar, baz)", repr(FooInfo))
        assert_eq("FooInfo(bar=\"bar_1\", baz=\"baz_1\")", repr(foo_1))
        assert_eq("bar_1", foo_1.bar)
        assert_eq("baz_1", foo_1.baz)
        assert_eq("FooInfo(bar=\"bar_2\", baz=None)", repr(foo_2))
        assert_eq("bar_2", foo_2.bar)
        assert_eq(None, foo_2.baz)

        assert_eq("{\"bar\":\"bar_1\",\"baz\":\"baz_1\"}", foo_1.to_json())
    "#
    ))?;

    tester.run_starlark_test_expecting_error(
        indoc!(
            r#"
    FooInfo = provider(fields=["bar", "baz"])

    def test():
        foo_1 = FooInfo(bar="bar1")
        foo_1.quz
    "#
        ),
        "Object of type `provider` has no attribute `quz`",
    );

    tester.run_starlark_test_expecting_error(
        indoc!(
            r#"
    list = []
    list.append(provider(fields=["bar", "baz"]))
    "#
        ),
        "must be assigned to a variable",
    );

    // Make sure that frozen UserProvider instances work
    let mut tester = provider_tester();
    tester.add_import(
        &ImportPath::testing_new("root//provider:def1.bzl"),
        indoc!(
            r#"
            FooInfo = provider(fields=["foo"])
            "#
        ),
    )?;
    tester.add_import(
        &ImportPath::testing_new("root//provider:def2.bzl"),
        indoc!(
            r#"
            load("//provider:def1.bzl", "FooInfo")
            foo = FooInfo(foo="foo1")
            "#
        ),
    )?;
    tester.run_starlark_test(indoc!(
        r#"
        load("//provider:def2.bzl", "foo")
        def test():
            assert_eq('FooInfo(foo="foo1")', repr(foo))
        "#
    ))?;

    Ok(())
}
