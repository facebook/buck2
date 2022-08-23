/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use indoc::indoc;

use crate::interpreter::rule_defs::artifact::testing::artifactory;
use crate::interpreter::rule_defs::cmd_args::tester::command_line_stringifier;
use crate::interpreter::rule_defs::transitive_set::testing::tset_factory;
use crate::interpreter::testing::expect_error;
use crate::interpreter::testing::import;
use crate::interpreter::testing::run_simple_starlark_test;
use crate::interpreter::testing::Tester;

#[test]
fn test_define_transitive_set() -> anyhow::Result<()> {
    run_simple_starlark_test(indoc!(
        r#"
        FooSet = transitive_set()
        FooSet2 = FooSet

        def test():
            assert_eq("FooSet", str(FooSet))
            assert_eq("Value(TransitiveSetDefinition(FooSet declared in root//some/package/defs.bzl))", debug(FooSet))
            assert_eq("FooSet", str(FooSet2))
            assert_eq("unnamed transitive set", repr(transitive_set()))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_define_transitive_set_projections() -> anyhow::Result<()> {
    run_simple_starlark_test(indoc!(
        r#"
        def project1(_value):
            pass

        FooSet = transitive_set(args_projections = { "foo": project1 })

        def test():
            pass
        "#
    ))?;

    Ok(())
}

#[test]
fn test_create_transitive_set() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    tester.run_starlark_bzl_test(indoc!(
        r#"
        FooSet = transitive_set()
        BarSet = transitive_set()

        def test():
            f1 = make_tset(FooSet, value = 1)
            f2 = make_tset(FooSet, value = 2)
            f3 = make_tset(FooSet, value = 3, children = [f1, f2])
        "#
    ))?;

    let contents = indoc!(
        r#"
        FooSet = transitive_set()
        BarSet = transitive_set()

        def test():
            f1 = make_tset(FooSet, value = 1)
            make_tset(BarSet, value = None, children = [f1])
        "#
    );

    expect_error(
        tester.run_starlark_bzl_test(contents),
        contents,
        "expected: `TransitiveSetDefinition(BarSet declared in root//some/package/defs.bzl)`, \
         got: `TransitiveSetDefinition(FooSet declared in root//some/package/defs.bzl)`",
    );

    Ok(())
}

#[test]
fn test_frozen_transitive_sets() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    tester.add_import(
        &import("root", "test", "def1.bzl"),
        "FooSet = transitive_set()",
    )?;

    tester.add_import(
        &import("root", "test", "use1.bzl"),
        indoc!(
            r#"
            load("//test:def1.bzl", "FooSet")

            f1 = make_tset(FooSet, value = 1)
            "#
        ),
    )?;

    tester.run_starlark_bzl_test(indoc!(
        r#"
        load("//test:def1.bzl", "FooSet")
        load("//test:use1.bzl", "f1")

        def test():
            f2 = make_tset(FooSet, value = 2)
            f3 = make_tset(FooSet, value = 3, children = [f1, f2])
        "#
    ))?;

    Ok(())
}

#[test]
fn test_transitive_set_display() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    tester.run_starlark_bzl_test(indoc!(
        r#"
        FooSet = transitive_set()

        def test():
            f1 = make_tset(FooSet, value = 1)
            f2 = make_tset(FooSet, value = 2, children = [f1])
            f3 = make_tset(FooSet, children = [f1])

            assert_eq("FooSet(value=1, 0 children)", repr(f1))
            assert_eq("FooSet(value=2, 1 children)", repr(f2))
            assert_eq("FooSet(1 children)", repr(f3))

            assert_eq("FooSet(\n  value=1,\n  0 children\n)", pprint_str(f1))
            assert_eq("FooSet( 1 children )", pprint_str(f3))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_transitive_sets_validation() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    let contents = indoc!(
        r#"
        def test():
            FooSet = transitive_set()
            make_tset(FooSet, value = None)
        "#
    );

    expect_error(
        tester.run_starlark_bzl_test(contents),
        contents,
        "used before being assigned",
    );

    let contents = indoc!(
        r#"
        def test():
            make_tset(123, value = None)
        "#
    );

    expect_error(
        tester.run_starlark_bzl_test(contents),
        contents,
        "not the output of transitive_set",
    );

    Ok(())
}

#[test]
fn test_transitive_sets_projection() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def project1(value):
            return str(value)

        FooSet = transitive_set(args_projections = {
            "foo": project1
        })

        def test():
            f1 = make_tset(FooSet, value = 1)
            f1.project_as_args("foo")
        "#
    ))?;

    let contents = indoc!(
        r#"
        FooSet = transitive_set()

        def test():
            f1 = make_tset(FooSet, value = 1)
            f1.project_as_args("foo")
        "#
    );

    expect_error(
        tester.run_starlark_bzl_test(contents),
        contents,
        "requested projection `foo` does not exist.",
    );

    Ok(())
}

#[test]
fn test_transitive_sets_iteration() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    tester.run_starlark_bzl_test(indoc!(
        r#"
        FooSet = transitive_set()

        def test():
            f1 = make_tset(FooSet, value = 1)
            f2 = make_tset(FooSet, value = 2, children = [f1])
            f3 = make_tset(FooSet, value = 3, children = [f1, f2])
            f4 = make_tset(FooSet, children = [f3])

            assert_eq([3, 1, 2], list(f4.traverse()))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_frozen_transitive_sets_iteration() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(tset_factory);

    tester.add_import(
        &import("root", "test", "def1.bzl"),
        "FooSet = transitive_set()",
    )?;

    tester.add_import(
        &import("root", "test", "use1.bzl"),
        indoc!(
            r#"
            load("//test:def1.bzl", "FooSet")

            f1 = make_tset(FooSet, value = 1)
            "#
        ),
    )?;

    tester.run_starlark_bzl_test(indoc!(
        r#"
        load("//test:def1.bzl", "FooSet")
        load("//test:use1.bzl", "f1")

        def test():
            f2 = make_tset(FooSet, value = 2, children = [f1])

            assert_eq([1], list(f1.traverse()))
            assert_eq([2, 1], list(f2.traverse()))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_projection_args() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(|builder| {
        tset_factory(builder);
        command_line_stringifier(builder);
    });

    tester.add_import(
        &import("root", "test", "decl.bzl"),
        indoc!(
            r#"
            "#
        ),
    )?;

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def project(val):
            if val == 1:
                return "foo"
            if val == 2:
                return cmd_args("bar")
            if val == 3:
                return ["baz"]

        FooSet = transitive_set(args_projections = {
            "project": project
        })

        def test():
            f3 = make_tset(FooSet, value = 3)
            f2 = make_tset(FooSet, value = 2)
            f1 = make_tset(FooSet, value = 1, children = [f2, f3])
            proj = f1.project_as_args("project")

            assert_eq(["foo", "bar", "baz"], get_args(proj))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_projection_inputs() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(|builder| {
        tset_factory(builder);
        artifactory(builder);
        command_line_stringifier(builder);
    });

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def project(value):
            return value

        FooSet = transitive_set(args_projections = {
            "project": project
        })

        def test():
            a1 = source_artifact("a1", "a1.h")
            a2 = source_artifact("a2", "a2.h")
            a3 = source_artifact("a3", "a3.h")

            f2 = make_tset(FooSet, value = [a2, a3])
            f1 = make_tset(FooSet, value = [a1], children = [f2])

            proj2 = f2.project_as_args("project")
            proj1 = f1.project_as_args("project")

            # Always just 1 input, no matter the struture.
            assert_eq(1, len(cmd_args().add(proj2).inputs))
            assert_eq(1, len(cmd_args().add(proj1).inputs))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_projection_iteration() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(|builder| {
        tset_factory(builder);
        command_line_stringifier(builder);
    });

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def project(value):
            return value

        FooSet = transitive_set(args_projections = {
            "project": project
        })

        def test():
            f2 = make_tset(FooSet, value = "bar")
            f1 = make_tset(FooSet, value = "foo", children = [f2])
            proj = f1.project_as_args("project")

            l = list(proj.traverse())

            assert_eq(2, len(l))
            assert_eq(["foo"], get_args(l[0]))
            assert_eq(["bar"], get_args(l[1]))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_reduction() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(|builder| {
        tset_factory(builder);
    });

    tester.run_starlark_bzl_test(indoc!(
        r#"
        def depth(children, _value):
            return max(children + [0]) + 1

        def truthyness(children, value):
            c = all(children)
            if value == None:
                return c
            return c and value

        FooSet = transitive_set(reductions = {
            "depth": depth,
            "truthyness": truthyness,
        })

        def test():
            f3 = make_tset(FooSet, value = False)
            f2 = make_tset(FooSet, value = True)
            f1 = make_tset(FooSet, value = True, children = [f2])
            f0 = make_tset(FooSet, children = [f1, f3])

            assert_eq(1, f3.reduce("depth"))
            assert_eq(1, f2.reduce("depth"))
            assert_eq(2, f1.reduce("depth"))
            assert_eq(3, f0.reduce("depth"))

            assert_eq(False, f3.reduce("truthyness"))
            assert_eq(True, f2.reduce("truthyness"))
            assert_eq(True, f1.reduce("truthyness"))
            assert_eq(False, f0.reduce("truthyness"))
        "#
    ))?;

    Ok(())
}

#[test]
fn test_definition_type() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(|builder| {
        tset_factory(builder);
    });

    tester.add_import(
        &import("root", "test", "def.bzl"),
        "FooSet = transitive_set()",
    )?;

    tester.run_starlark_bzl_test(indoc!(
        r#"
        load("//test:def.bzl", "FooSet")

        BarSet = transitive_set()

        def test():
            QuxSet = transitive_set();
            assert_eq("FooSet", FooSet.type)
            assert_eq("BarSet", BarSet.type)
            assert_eq("transitive_set_definition", QuxSet.type)
        "#
    ))?;

    Ok(())
}

#[test]
fn test_type() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(|builder| {
        tset_factory(builder);
    });

    tester.run_starlark_bzl_test(indoc!(
        r#"
        FooSet = transitive_set()

        def wants_foo_set(f: FooSet.type):
            pass

        def wants_transitive_set(f: "transitive_set"):
            pass

        def test():
            s = make_tset(FooSet)
            wants_foo_set(s)
            wants_transitive_set(s)
        "#
    ))?;

    let contents = indoc!(
        r#"
        FooSet = transitive_set()

        def wants_not_foo_set(f: "some"):
            pass

        def test():
            s = make_tset(FooSet)
            wants_not_foo_set(s)
        "#
    );

    expect_error(
        tester.run_starlark_bzl_test(contents),
        contents,
        "does not match the type annotation",
    );

    Ok(())
}
