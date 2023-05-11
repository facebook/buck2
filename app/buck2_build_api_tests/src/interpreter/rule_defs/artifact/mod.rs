/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::result::SharedResult;
use buck2_interpreter_for_build::interpreter::testing::expect_error;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use indoc::indoc;

use crate::interpreter::rule_defs::artifact::testing::artifactory;

pub(crate) mod testing;

#[test]
fn source_artifact() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
        r#"
            a1 = source_artifact("foo/bar", "baz/quz.h")
            a2 = source_artifact("foo/bar", "baz/file1")

            def test():
                a3 = source_artifact("foo/bar", "baz/quz.cpp")
                a4 = source_artifact("foo/bar", "baz/file2")

                assert_eq("<source foo/bar/baz/quz.h>", repr(a1))
                assert_eq("quz.h", a1.basename)
                assert_eq("baz/quz.h", a1.short_path)
                assert_eq(".h", a1.extension)
                assert_eq(True, a1.is_source)
                assert_eq(None, a1.owner)

                assert_eq("<source foo/bar/baz/file1>", repr(a2))
                assert_eq("file1", a2.basename)
                assert_eq("baz/file1", a2.short_path)
                assert_eq("", a2.extension)
                assert_eq(True, a2.is_source)
                assert_eq(None, a2.owner)

                assert_eq("<source foo/bar/baz/quz.cpp>", repr(a3))
                assert_eq("quz.cpp", a3.basename)
                assert_eq("baz/quz.cpp", a3.short_path)
                assert_eq(".cpp", a3.extension)
                assert_eq(True, a3.is_source)
                assert_eq(None, a3.owner)

                assert_eq("<source foo/bar/baz/file2>", repr(a4))
                assert_eq("file2", a4.basename)
                assert_eq("baz/file2", a4.short_path)
                assert_eq("", a4.extension)
                assert_eq(True, a4.is_source)
                assert_eq(None, a4.owner)

                # Validate that attrs are setup properly
                for a in (a1, a2, a3, a4):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
    ))?;

    let as_output = indoc!(
        r#"
            def test():
                source_artifact("foo/bar", "baz/quz.cpp").as_output()
            "#
    );
    expect_error(
        tester.run_starlark_bzl_test(as_output),
        as_output,
        "Source artifacts may not be outputs",
    );
    Ok(())
}

#[test]
fn bound_artifact() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(buck2_build_api::interpreter::rule_defs::register_rule_defs);
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = bound_artifact("//foo:bar", "baz/quz.h")
            a2 = bound_artifact("//foo:bar", "baz/file1")

            def test():
                a3 = bound_artifact("//foo:bar", "baz/quz.cpp")
                a4 = bound_artifact("//foo:bar", "baz/file2")

                assert_eq_ignore_hash("<build artifact baz/quz.h bound to root//foo:bar (<testing>#<HASH>)>", repr(a1))
                assert_eq("quz.h", a1.basename)
                assert_eq("baz/quz.h", a1.short_path)
                assert_eq(".h", a1.extension)
                assert_eq(False, a1.is_source)
                assert_eq("bar", a1.owner.name)

                assert_eq_ignore_hash("<build artifact baz/file1 bound to root//foo:bar (<testing>#<HASH>)>", repr(a2))
                assert_eq("file1", a2.basename)
                assert_eq("baz/file1", a2.short_path)
                assert_eq("", a2.extension)
                assert_eq(False, a2.is_source)
                assert_eq("bar", a2.owner.name)

                assert_eq_ignore_hash("<build artifact baz/quz.cpp bound to root//foo:bar (<testing>#<HASH>)>", repr(a3))
                assert_eq("quz.cpp", a3.basename)
                assert_eq("baz/quz.cpp", a3.short_path)
                assert_eq(".cpp", a3.extension)
                assert_eq(False, a3.is_source)
                assert_eq("bar", a3.owner.name)

                assert_eq_ignore_hash("<build artifact baz/file2 bound to root//foo:bar (<testing>#<HASH>)>", repr(a4))
                assert_eq("file2", a4.basename)
                assert_eq("baz/file2", a4.short_path)
                assert_eq("", a4.extension)
                assert_eq(False, a4.is_source)
                assert_eq("bar", a4.owner.name)

                # Validate that attrs are setup properly
                for a in (a1, a2, a3, a4):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
        ))?;

    let as_output = indoc!(
        r#"
            def test():
                bound_artifact("//foo:bar", "baz/quz.cpp").as_output()
            "#
    );
    expect_error(
        tester.run_starlark_bzl_test(as_output),
        as_output,
        "already used",
    );
    Ok(())
}

#[test]
fn declared_artifact() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
        r#"
            def test():
                a1 = declared_artifact("baz/quz.cpp")
                a2 = declared_artifact("baz/file2")

                assert_eq("<build artifact baz/quz.cpp>", repr(a1))
                assert_eq("quz.cpp", a1.basename)
                assert_eq(".cpp", a1.extension)
                assert_eq(False, a1.is_source)
                assert_eq(None, a1.owner)
                assert_eq("<output artifact for baz/quz.cpp>", repr(a1.as_output()))

                assert_eq("<build artifact baz/file2>", repr(a2))
                assert_eq("file2", a2.basename)
                assert_eq("", a2.extension)
                assert_eq(False, a2.is_source)
                assert_eq(None, a2.owner)
                assert_eq("<output artifact for baz/file2>", repr(a2.as_output()))

                # Validate that attrs are setup properly
                for a in (a1, a2):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
    ))?;
    Ok(())
}

#[test]
fn declared_bound() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(buck2_build_api::interpreter::rule_defs::register_rule_defs);
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = declared_bound_artifact("//foo:bar", "baz/quz.h")
            a2 = declared_bound_artifact("//foo:bar", "baz/file1")

            def test():
                a3 = declared_bound_artifact("//foo:bar", "baz/quz.cpp")
                a4 = declared_bound_artifact("//foo:bar", "baz/file2")

                assert_eq_ignore_hash("<build artifact baz/quz.h bound to root//foo:bar (<testing>#<HASH>)>", repr(a1))
                assert_eq("quz.h", a1.basename)
                assert_eq("baz/quz.h", a1.short_path)
                assert_eq(".h", a1.extension)
                assert_eq(False, a1.is_source)
                assert_eq("bar", a1.owner.name)

                assert_eq_ignore_hash("<build artifact baz/file1 bound to root//foo:bar (<testing>#<HASH>)>", repr(a2))
                assert_eq("file1", a2.basename)
                assert_eq("baz/file1", a2.short_path)
                assert_eq("", a2.extension)
                assert_eq(False, a2.is_source)
                assert_eq("bar", a2.owner.name)

                assert_eq_ignore_hash("<build artifact baz/quz.cpp bound to root//foo:bar (<testing>#<HASH>)>", repr(a3))
                assert_eq("quz.cpp", a3.basename)
                assert_eq("baz/quz.cpp", a3.short_path)
                assert_eq(".cpp", a3.extension)
                assert_eq(False, a3.is_source)
                assert_eq("bar", a3.owner.name)

                assert_eq_ignore_hash("<build artifact baz/file2 bound to root//foo:bar (<testing>#<HASH>)>", repr(a4))
                assert_eq("file2", a4.basename)
                assert_eq("baz/file2", a4.short_path)
                assert_eq("", a4.extension)
                assert_eq(False, a4.is_source)
                assert_eq("bar", a4.owner.name)

                # Validate that attrs are setup properly
                for a in (a1, a2, a3, a4):
                    for prop in dir(a):
                        assert_eq(True, hasattr(a, prop))
                        if prop != "as_output":
                            getattr(a, prop)
            "#
        ))?;

    Ok(())
}

#[test]
fn project_declared_artifact() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
            r#"
            def test():
                bound = declared_bound_artifact("//foo:bar", "out").project("baz.o")
                assert_eq_ignore_hash("<build artifact out/baz.o bound to root//foo:bar (<testing>#<HASH>)>", repr(bound))
                assert_eq("baz.o", bound.basename)
                assert_eq(".o", bound.extension)

                unbound = declared_artifact("out").project("qux.so")
                assert_eq("<build artifact out/qux.so>", repr(unbound))
                assert_eq("<output artifact for out/qux.so>", repr(unbound.as_output()))
                assert_eq("qux.so", unbound.basename)
                assert_eq(".so", unbound.extension)
            "#
        ))?;
    Ok(())
}

#[test]
fn test_short_path() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
            r#"
            def test():
                test = declared_artifact("foo/bar/baz")
                assert_eq("foo/bar/baz", test.short_path)

                test = declared_artifact("foo").project("bar/baz")
                assert_eq("foo/bar/baz", test.short_path)

                test = declared_artifact("foo").project("bar").project("baz")
                assert_eq("foo/bar/baz", test.short_path)

                test = declared_artifact("foo").project("bar/baz", hide_prefix=True)
                assert_eq("bar/baz", test.short_path)

                test = declared_artifact("foo").project("bar").project("baz", hide_prefix=True)
                assert_eq("baz", test.short_path)

                test = declared_artifact("foo").project("bar", hide_prefix=True).project("baz")
                assert_eq("bar/baz", test.short_path)

                test = declared_artifact("foo").project("bar", hide_prefix=True).project("baz", hide_prefix=True)
                assert_eq("baz", test.short_path)
            "#
        ))?;
    Ok(())
}

#[test]
fn project_source_artifact() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    let test = indoc!(
        r#"
            def test():
                source_artifact("foo/bar", "baz").project("foo")
            "#
    );
    expect_error(
        tester.run_starlark_bzl_test(test),
        test,
        "Source artifacts cannot be projected",
    );
    Ok(())
}

#[test]
fn project_artifact() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    let test = indoc!(
        r#"
            def test():
                bound_artifact("//foo:bar", "baz").project("foo")
            "#
    );
    expect_error(
        tester.run_starlark_bzl_test(test),
        test,
        "This artifact was declared by another rule",
    );
    Ok(())
}

#[test]
fn stringifies_for_command_line() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
            r#"
            a1 = bound_artifact("//foo:bar", "baz/quz.h")
            a2 = source_artifact("foo/bar", "baz/file1")

            def test():
                a3 = bound_artifact("//foo:bar", "baz/quz.cpp")
                a4 = source_artifact("foo/bar", "baz/file2")

                assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/foo/__bar__/baz/quz.h", stringify_for_cli(a1))
                assert_eq("foo/bar/baz/file1", stringify_for_cli(a2))
                assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/foo/__bar__/baz/quz.cpp", stringify_for_cli(a3))
                assert_eq("foo/bar/baz/file2", stringify_for_cli(a4))
            "#
        ))
}

#[test]
fn bound_artifact_with_associated_artifacts() -> SharedResult<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(buck2_build_api::interpreter::rule_defs::register_rule_defs);
    tester.additional_globals(artifactory);
    tester.run_starlark_bzl_test(indoc!(
        r#"
            def test():
                # declare an artifact (a2) with string and add an associated artifact (a1)
                a1 = source_artifact("foo/bar", "baz/file1")
                a2 = declared_bound_artifact_with_associated_artifacts("baz/quz.h", [a1])
                assert_eq(a2.short_path, "baz/quz.h")
                assert_eq(get_associated_artifacts_as_string(a1), "")
                assert_eq(get_associated_artifacts_as_string(a2), "root//foo/bar/baz/file1")

                # use a predeclared artifact (a3) and add an associated artifact (a4)
                a3 = declared_artifact("wom/bat.h")
                a4 = source_artifact("foo/bar", "baz/file2")
                a5 = declared_bound_artifact_with_associated_artifacts(a3, [a4])
                assert_eq(a3.short_path, "wom/bat.h")
                assert_eq(a5.short_path, "wom/bat.h")
                assert_eq(get_associated_artifacts_as_string(a3), "")
                assert_eq(get_associated_artifacts_as_string(a5), "root//foo/bar/baz/file2")

                # use a predeclared artifact (a3) with no associated artifacts
                a6 = declared_bound_artifact_with_associated_artifacts(a3, [])
                assert_eq(a6.short_path, "wom/bat.h")
                assert_eq(get_associated_artifacts_as_string(a6), "")

                a7 = a5.without_associated_artifacts()
                assert_eq(a5.short_path, a7.short_path)
                assert_eq(get_associated_artifacts_as_string(a7), "")
            "#
    ))
}
