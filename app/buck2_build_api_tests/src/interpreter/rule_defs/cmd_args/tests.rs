/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_build_api::interpreter::rule_defs::cmd_args::SimpleCommandLineArtifactVisitor;
use buck2_build_api::interpreter::rule_defs::cmd_args::StarlarkCommandLineInputs;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_core::bzl::ImportPath;
use buck2_interpreter::types::regex::register_buck_regex;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_interpreter_for_build::interpreter::testing::expect_error;
use buck2_interpreter_for_build::label::testing::label_creator;
use indoc::indoc;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::list_or_tuple::UnpackListOrTuple;

use crate::interpreter::rule_defs::artifact::testing::artifactory;
use crate::interpreter::rule_defs::cmd_args::testing;

#[starlark_module]
pub(crate) fn inputs_helper(builder: &mut GlobalsBuilder) {
    fn make_inputs<'v>(
        values: UnpackListOrTuple<Value<'v>>,
    ) -> starlark::Result<StarlarkCommandLineInputs> {
        let mut visitor = SimpleCommandLineArtifactVisitor::new();
        for v in values {
            let cli = ValueAsCommandLineLike::unpack_value_err(v)?.0;
            cli.visit_artifacts(&mut visitor)?;
        }

        Ok(StarlarkCommandLineInputs {
            inputs: visitor.inputs,
        })
    }
}

fn tester() -> buck2_error::Result<Tester> {
    let mut tester = Tester::new()?;
    tester.additional_globals(testing::command_line_stringifier);
    tester.additional_globals(inputs_helper);
    tester.additional_globals(artifactory);
    tester.additional_globals(label_creator);
    tester.additional_globals(register_rule_defs);
    tester.additional_globals(register_buck_regex);
    Ok(tester)
}

#[test]
fn stringifies_correctly() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        artifact1 = source_artifact("foo", "bar/baz.h")
        artifact2 = bound_artifact("//:dep1", "dir/baz.h")
        arg2 = "string1"
        arg3 = artifact1
        arg4 = artifact2
        arg5 = label("//foo:bar[baz]")

        def test():
            artifact3 = source_artifact("foo", "bar/quz.h")
            artifact4 = bound_artifact("//:dep2", "dir/quz.h")
            arg7 = "string2"
            arg8 = artifact3
            arg9 = artifact4
            arg10 = label("//foo:bar[quz]")

            assert_eq("string1", stringify_cli_arg(arg2))
            assert_eq("foo/bar/baz.h", stringify_cli_arg(arg3))
            assert_eq_ignore_hash("buck-out/v2/art/root/<HASH>/__dep1__/dir/baz.h", stringify_cli_arg(arg4))
            assert_eq_ignore_hash("root//foo:bar[baz] (<testing>#<HASH>)", stringify_cli_arg(arg5))
            assert_eq("string2", stringify_cli_arg(arg7))
            assert_eq("foo/bar/quz.h", stringify_cli_arg(arg8))
            assert_eq_ignore_hash("buck-out/v2/art/root/<HASH>/__dep2__/dir/quz.h", stringify_cli_arg(arg9))
            assert_eq_ignore_hash("root//foo:bar[quz] (<testing>#<HASH>)", stringify_cli_arg(arg10))
        "#
    ))?;

    let contents = indoc!(
        r#"
        def test():
            arg = stringify_cli_arg(["list of strings aren't valid"])
        "#
    );

    expect_error(
        tester.run_starlark_bzl_test(contents),
        contents,
        "Expected `Artifact | CellPath | CellRoot",
    );

    Ok(())
}

#[test]
fn displays_correctly() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        def test():
            cli = cmd_args(format="x{}y", quote="shell", hidden="bar")
            cli.add("foo")
            # TODO(nga): fix options formatting.
            assert_eq('cmd_args("foo", hidden=["bar"], format="x{}y", quote="shell")', str(cli))
            assert_eq('cmd_args(\n  "foo",\n  hidden=[ "bar" ],\n  format="x{}y",\n  quote="shell"\n)', prepr(cli))
        "#
    ))?;

    Ok(())
}

#[test]
fn displays_correctly_replace_regex() {
    let mut tester = tester().unwrap();
    tester
        .run_starlark_bzl_test(indoc!(
            r#"
        def test():
            cli = cmd_args(replace_regex=(regex("foo"), "bar"))
            assert_eq('cmd_args(replacements=[("foo", "bar")])', str(cli))
        "#
        ))
        .unwrap();
}

#[test]
fn command_line_builder() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let content = indoc!(
        r#"
        b1 = cmd_args()
        b2 = cmd_args()
        b3 = cmd_args()
        b4 = cmd_args()
        b5 = cmd_args()

        b2.add("b2 s1").add(["b2 s2"])
        b3.add("b3 s1").add(["b3 s2"])

        a1 = source_artifact("foo", "bar1/baz.h")
        a2 = bound_artifact("//:dep1", "dir/baz.h")
        a3 = source_artifact("foo", "bar2/baz.h")
        a4 = bound_artifact("//:dep2", "dir/baz.h")
        l1 = str(label("//foo:bar[baz]"))

        b1.add("b1 s1", "b1 s2").add(b2).add(a1).add(a2)
        b1.add([b3, l1]).add([a3, a4])

        # Add something so we can verify it doesn't modify b1
        b2 = b2.copy()
        b2.add("not in b1")

        # Ensure that both lists, and *args formats work
        b4.add(cmd_args(["--foo=", a1, ",bar=", b3, ",baz"], delimiter = ""))
        b4.add(cmd_args("--foo=", a1, ",bar=", b3, ",baz", delimiter = ""))

        # Make sure delimiters work properly
        b5.add(cmd_args("foo", delimiter = ""))
        b5.add(cmd_args("foo", "bar", delimiter=","))
        b5.add(cmd_args("foo", "bar", "baz", delimiter=","))

        mutable_b1_args = get_args(b1)
        mutable_b2_args = get_args(b2)
        mutable_b3_args = get_args(b3)
        mutable_b4_args = get_args(b4)
        mutable_b5_args = get_args(b5)

        def test():
            b1_args_expected = [
                "b1 s1",
                "b1 s2",
                "b2 s1",
                "b2 s2",
                "foo/bar1/baz.h",
                "buck-out/v2/art/root/<HASH>/__dep1__/dir/baz.h",
                "b3 s1",
                "b3 s2",
                "root//foo:bar[baz] (<testing>#<HASH>)",
                "foo/bar2/baz.h",
                "buck-out/v2/art/root/<HASH>/__dep2__/dir/baz.h",
            ]
            b2_args_expected = [
                "b2 s1",
                "b2 s2",
                "not in b1",
            ]
            b3_args_expected = [
                "b3 s1",
                "b3 s2",
            ]
            b4_args_expected = [
                "--foo=foo/bar1/baz.h,bar=b3 s1b3 s2,baz",
                "--foo=foo/bar1/baz.h,bar=b3 s1b3 s2,baz",
            ]
            b5_args_expected = [
                "foo",
                "foo,bar",
                "foo,bar,baz",
            ]

            # Check values while builders were mutable
            assert_eq_ignore_hash(b1_args_expected, mutable_b1_args)

            assert_eq_ignore_hash(b2_args_expected, mutable_b2_args)

            assert_eq_ignore_hash(b3_args_expected, mutable_b3_args)

            assert_eq_ignore_hash(b4_args_expected, mutable_b4_args)

            assert_eq_ignore_hash(b5_args_expected, mutable_b5_args)

            # Check that frozen values still work
            frozen_b1_args = get_args(b1)
            frozen_b2_args = get_args(b2)
            frozen_b3_args = get_args(b3)
            frozen_b4_args = get_args(b4)
            frozen_b5_args = get_args(b5)

            assert_eq_ignore_hash(b1_args_expected, frozen_b1_args)

            assert_eq_ignore_hash(b2_args_expected, frozen_b2_args)

            assert_eq_ignore_hash(b3_args_expected, frozen_b3_args)

            assert_eq_ignore_hash(b4_args_expected, frozen_b4_args)

            assert_eq_ignore_hash(b5_args_expected, frozen_b5_args)

            # Make sure we can add frozen CLIs to unfrozen ones
            b6 = cmd_args()
            b6.add("b6").add(b2).add([b3])

            b6_args_expected = ["b6"] + b2_args_expected + b3_args_expected
            assert_eq(b6_args_expected, get_args(b6))
        "#
    );

    tester.run_starlark_bzl_test(content)?;

    let content_invalid_type_1 = r#"cmd_args().add({"not": "an arg"})"#;
    let content_invalid_type_3 = r#"cmd_args().add([{"not": "an arg"}])"#;

    expect_error(
        tester.run_starlark_bzl_test(content_invalid_type_1),
        content_invalid_type_1,
        "Expected `Artifact | CellPath | CellRoot",
    );
    expect_error(
        tester.run_starlark_bzl_test(content_invalid_type_3),
        content_invalid_type_3,
        "Expected `Artifact | CellPath | CellRoot",
    );

    Ok(())
}

#[test]
fn test_relative_absolute_old() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(absolute_prefix="$ABSOLUTE/", absolute_suffix="!")
            args.add(source_artifact("foo","bar/baz/qux.h"))
            args.relative_to(source_artifact("foo", "bar/foo"))

            assert_eq(get_args(args), ["$ABSOLUTE/../baz/qux.h!"])

            args = cmd_args()
            args.add(source_artifact("foo","bar/baz/qux.h"))
            args.relative_to(source_artifact("foo", "bar/baz"), parent=1)
            assert_eq(get_args(args), ["baz/qux.h"])
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_relative_absolute() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(absolute_prefix="$ABSOLUTE/", absolute_suffix="!", relative_to=source_artifact("foo", "bar/foo"))
            args.add(source_artifact("foo","bar/baz/qux.h"))

            assert_eq(get_args(args), ["$ABSOLUTE/../baz/qux.h!"])

            args = cmd_args(relative_to=(source_artifact("foo", "bar/baz"), 1))
            args.add(source_artifact("foo","bar/baz/qux.h"))
            assert_eq(get_args(args), ["baz/qux.h"])
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_relative_to_propagated_up_and_down() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(source_artifact("foo", "bar.h"))
            # Self check
            assert_eq(get_args(args), ["foo/bar.h"])

            # `relative_to` is propagated down to `args`
            args2 = cmd_args(args, relative_to=(source_artifact("foo", "baz.c"), 1))
            assert_eq(get_args(args2), ["./bar.h"])

            # `relative_to` is propagated up to `args3`
            args3 = cmd_args(args2)
            assert_eq(get_args(args3), ["./bar.h"])
        "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_relative_to_does_not_affect_new_artifacts() -> buck2_error::Result<()> {
    let mut tester = tester().unwrap();
    let content = indoc!(
        r#"
        def test():
            args = cmd_args(
                source_artifact("foo", "bar.h"),
                relative_to=(source_artifact("foo", "baz.c"), 1),
            )
            # Self check
            assert_eq(get_args(args), ["./bar.h"])

            args2 = cmd_args(args, source_artifact("foo", "bar2.h"))
            assert_eq(get_args(args2), ["./bar.h", "foo/bar2.h"])
        "#
    );
    tester.run_starlark_bzl_test(content)?;
    Ok(())
}

#[test]
fn test_parent() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(absolute_suffix="!", parent=1)
            args.add(source_artifact("foo","bar/baz/qux.h"))
            assert_eq(get_args(args), ["foo/bar/baz!"])
        "#
    );
    tester.run_starlark_bzl_test(contents)?;

    let too_many_parent_calls = indoc!(
        r#"
        def test():
            args = cmd_args(parent=3)
            args.add(source_artifact("foo","qux.h"))
            get_args(args)
        "#
    );
    expect_error(
        tester.run_starlark_bzl_test(too_many_parent_calls),
        too_many_parent_calls,
        "too many .parent() calls",
    );

    Ok(())
}

#[test]
fn test_parent_n() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(
                source_artifact("foo","bar/baz/qux.h"),
                absolute_suffix="!",
                parent=2,
            )
            assert_eq(get_args(args), ["foo/bar!"])
        "#
    );
    tester.run_starlark_bzl_test(contents)?;

    Ok(())
}

#[test]
fn test_parent_n_too_many_parents() -> buck2_error::Result<()> {
    let mut tester = tester()?;

    let too_many_parent_calls = indoc!(
        r#"
        def test():
            args = cmd_args(
                source_artifact("foo","qux.h"),
                parent=3,
            )
            get_args(args)
        "#
    );
    expect_error(
        tester.run_starlark_bzl_test(too_many_parent_calls),
        too_many_parent_calls,
        "too many .parent() calls",
    );

    Ok(())
}

#[test]
fn test_parent_n_parent_type() -> buck2_error::Result<()> {
    let mut tester = tester()?;

    let bad_count = indoc!(
        r#"
        def test():
            args = cmd_args(
                source_artifact("foo","qux.h"),
                parent=-12,
            )
            get_args(args)
        "#
    );
    expect_error(
        tester.run_starlark_bzl_test(bad_count),
        bad_count,
        "Integer value is too big to fit in u32",
    );

    Ok(())
}

#[test]
fn test_format() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args1 = cmd_args(delimiter="", format="format-{}-{}-string")
            args2 = cmd_args(format="format-{}-{}-string")
            args3 = cmd_args()

            args1.add("foo")
            args1.add(",bar")
            args2.add("foo")
            args2.add("bar")
            args3.add(cmd_args(["foo", "bar"], format="format-{}-{}-string", delimiter = ""))
            args3.add(cmd_args(["foo", "bar"], delimiter=",", format="format-{}-{}-string"))
            args3.add(cmd_args("foo", format="format-{}-{}-string1"))
            args3.add(cmd_args("bar", format="format-{}-{}-string2"))

            assert_eq(["format-foo-foo-stringformat-,bar-,bar-string"], get_args(args1))
            assert_eq(
                [
                    "format-foo-foo-string",
                    "format-bar-bar-string",
                ],
                get_args(args2),
            )
            assert_eq(
                [
                    "format-foo-foo-stringformat-bar-bar-string",
                    "format-foo-foo-string,format-bar-bar-string",
                    "format-foo-foo-string1",
                    "format-bar-bar-string2",
                ],
                get_args(args3),
            )
        "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_joined_with_empty_args() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(cmd_args(["", "foo"], delimiter=",", format="format-{}-string"))
            args.add(cmd_args(["", "", "foo"], delimiter=","))
            assert_eq(
                [
                    "format--string,format-foo-string",
                    ",,foo",
                ],
                get_args(args),
            )
        "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_inputs_outputs() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        artifact1 = source_artifact("foo", "bar/baz.h")
        artifact2 = bound_artifact("//:dep1", "dir/baz.h")

        def norm(xs):
            # Try and make everything consistent for the equality test
            return sorted([repr(x) for x in xs])

        def test():
            artifact3 = source_artifact("foo", "bar/quz.h")
            artifact4 = bound_artifact("//:dep2", "dir/quz.h")
            artifact5 = declared_artifact("declared")

            cli = cmd_args(hidden=artifact1)
            cli.add(artifact3)
            cli.add("just a string")
            cli.add(artifact4)
            cli.add(artifact5.as_output())

            assert_eq(make_inputs([artifact3, artifact4, artifact1]), cli.inputs)
            assert_eq(3, len(cli.inputs))
            assert_eq(norm([artifact5.as_output()]), norm(cli.outputs))
        "#
    ))?;
    Ok(())
}

#[test]
fn test_ignore_artifacts() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        def norm(xs):
            # Try and make everything consistent for the equality test
            return sorted([repr(x) for x in xs])

        def test():
            artifact = bound_artifact("//:dep2", "dir/quz.h")

            cli = cmd_args(ignore_artifacts=True)
            cli.add(artifact)

            assert_eq(make_inputs([]), cli.inputs)
            assert_eq([], cli.outputs)

            assert_eq_ignore_hash(["buck-out/v2/art/root/<HASH>/__dep2__/dir/quz.h"], get_args(cli))
        "#
    ))?;
    Ok(())
}

#[test]
fn test_frozen_inputs_outputs() -> buck2_error::Result<()> {
    let mut tester = tester()?;

    tester.add_import(
        &ImportPath::testing_new("root//test:def1.bzl"),
        indoc!(
            r#"
            def norm(xs):
                # Try and make everything consistent for the equality test
                return sorted([repr(x) for x in xs])

            input = source_artifact("foo", "bar/quz.h")
            output = declared_bound_artifact("//foo:bar", "declared")

            cli = cmd_args()
            cli.add("string")
            cli.add(input)
            cli.add(output.as_output())

            expected_outputs = norm([output.as_output()])
            expected_inputs = make_inputs([input])
            "#
        ),
    )?;

    tester.run_starlark_bzl_test(indoc!(
        r#"
        load("//test:def1.bzl", "cli", "expected_inputs", "expected_outputs", "norm")

        def test():
            assert_eq(expected_inputs, cli.inputs)
            assert_eq(expected_outputs, norm(cli.outputs))
        "#
    ))?;
    Ok(())
}

#[test]
fn test_quote_style_shell() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(cmd_args("$HELLO", quote = "shell"))
            assert_eq(get_args(args), ["\"\\$HELLO\""])

            args = cmd_args()
            args.add(cmd_args(source_artifact("foo", "bar$qux.h"), quote = "shell"))
            assert_eq(get_args(args), ["\"foo/bar\\$qux.h\""])
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_prepend() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add([["foo", "bar"], "baz"])
            assert_eq(["foo", "bar", "baz"], get_args(args))
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_list_list() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(cmd_args(["foo", "bar"], prepend = "-X"))
            assert_eq(["-X", "foo", "-X", "bar"], get_args(args))
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_concat() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args("foo", "bar", delimiter = "-")
            assert_eq(["foo-bar"], get_args(args))
            args = cmd_args("foo", "bar", delimiter = "-", format = "({})")
            assert_eq(["(foo)-(bar)"], get_args(args))
            args = cmd_args("foo", "bar", delimiter = "-", prepend = "@", format = "({})")
            assert_eq(["@(foo)-@(bar)"], get_args(args))
            args = cmd_args("foo", "bar", delimiter = "-", prepend = "@", format = "({})", quote="shell")
            assert_eq(['@"(foo)"-@"(bar)"'], get_args(args))

            # Nail down the order of application
            # Note that prepend and delimiter together are not separable
            args = cmd_args(cmd_args(cmd_args("foo", "bar", format="({})"), quote="shell"), prepend="@", delimiter="-")
            assert_eq(['@"(foo)"-@"(bar)"'], get_args(args))
            args = cmd_args(cmd_args(cmd_args(cmd_args("foo", "bar", format="({})"), quote="shell"), prepend="@"), delimiter="-")
            assert_eq(['@-"(foo)"-@-"(bar)"'], get_args(args))
            args = cmd_args(cmd_args(cmd_args(cmd_args("foo", "bar", format="({})"), quote="shell"), delimiter="-"), prepend="@")
            assert_eq(['@', '"(foo)"-"(bar)"'], get_args(args))
     "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_replace_regex() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(
                "$OUT",
                "$OUTPUT",
                "$SRCS",
                format="$OUT: {}",
                replace_regex=[("\\$OUT\\b", "%OUT%"), ("\\$SRCS\\b", "%SRCS%")],
            )
            assert_eq(["$OUT: %OUT%", "$OUT: $OUTPUT", "$OUT: %SRCS%"], get_args(args))

            args = cmd_args(
                "\\n\n",
                replace_regex=[("\\\\n", "\\\n"), ("\\n", "\\n")],
            )
            assert_eq(["\\\\n\\n"], get_args(args))
        "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_replace_regex_old() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args(
                "$OUT",
                "$OUTPUT",
                "$SRCS",
                format="$OUT: {}",
                replace_regex=[("\\$OUT\\b", "%OUT%"), ("\\$SRCS\\b", "%SRCS%")],
            )
            assert_eq(["$OUT: %OUT%", "$OUT: $OUTPUT", "$OUT: %SRCS%"], get_args(args))

            args = cmd_args(
                "\\n\n",
                replace_regex=[("\\\\n", "\\\n"), ("\\n", "\\n")],
            )
            assert_eq(["\\\\n\\n"], get_args(args))
        "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_replace_regex_regex() -> buck2_error::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args("$OUT", "$OUTPUT", "$SRCS", format="$OUT: {}",
                replace_regex=[(regex("\\$OUT\\b"), "%OUT%"), (regex("\\$SRCS\\b"), "%SRCS%")])
            assert_eq(["$OUT: %OUT%", "$OUT: $OUTPUT", "$OUT: %SRCS%"], get_args(args))

            args = cmd_args("\\n\n",
                replace_regex = [(regex("\\\\n"), "\\\n"), ("\\n", "\\n")])
            assert_eq(["\\\\n\\n"], get_args(args))
        "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}
