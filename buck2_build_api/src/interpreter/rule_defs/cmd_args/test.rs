use std::sync::Arc;

use buck2_core::result::SharedResult;
use indoc::indoc;

use super::tester;
use crate::interpreter::{
    rule_defs::{artifact::testing::artifactory, label::testing::label_creator},
    testing::{expect_error, import, Tester},
};

fn tester() -> anyhow::Result<Tester> {
    let mut tester = Tester::new()?;
    tester.set_additional_globals(Arc::new(|builder| {
        tester::command_line_stringifier(builder);
        tester::inputs_helper(builder);
        artifactory(builder);
        label_creator(builder);
    }));
    Ok(tester)
}

#[test]
fn stringifies_correctly() -> SharedResult<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        artifact1 = source_artifact("foo", "bar/baz.h")
        artifact2 = bound_artifact("//:dep1", "dir/baz.h")
        arg2 = "string1"
        arg3 = artifact1
        arg4 = artifact2
        arg5 = str(label("//foo:bar[baz]"))

        def test():
            artifact3 = source_artifact("foo", "bar/quz.h")
            artifact4 = bound_artifact("//:dep2", "dir/quz.h")
            arg7 = "string2"
            arg8 = artifact3
            arg9 = artifact4
            arg10 = str(label("//foo:bar[quz]"))

            assert_eq("string1", stringify_cli_arg(arg2))
            assert_eq("foo/bar/baz.h", stringify_cli_arg(arg3))
            assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/__dep1__/dir/baz.h", stringify_cli_arg(arg4))
            assert_eq("root//foo:bar[baz] (<testing>)", stringify_cli_arg(arg5))
            assert_eq("string2", stringify_cli_arg(arg7))
            assert_eq("foo/bar/quz.h", stringify_cli_arg(arg8))
            assert_eq_ignore_hash("buck-out/v2/gen/root/<HASH>/__dep2__/dir/quz.h", stringify_cli_arg(arg9))
            assert_eq("root//foo:bar[quz] (<testing>)", stringify_cli_arg(arg10))
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
        "expected command line item to be a string",
    );

    Ok(())
}

#[test]
fn displays_correctly() -> SharedResult<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        def test():
            cli = cmd_args()
            cli.add("foo")
            cli.hidden("bar")
            assert_eq('cmd_args("foo", hidden = ["bar"])', str(cli))
        "#
    ))?;

    Ok(())
}

#[test]
fn command_line_builder() -> SharedResult<()> {
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
        b4.add_joined(["--foo=", a1, ",bar=", b3, ",baz"])
        b4.add_joined("--foo=", a1, ",bar=", b3, ",baz")

        # Make sure delimiters work properly
        b5.add_joined("foo")
        b5.add_joined("foo", "bar", delimiter=",")
        b5.add_joined("foo", "bar", "baz", delimiter=",")

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
                "buck-out/v2/gen/root/<HASH>/__dep1__/dir/baz.h",
                "b3 s1",
                "b3 s2",
                "root//foo:bar[baz] (<testing>)",
                "foo/bar2/baz.h",
                "buck-out/v2/gen/root/<HASH>/__dep2__/dir/baz.h",
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
    // TODO(ndmitchel): We claim you can add labels to a command line, but you can't
    let content_invalid_type_2 = r#"cmd_args().add(label("//:foo"))"#;
    let content_invalid_type_3 = r#"cmd_args().add([{"not": "an arg"}])"#;

    expect_error(
        tester.run_starlark_bzl_test(content_invalid_type_1),
        content_invalid_type_1,
        "expected command line item",
    );
    expect_error(
        tester.run_starlark_bzl_test(content_invalid_type_2),
        content_invalid_type_2,
        "expected command line item",
    );
    expect_error(
        tester.run_starlark_bzl_test(content_invalid_type_3),
        content_invalid_type_3,
        "expected command line item",
    );

    Ok(())
}

#[test]
fn test_short_name() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            assert_eq("run foo", short_name(["foo", "bar"]))
            assert_eq("run command", short_name([]))
            c1 = cmd_args()
            c1.add("foo", "bar")
            c2 = cmd_args()
            c2.add(c1, "baz")
            assert_eq("run foo", short_name([c1, c2]))
        "#
    );

    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_relative_absolute() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(source_artifact("foo","bar/baz/qux.h"))
            args.relative_to(source_artifact("foo", "bar/foo"))
            args.absolute_prefix("$ABSOLUTE/")
            assert_eq(get_args(args), ["$ABSOLUTE/../baz/qux.h"])

            args.absolute_suffix("!")
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
fn test_parent() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(source_artifact("foo","bar/baz/qux.h"))
            args.parent().absolute_suffix("!")
            assert_eq(get_args(args), ["foo/bar/baz!"])
        "#
    );
    tester.run_starlark_bzl_test(contents)?;

    let too_many_parent_calls = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(source_artifact("foo","qux.h"))
            args.parent().parent().parent()
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
fn test_parent_n() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(source_artifact("foo","bar/baz/qux.h"))
            args.parent(2).absolute_suffix("!")
            assert_eq(get_args(args), ["foo/bar!"])
        "#
    );
    tester.run_starlark_bzl_test(contents)?;

    let too_many_parent_calls = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(source_artifact("foo","qux.h"))
            args.parent(3)
            get_args(args)
        "#
    );
    expect_error(
        tester.run_starlark_bzl_test(too_many_parent_calls),
        too_many_parent_calls,
        "too many .parent() calls",
    );

    let bad_count = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(source_artifact("foo","qux.h"))
            args.parent(-12)
            get_args(args)
        "#
    );
    expect_error(
        tester.run_starlark_bzl_test(bad_count),
        bad_count,
        "Type of parameter `count` doesn't match",
    );

    Ok(())
}

#[test]
fn test_format() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args1 = cmd_args(joined=True, delimiter="", format="format-{}-{}-string")
            args2 = cmd_args(joined=False, format="format-{}-{}-string")
            args3 = cmd_args()

            args1.add("foo")
            args1.add(",bar")
            args2.add("foo")
            args2.add("bar")
            args3.add_joined(["foo", "bar"], format="format-{}-{}-string")
            args3.add_joined(["foo", "bar"], delimiter=",", format="format-{}-{}-string")
            args3.add("foo", format="format-{}-{}-string1")
            args3.add("bar", format="format-{}-{}-string2")

            assert_eq(["format-foo,bar-foo,bar-string"], get_args(args1))
            assert_eq(
                [
                    "format-foo-foo-string",
                    "format-bar-bar-string",
                ],
                get_args(args2),
            )
            assert_eq(
                [
                    "format-foobar-foobar-string",
                    "format-foo,bar-foo,bar-string",
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
fn test_joined_with_empty_args() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add_joined(["", "foo"], delimiter=",", format="format-{}-string")
            args.add_joined(["", "", "foo"], delimiter=",")
            assert_eq(
                [
                    "format-,foo-string",
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
fn test_inputs_outputs() -> anyhow::Result<()> {
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

            cli = cmd_args()
            cli.add(artifact3)
            cli.add("just a string")
            cli.add(artifact4)
            cli.hidden(artifact1)
            cli.add(artifact5.as_output())

            assert_eq(make_inputs([artifact3, artifact4, artifact1]), cli.inputs)
            assert_eq(3, len(cli.inputs))
            assert_eq(norm([artifact5.as_output()]), norm(cli.outputs))
        "#
    ))?;
    Ok(())
}

#[test]
fn test_ignore_artifacts() -> anyhow::Result<()> {
    let mut tester = tester()?;
    tester.run_starlark_bzl_test(indoc!(
        r#"
        def norm(xs):
            # Try and make everything consistent for the equality test
            return sorted([repr(x) for x in xs])

        def test():
            artifact = bound_artifact("//:dep2", "dir/quz.h")

            cli = cmd_args()
            cli.add(artifact)
            cli.ignore_artifacts()

            assert_eq(make_inputs([]), cli.inputs)
            assert_eq([], cli.outputs)

            assert_eq_ignore_hash(["buck-out/v2/gen/root/<HASH>/__dep2__/dir/quz.h"], get_args(cli))
        "#
    ))?;
    Ok(())
}

#[test]
fn test_frozen_inputs_outputs() -> anyhow::Result<()> {
    let mut tester = tester()?;

    tester.add_import(
        &import("root", "test", "def1.bzl"),
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
fn test_quote_style_shell() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add("$HELLO", quote = "shell")
            assert_eq(get_args(args), ["\"\\$HELLO\""])

            args = cmd_args()
            args.add(source_artifact("foo", "bar$qux.h"), quote = "shell")
            assert_eq(get_args(args), ["\"foo/bar\\$qux.h\""])
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_prepend() -> anyhow::Result<()> {
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
fn test_list_list() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args()
            args.add(["foo", "bar"], prepend = "-X")
            assert_eq(["-X", "foo", "-X", "bar"], get_args(args))
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}

#[test]
fn test_concat() -> anyhow::Result<()> {
    let mut tester = tester()?;
    let contents = indoc!(
        r#"
        def test():
            args = cmd_args("foo", "bar", delimiter = "-")
            assert_eq(["foo-bar"], get_args(args))
            "#
    );
    tester.run_starlark_bzl_test(contents)?;
    Ok(())
}
