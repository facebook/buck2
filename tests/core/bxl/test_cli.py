# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import random
import string

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_bxl_cli(buck: Buck) -> None:
    result = await buck.bxl(
        "//cli_args.bxl:cli_test",
        "--",
        "--int_arg",
        "1",
        "--float_arg",
        "4.3",
        "--enum_type",
        "a",
        "--list_type",
        "1",
        "2",
        "3",
        "--target",
        ":foo",
        "--sub_target",
        "cell/pkg:bar[sub]",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nbool_arg_with_default: True\nstring_arg: "default"\nint_arg: 1\nfloat_arg: 4.3\noptional: None\nenum_type: "a"\ntarget: root//:foo\nsub_target: root//cell/pkg:bar[sub]\nlist: [1, 2, 3]\n'
    )

    result = await buck.bxl(
        "//cli_args.bxl:cli_test",
        "--",
        # Override default bool arg with false
        "--bool_arg_with_default",
        "false",
        "--int_arg",
        "2",
        "--float_arg",
        "3.4",
        "--optional",
        "value",
        "--enum_type",
        "b",
        "--list_type",
        "1",
        "--target",
        "bar:foo",
        "--sub_target",
        "cell/pkg:bar",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nbool_arg_with_default: False\nstring_arg: "default"\nint_arg: 2\nfloat_arg: 3.4\noptional: "value"\nenum_type: "b"\ntarget: root//bar:foo\nsub_target: root//cell/pkg:bar\nlist: [1]\n'
    )

    # multiple occurrences of a list-type argument
    # i.e., --arg 1 --arg 2 --arg 3
    result = await buck.bxl(
        "//cli_args.bxl:cli_test",
        "--",
        "--int_arg",
        "1",
        "--float_arg",
        "4.3",
        "--enum_type",
        "a",
        "--list_type",
        "1",
        "--list_type",
        "2",
        "--list_type",
        "3",
        "--target",
        ":foo",
        "--sub_target",
        "cell/pkg:bar[sub]",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nbool_arg_with_default: True\nstring_arg: "default"\nint_arg: 1\nfloat_arg: 4.3\noptional: None\nenum_type: "a"\ntarget: root//:foo\nsub_target: root//cell/pkg:bar[sub]\nlist: [1, 2, 3]\n'
    )

    # illegal target
    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:cli_test",
            "--",
            "--int_arg",
            "2",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "b",
            "--list_type",
            "1",
            "--target",
            "illegal?target",
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # not int
    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:cli_test",
            "--",
            "--int_arg",
            "2.0",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "b",
            "--list_type",
            "1",
            "--target",
            ":foo",
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # list inner type mismatch
    await expect_failure(
        buck.bxl(
            "//bxl:cli_args.bxl:cli_test",
            "--",
            "--int_arg",
            "2",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "b",
            "--list_type",
            "wrong_inner_list_type",
            "--target",
            "bar:foo",
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # not valid enum variant
    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:cli_test",
            "--",
            "--int_arg",
            "2",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "not_enum",
            "--list_type",
            "1",
            "--target",
            ":foo",
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # missing non-optional field
    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:cli_test",
            "--",
            "--int_arg",
            "2",
            "--optional",
            "value",
            "--enum_type",
            "a",
            "--list_type",
            "1",
            "--target",
            ":foo",
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # check short args work
    result = await buck.bxl(
        "//cli_args.bxl:cli_test_short",
        "--",
        "-i",
        "1",
        "-f",
        "4.3",
        "-e",
        "a",
        "-l",
        "1",
        "2",
        "3",
        "-t",
        ":foo",
        "-s",
        "default",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nstring_arg: "default"\nint_arg: 1\nfloat_arg: 4.3\noptional: None\nenum_type: "a"\ntarget: root//:foo\nlist: [1, 2, 3]\n'
    )

    # check long args still work with short args
    result = await buck.bxl(
        "//cli_args.bxl:cli_test_short",
        "--",
        "--int_arg",
        "1",
        "--float_arg",
        "4.3",
        "--enum_type",
        "a",
        "--list_type",
        "1",
        "2",
        "3",
        "--target",
        ":foo",
        "--string_arg",
        "default",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nstring_arg: "default"\nint_arg: 1\nfloat_arg: 4.3\noptional: None\nenum_type: "a"\ntarget: root//:foo\nlist: [1, 2, 3]\n'
    )

    # check snakecase cli_arg access from bxl context, make sure it still works with default args and shorthand args
    result = await buck.bxl(
        "//cli_args.bxl:cli_test_snakecase_access",
        "--",
        "--my-arg",
        "this is my arg",
    )
    assert result.stdout == 'my-arg: "this is my arg"\n'

    result = await buck.bxl(
        "//cli_args.bxl:cli_test_snakecase_access", "--", "-a", "this is my arg"
    )
    assert result.stdout == 'my-arg: "this is my arg"\n'

    result = await buck.bxl("//cli_args.bxl:cli_test_snakecase_access")
    assert result.stdout == 'my-arg: "default"\n'

    await expect_failure(
        buck.bxl(
            "//cli_args_bad_case.bxl:cli_test_bad_case",
            "--",
            "--my-arg",
            "this is my arg",
        )
    )


@buck_test()
async def test_bxl_cli_json_args(buck: Buck) -> None:
    json_args = {}
    json_args.update({"int": 1})
    json_args.update({"string": "foo"})
    json_args.update({"float": 1.0})
    json_args.update({"bool": True})
    json_args.update({"none": None})
    json_args.update({"list": [1, 2, 3]})

    nested = {}
    nested.update({"nested_string": "bar"})
    nested.update({"nested_int": -1})
    json_args.update({"nested": nested})

    my_json = json.dumps(json_args)

    await buck.bxl(
        "//cli_args.bxl:cli_json_arg",
        "--",
        "--my-json",
        my_json,
    )

    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:cli_json_arg",
            "--",
            "--my-json",
            "[1,2,3]",
        ),
        stderr_regex="Expecting json object. Got: `\\[1,2,3]\\`",
    )


@buck_test()
async def test_bxl_cli_short_bad(buck: Buck) -> None:
    # duplicate "short"
    await expect_failure(
        buck.bxl(
            "//cli_args_bad.bxl:cli_test_short_bad",
            "--",
            "-a",
            "2",
            "-a",
            "value",
            "-a",
            "a",
            "-a",
            "1",
            "-a",
            ":foo",
            "-a",
            "default",
        ),
        stderr_regex="Duplicate short args are not allowed",
    )


@buck_test()
async def test_cli_target_pattern(buck: Buck) -> None:
    result = await buck.bxl(
        "//cli_args.bxl:target_expr_test",
        "--",
        "--targets",
        ":t1",
    )
    assert "[root//:t1]" in result.stdout

    result = await buck.bxl(
        "//cli_args.bxl:target_expr_test",
        "--",
        "--targets",
        "root//:",
    )
    assert "root//:t1" in result.stdout
    assert "root//:t2" in result.stdout

    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            ":non-existent",
        )
    )

    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            "invalid/...",
        )
    )


@buck_test()
async def test_cli_sub_target_pattern(buck: Buck) -> None:
    # Tests where no sub-target is specified; should ensure functionality
    # of regular target patterns work with these subtarget patterns.
    result = await buck.bxl(
        "//cli_args.bxl:sub_target_expr_test",
        "--",
        "--sub_targets",
        ":t1",
    )
    print(result.stdout)
    assert "[root//:t1]" in result.stdout

    result = await buck.bxl(
        "//cli_args.bxl:sub_target_expr_test",
        "--",
        "--sub_targets",
        "root//:",
    )
    assert "root//:t1" in result.stdout
    assert "root//:t2" in result.stdout

    # Test single sub-targets.
    result = await buck.bxl(
        "//cli_args.bxl:sub_target_expr_test",
        "--",
        "--sub_targets",
        "root//:t1[sub]",
    )
    assert "[root//:t1[sub]" in result.stdout

    # Several subtargets / nested subtargets.
    result = await buck.bxl(
        "//cli_args.bxl:sub_target_expr_test",
        "--",
        "--sub_targets",
        "root//:t2[sub1][sub2]",
    )
    assert "[root//:t2[sub1][sub2]" in result.stdout

    await expect_failure(
        buck.bxl(
            "//cli_args.bxl:sub_target_expr_test",
            "--",
            "--sub_targets",
            ":fake_bin[sub]",
        )
    )


def random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))
