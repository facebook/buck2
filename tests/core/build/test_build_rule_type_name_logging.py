# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


async def check_rule_type_names(
    buck: Buck, expected_rule_type_names: typing.List[typing.Optional[str]]
) -> None:
    rule_names = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
        "build_targets",
    )
    rule_names = rule_names[0]
    assert len(rule_names) == len(expected_rule_type_names)
    for actual, expected in zip(rule_names, expected_rule_type_names):
        if expected is not None:
            assert actual["target_rule_type_name"] == expected


@buck_test()
async def test_build_nested_subtargets(buck: Buck) -> None:
    await buck.build(
        "//:nested[sub][nested_sub]",
    )
    await check_rule_type_names(buck, ["nested_subtargets"])


@buck_test()
async def test_build_single_dep_touch(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
    )
    await check_rule_type_names(buck, ["one"])


@buck_test()
async def test_build_two_out_of_order(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
        "//:nested[sub][nested_sub]",
    )
    await check_rule_type_names(buck, ["nested_subtargets", "one"])


@buck_test()
async def test_build_rule_with_transition(buck: Buck) -> None:
    await buck.build(
        "//:a_writer_with_transition",
    )

    await check_rule_type_names(buck, ["three_with_transition"])


@buck_test()
async def test_build_all_in_target(buck: Buck) -> None:
    await buck.build(
        "//:",
    )
    await check_rule_type_names(
        buck,
        [
            "two",
            "three_with_transition",
            "nested_subtargets",
            "one",
            "one",
            "platform",
        ],
    )


@buck_test()
async def test_build_all_recursive(buck: Buck) -> None:
    await buck.build(
        "//...",
    )
    await check_rule_type_names(
        buck,
        [
            "two",
            "three_with_transition",
            "nested_subtargets",
            "one",
            "one",
            "platform",
        ],
    )
