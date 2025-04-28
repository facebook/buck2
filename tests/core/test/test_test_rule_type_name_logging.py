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
    test_response = await filter_events(
        buck,
        "Result",
        "result",
        "test_response",
    )
    assert len(test_response) == 1
    assert test_response[0]["target_rule_type_names"] == expected_rule_type_names


@buck_test()
async def test_test_single_dep_touch(buck: Buck) -> None:
    await buck.test(
        "//:rule1",
    )
    await check_rule_type_names(buck, ["one"])


@buck_test()
async def test_test_two_out_of_order(buck: Buck) -> None:
    await buck.test(
        "//:rule2",
        "//:rule1",
    )
    await check_rule_type_names(buck, ["one", "two"])


@buck_test()
async def test_test_all_in_target(buck: Buck) -> None:
    await buck.test(
        "//:",
    )
    await check_rule_type_names(
        buck,
        [
            "one",
            "one",
            "two",
        ],
    )


@buck_test()
async def test_test_all_recursive(buck: Buck) -> None:
    await buck.test(
        "//...",
    )
    await check_rule_type_names(
        buck,
        [
            "one",
            "one",
            "two",
        ],
    )
