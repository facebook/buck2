# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import typing

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test


def check_rule_type_names(
    res: BuckResult, expected_rule_type_names: typing.List[str]
) -> None:
    record = res.invocation_record()
    assert record["target_rule_type_names"] == expected_rule_type_names


@buck_test(write_invocation_record=True)
async def test_test_single_dep_touch(buck: Buck) -> None:
    res = await buck.test("//:rule1")
    check_rule_type_names(res, ["one"])


@buck_test(write_invocation_record=True)
async def test_test_two_out_of_order(buck: Buck) -> None:
    res = await buck.test("//:rule2", "//:rule1")
    check_rule_type_names(res, ["one", "two"])


@buck_test(write_invocation_record=True)
async def test_test_all_in_target(buck: Buck) -> None:
    res = await buck.test("//:")
    # Includes `not_a_test` because the recorder receives a
    # `TargetRuleTypeName` event for every CLI-resolved top-level target,
    # not only the ones that resolve to a test rule (T269576064).
    check_rule_type_names(res, ["not_a_test", "one", "two"])


@buck_test(write_invocation_record=True)
async def test_test_all_recursive(buck: Buck) -> None:
    res = await buck.test("//...")
    check_rule_type_names(res, ["not_a_test", "one", "two"])


@buck_test(write_invocation_record=True)
async def test_test_non_test_rule_logs_actual_rule_type(buck: Buck) -> None:
    # `buck test` against a non-test rule logs that rule's actual type in
    # `target_rule_type_names`.
    res = await buck.test("//:not_a_test_target")
    check_rule_type_names(res, ["not_a_test"])
