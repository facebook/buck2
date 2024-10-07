# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import typing
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


async def check_targets(
    buck: Buck,
    expected_target_names: typing.List[str],
    expected_error_messages: typing.List[str],
) -> None:
    build_response = await filter_events(
        buck,
        "Result",
        "result",
        "build_response",
    )
    build_response = build_response[0]
    build_targets = build_response["build_targets"]
    assert len(build_targets) == len(expected_target_names)
    for actual, expected in zip(build_targets, expected_target_names):
        if expected is not None:
            assert actual["target"] == expected
    error_messages = build_response["errors"]
    assert len(error_messages) == len(expected_error_messages)
    for actual_msg, expected in zip(error_messages, expected_error_messages):
        if expected is not None:
            assert expected in actual_msg["message"]


@buck_test()
async def test_build_one_fails(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "build-report.json"
    await expect_failure(
        buck.build(
            "--build-report",
            str(report),
            "//:fail",
            "//:a_one",
        ),
        stderr_regex="Failed to build 'root//:fail",
    )
    await check_targets(
        buck,
        ["root//:a_one", "root//:fail"],
        ["Failed to build 'root//:fail (<unspecified>)'"],
    )
