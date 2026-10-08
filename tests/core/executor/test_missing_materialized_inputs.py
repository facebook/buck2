# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import os
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events, json_get


async def _command_execution(buck: Buck) -> dict[str, Any]:
    raw_log = (await buck.log("show")).stdout
    log = raw_log.strip().splitlines()
    for line in log:
        commands = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "commands",
        )
        if commands:
            return commands[-1]
    raise AssertionError(
        f"Did not find a command execution in the event log:\n{raw_log}"
    )


@buck_test(skip_for_os=["windows"])
async def test_missing_test_binary_reports_missing_materialized_input(
    buck: Buck,
) -> None:
    # The first run builds and materializes the test binary, and the test passes.
    await buck.test("//:test_binary", "--local-only", "--no-remote-cache")
    result = await buck.build("//:test_binary")
    binary = result.get_build_report().output_for_target("//:test_binary")

    os.unlink(binary)

    # Nothing is rebuilt because Buck still believes the binary is on disk, so
    # the test command fails to spawn.
    await expect_failure(
        buck.test("//:test_binary", "--local-only", "--no-remote-cache")
    )
    test_runs = await filter_events(buck, "Event", "data", "SpanEnd", "data", "TestRun")
    assert test_runs, "Expected a TestRun span for the second run"
    status = test_runs[-1]["command_report"]["status"]
    assert status == {"Failure": {"missing_materialized_inputs": True}}, status


@buck_test(skip_for_os=["windows"])
async def test_missing_undeclared_executable_is_not_a_missing_input(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:missing_undeclared_executable", "--local-only", "--no-remote-cache"
        )
    )
    command = await _command_execution(buck)
    assert command["status"] == {"Failure": {}}, command


@buck_test(skip_for_os=["windows"])
async def test_ordinary_failure_json_stays_empty(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:ordinary_failure", "--local-only", "--no-remote-cache")
    )
    command = await _command_execution(buck)
    assert command["status"] == {"Failure": {}}
