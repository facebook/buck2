# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


# Tests to ensure that the log format for a few important fields hasn't
# changed. This ensures compatibility with downstream processing tools.
#
# If this test needs to be updated, please sync with @athmasagar or the
# fbcode_build_infra oncall to ensure that log parsers are also migrated.
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_log_format(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome")
    out = await buck.log("show")

    lines = [line.strip() for line in out.stdout.splitlines()]
    test_line = None
    for line in lines:
        if "output_size" in line and "cpu_instructions_user" in line:
            test_line = line
            break

    assert test_line is not None
    decoded_line = json.loads(test_line)

    span_end = decoded_line["Event"]["data"]["SpanEnd"]
    assert span_end["duration_us"] >= 0

    action_execution = span_end["data"]["ActionExecution"]
    assert action_execution["execution_kind"] >= 0

    target_label = action_execution["key"]["owner"]["TargetLabel"]
    label = target_label["label"]
    assert label["package"] is not None
    assert label["name"] is not None
    assert target_label["configuration"]["full_name"] is not None

    action_name = action_execution["name"]
    assert action_name["category"] is not None
    assert action_name["identifier"] is not None
    assert action_execution["output_size"] >= 0

    commands = action_execution["commands"]
    execution_stats = commands[-1]["details"]["metadata"]["execution_stats"]
    assert execution_stats["cpu_instructions_user"] >= 0
    assert execution_stats["userspace_events"]["count"] >= 0
    assert execution_stats["userspace_events"]["time_enabled"] >= 0
    assert execution_stats["userspace_events"]["time_running"] >= 0


# Placeholder for tests to be listed successfully on Windows.
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
