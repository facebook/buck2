# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events, json_get, random_string


@buck_test(data_dir="materialize_inputs_for_failed_actions")
async def test_materialize_inputs_for_failed_actions(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:action_fail",
            "--remote-only",
            "--no-remote-cache",
            "--materialize-failed-inputs",
            "-c",
            f"test.cache_buster={random_string()}",
        ),
    )

    log = (await buck.log("show")).stdout.strip().splitlines()

    found_action_error = False
    found_materialize_failed_inputs_span = False

    for line in log:
        # Look for MaterializeFailedInputs ReStage event
        if "MaterializeFailedInputs" in line:
            found_materialize_failed_inputs_span = True

        # Inspect failed input
        materialized_inputs_for_failed = json_get(
            line,
            "Event",
            "data",
            "Instant",
            "data",
            "ActionError",
            "last_command",
            "details",
            "command_kind",
            "command",
            "RemoteCommand",
            "materialized_inputs_for_failed",
        )

        if materialized_inputs_for_failed:
            found_action_error = True
            assert len(materialized_inputs_for_failed) == 1
            input = materialized_inputs_for_failed[0]
            with open(Path(buck.cwd / input), "r") as materialized_input_path:
                contents = materialized_input_path.read()
                assert contents == "yay!"

    if not found_action_error:
        raise AssertionError("Did not find relevant ActionError")
    if not found_materialize_failed_inputs_span:
        raise AssertionError("Did not find relevant MaterializeFailedInputs span")


@buck_test(data_dir="materialize_outputs_for_failed_actions")
async def test_materialize_outputs_for_failed_actions(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:action_fail",
            "--remote-only",
            "--materialize-failed-outputs",
        ),
    )

    materialized = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "ActionError",
        "last_command",
        "details",
        "command_kind",
        "command",
        "RemoteCommand",
        "materialized_outputs_for_failed_actions",
    )
    assert len(materialized) == 1 and len(materialized[0]) == 2


@buck_test(data_dir="materialize_outputs_for_failed_actions")
async def test_undeclared_outputs_to_materialize_will_fail(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:undeclared_output",
            "--remote-only",
            "--no-remote-cache",
        ),
        stderr_regex="marked to be materialized on failure but is not declared as an output of the action",
    )


@buck_test(data_dir="materialize_outputs_for_failed_actions")
async def test_materialize_outputs_defined_by_run_action(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "//:action_fail",
            "--remote-only",
            "--no-remote-cache",
        ),
    )

    materialized = await filter_events(
        buck,
        "Event",
        "data",
        "Instant",
        "data",
        "ActionError",
        "last_command",
        "details",
        "command_kind",
        "command",
        "RemoteCommand",
        "materialized_outputs_for_failed_actions",
    )

    found_action_error = False
    if materialized:
        found_action_error = True
        assert len(materialized[0]) == 1
        materialized[0][0].endswith("failed_action.json")

    if not found_action_error:
        raise AssertionError("Did not find relevant ActionError")
