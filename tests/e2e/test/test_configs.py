# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_no_args(buck: Buck) -> None:
    buck_config = await execute_test_with_args(buck, [])

    # TODO: CI uses mode=@fbcode//mode/dev, once it's fixed in buck, please update the test accordingly
    assert_buck_args_config_equal(
        buck_config,
        {
            "mode": "",
            "config": "",
            "host": "linux",
        },
    )


@buck_test(inplace=True)
async def test_dev_mode(buck: Buck) -> None:
    buck_config = await execute_test_with_args(buck, ["@fbcode//mode/dev"])

    assert_buck_args_config_equal(
        buck_config,
        {
            "mode": "@fbcode//mode/dev",
            "config": "",
            "host": "linux",
        },
    )


#########
# Helpers
#########


# run the buck test command and return the actual used buck config
async def execute_test_with_args(buck: Buck, args: list[str]) -> dict[str, str]:
    tpx_trace_path = tempfile.NamedTemporaryFile(delete=False)
    await buck.test(
        *args,
        "fbcode//testinfra/playground/cpp/tests:test_example",
        "--",
        "--trace-file-path",
        tpx_trace_path.name,
    )

    return get_buck_config(tpx_trace_path.name)


def get_buck_config(tpx_trace_path: str) -> dict[str, str]:
    # Each row in the log is a json string
    # Find the first one with 'event name == run.external_buck_config_finalized'
    with open(tpx_trace_path, encoding="utf-8") as f:
        for _, line in enumerate(f):
            data = json.loads(line)
            if "fields" in data and "event_name" in data["fields"]:
                if data["fields"]["event_name"] == "run.external_buck_config_finalized":
                    return dict(json.loads(data["fields"]["config"]))
    return {}


def assert_buck_args_config_equal(
    actual_config: dict[str, str], expected_config: dict[str, str]
) -> None:
    assert (
        actual_config == expected_config
    ), f"Expected {expected_config}, got {actual_config}"
