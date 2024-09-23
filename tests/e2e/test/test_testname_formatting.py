# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import re
from typing import List

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
@pytest.mark.parametrize("adapter", ["legacy", "new"])
@pytest.mark.parametrize("listing", ["static", "dynamic"])
@pytest.mark.parametrize("python_version", ["3.10", "3.12"])
async def testname_formatting(
    buck: Buck,
    adapter: str,
    listing: str,
    python_version: str,
) -> None:
    target = f"{adapter}_{listing}_{python_version}"

    if python_version == "3.12" and adapter == "new":
        pytest.xfail("Test name formatting is different in 3.12")  # pyre-ignore[29]

    await expect_failure(
        buck.test(
            f"fbcode//buck2/tests/targets/rules/python/test_name_formatting:{target}",
        )
    )
    log = (await buck.log("show")).stdout.strip().splitlines()
    test_results_paths = get_test_results_path(log)
    assert test_results_paths
    actual_tests = get_events_test_names(test_results_paths)
    expected_tests = [
        "test_failure (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        "test_nested_test_class (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        "test_success (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
    ]
    assert expected_tests == actual_tests


#########
# Helpers
#########


def get_test_results_path(log: list[str]) -> List[str]:
    tpx_paths: List[str] = []
    for line in log:
        candidate_line = re.search(
            r"/data[\w/.-]+/tpx_execution_dir/test_results\.json", line
        )
        if candidate_line:
            tpx_paths.append(candidate_line.group())
    return tpx_paths


def get_event_test_name_from_test_results(
    test_results: list[str], event_name: str
) -> str:
    for test_result in test_results:
        data = json.loads(test_result)
        event = data.get(event_name)
        if event:
            return str(event.get("name"))
    return ""


def get_events_test_names(tpx_test_result_paths: List[str]) -> List[str]:
    test_runs = set()
    for tpx_test_result_path in tpx_test_result_paths:
        f = open(tpx_test_result_path, "r")
        test_results = f.readlines()
        f.close()
        test_name = get_event_test_name_from_test_results(test_results, "finish")
        test_runs.add(test_name)
    return sorted(test_runs)
