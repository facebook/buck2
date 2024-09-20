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


class TestCaseRun:
    def __init__(self, start_event_test_name: str, stop_event_test_name: str) -> None:
        self.start_event_test_name = start_event_test_name
        self.stop_event_test_name = stop_event_test_name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, TestCaseRun):
            return (
                self.start_event_test_name == other.start_event_test_name
                and self.stop_event_test_name == other.stop_event_test_name
            )
        return False


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

    if python_version == "3.12":
        pytest.xfail("Test name formatting is different in 3.12")  # pyre-ignore[29]

    await expect_failure(
        buck.test(
            f"fbcode//buck2/tests/targets/rules/python/test_name_formatting:{target}",
        )
    )
    log = (await buck.log("show")).stdout.strip().splitlines()
    test_results_paths = get_test_results_path(log)
    assert test_results_paths
    test_runs = get_events_test_names(test_results_paths)
    assert (
        TestCaseRun(
            "test_success (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
            "test_success (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        )
    ) in test_runs
    assert (
        TestCaseRun(
            "test_failure (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
            "test_failure (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        )
    ) in test_runs
    assert (
        TestCaseRun(
            "test_nested_test_class (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
            "test_nested_test_class (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        )
    ) in test_runs


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


def get_events_test_names(tpx_test_result_paths: List[str]) -> List[TestCaseRun]:
    test_runs: List[TestCaseRun] = []
    for tpx_test_result_path in tpx_test_result_paths:
        f = open(tpx_test_result_path, "r")
        test_results = f.readlines()
        f.close()
        start_event_test_name = get_event_test_name_from_test_results(
            test_results, "start"
        )
        end_event_test_name = get_event_test_name_from_test_results(
            test_results, "finish"
        )
        test_runs.append(TestCaseRun(start_event_test_name, end_event_test_name))
    return test_runs
