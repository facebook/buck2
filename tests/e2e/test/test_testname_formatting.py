# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
from typing import Any, List

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
@pytest.mark.parametrize("adapter", ["testpilot", "builtin"])
@pytest.mark.parametrize("listing", ["static", "dynamic"])
@pytest.mark.parametrize("python_version", ["3.12"])
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
    actual_tests = get_events_test_names(log)
    expected_tests = [
        "test_failure (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        "test_nested_test_class (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
        "test_success (buck2.tests.targets.rules.python.test_name_formatting.test_name_formatting.TestCase)",
    ]
    assert expected_tests == actual_tests


#########
# Helpers
#########


def get_test_name_from_end_event(event: Any) -> List[str]:
    return event["Event"]["data"]["SpanEnd"]["data"]["TestEnd"]["suite"]["test_names"]


def get_events_test_names(log: List[str]) -> List[str]:
    test_end_events = [json.loads(line) for line in log if "TestEnd" in line]
    test_end_events = [
        test_name
        for test_event in test_end_events
        for test_name in get_test_name_from_end_event(test_event)
    ]
    return sorted(test_end_events)
