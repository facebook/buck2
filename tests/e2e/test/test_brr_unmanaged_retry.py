# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import re
from pathlib import Path

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform


def remove_ansi_escape_sequences(ansi_str: str) -> str:
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", ansi_str)


# (target, broken_method) pairs exercised by the BRR roundtrip below. Each target
# is an unmanaged test (TestCaseSelector::None): TPX runs it as a black box with
# no listing, but the testpilot adapter still reports the individual method
# result "target - <broken_method> (...)". `broken_method` is the substring that
# must show up in the failure report's test_names and be preserved across the BRR
# retry. Add a (target, method) tuple here to cover a new unmanaged target shape.
TARGET_AND_BROKEN_METHOD: list[tuple[str, str]] = [
    (
        "fbcode//testinfra/playground/python/broken_unmanaged:broken_unmanaged_test",
        "test_the_test",
    ),
]


def _case_id(case: tuple[str, str]) -> str:
    target, _method = case
    return target.rsplit(":", 1)[-1]


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
@pytest.mark.parametrize("target_and_method", TARGET_AND_BROKEN_METHOD, ids=_case_id)
async def test_brr_roundtrip_unmanaged_failure(
    buck: Buck, tmp_path: Path, target_and_method: tuple[str, str]
) -> None:
    """
    End-to-end BRR roundtrip for an unmanaged test.

    The testpilot adapter reports the individual method name, so the BRR file
    records "target - <broken_method> (...)" (not the suite synthetic). On retry,
    the test is still unmanaged (TestCaseSelector::None), so the filter matches
    the BRR name against the suite synthetic ("- main"/"- unmanaged"), which
    does not match the individual name -- the suite is silently filtered out and
    NO TESTS RAN.

    This test asserts the correct behavior: the retry should actually run the
    test and reproduce the failure.
    """
    target, broken_method = target_and_method
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"
    retry_report_file = tmp_path / "retry_report.json"

    # Step 1: Run the test with a failure. TPX_PLAYGROUND_FAIL makes the
    # test method fail, so TPX records "target - <broken_method> (...)" as a
    # failed case.
    await expect_failure(
        buck.test(
            target,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FAIL=1",
            "--save-failures-for-retry-in-file",
            str(report_file),
        )
    )

    # Step 2: Verify the report was written and contains the individual test
    # method name, NOT just the suite synthetic. This is the key difference
    # from the suite-level tests in test_brr_suite_level_retry.py.
    assert report_file.exists(), "Failure report was not written"
    report = json.loads(report_file.read_text())
    test_names = report.get("test_names", [])
    has_the_test = any(broken_method in name for name in test_names)
    assert has_the_test, (
        f"Expected a '{broken_method}' entry in test_names, got: {test_names}"
    )

    # Step 3: Feed the report back as a BRR retry input (still with the
    # failure trigger), saving a retry report. The retry must reproduce the
    # failure, so a BuckException is required here -- a passing retry means the
    # suite was not actually re-run.
    await expect_failure(
        buck.test(
            target,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FAIL=1",
            "--base-rev-retry-with-input-file",
            str(report_file),
            "--save-failures-for-retry-in-file",
            str(retry_report_file),
        )
    )

    # Step 4: The retry must actually run the test (not silently filter it
    # out). If the suite was filtered out, the retry report file is either
    # missing or empty (TPX creates the file but writes nothing when the
    # suite is filtered out).
    retry_report_text = (
        retry_report_file.read_text() if retry_report_file.exists() else ""
    )
    assert retry_report_text.strip(), (
        "Retry failure report is missing or empty -- the unmanaged suite was "
        "likely filtered out because the BRR filter had the individual test "
        f"name (e.g. '{broken_method}') that does not match the suite synthetic "
        "('- main'/'- unmanaged') for TestCaseSelector::None"
    )
    retry_report = json.loads(retry_report_text)
    retry_test_names = retry_report.get("test_names", [])
    retry_has_the_test = any(broken_method in name for name in retry_test_names)
    assert retry_has_the_test, (
        f"Expected '{broken_method}' in retry report test_names, got: {retry_test_names}"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
@pytest.mark.parametrize("target_and_method", TARGET_AND_BROKEN_METHOD, ids=_case_id)
async def test_brr_transient_unmanaged_failure_runs_tests(
    buck: Buck, tmp_path: Path, target_and_method: tuple[str, str]
) -> None:
    """
    Transient failure for an unmanaged test: the test method fails on the first
    run (TPX_PLAYGROUND_FAIL), but passes on the BRR retry.

    The BRR filter must not silently drop the unmanaged suite just because the
    individual test name "target - <broken_method> (...)" does not match the
    suite synthetic ("- main"/"- unmanaged"). The retry should run the full
    suite and report a passing result.
    """
    target, broken_method = target_and_method
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"
    retry_report_file = tmp_path / "retry_report.json"

    # Step 1: Run with TPX_PLAYGROUND_FAIL=1 to get a failure.
    await expect_failure(
        buck.test(
            target,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FAIL=1",
            "--save-failures-for-retry-in-file",
            str(report_file),
        )
    )

    # Step 2: Verify the report has the individual test name.
    assert report_file.exists(), "Failure report was not written"
    report = json.loads(report_file.read_text())
    test_names = report.get("test_names", [])
    has_the_test = any(broken_method in name for name in test_names)
    assert has_the_test, f"Expected '{broken_method}' in test_names, got: {test_names}"

    # Step 3: Feed the report back WITHOUT TPX_PLAYGROUND_FAIL — the test
    # passes this time. The BRR filter must let the unmanaged suite through
    # so that a result is produced.
    try:
        result = await buck.test(
            target,
            mode,
            "--",
            "--base-rev-retry-with-input-file",
            str(report_file),
            "--save-failures-for-retry-in-file",
            str(retry_report_file),
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # Step 4: The retry must have produced a result — not "NO TESTS RAN".
    # The suite should not be silently dropped by the filter just because
    # the BRR file had an individual test name that does not match the suite
    # synthetic.
    stderr = remove_ansi_escape_sequences(stderr)
    assert "NO TESTS RAN" not in stderr, (
        "BRR retry with a transient unmanaged failure should produce results, "
        "not silently skip the unmanaged suite. This happens because the BRR "
        f"filter has 'target - {broken_method} (...)' but the suite synthetic is "
        "'- main'/'- unmanaged' for TestCaseSelector::None."
    )
    assert "Pass" in stderr
