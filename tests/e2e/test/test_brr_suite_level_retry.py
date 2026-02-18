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

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform


def remove_ansi_escape_sequences(ansi_str: str) -> str:
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", ansi_str)


PYTHON_TEST_TARGET: str = "fbcode//buck2/tests/targets/rules/python/test:test"


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_suite_level_main_runs_all_tests(buck: Buck, tmp_path: Path) -> None:
    """
    When a BRR input file contains a suite-level '- main' entry (indicating
    a run_as_bundle test failed in the original run), the retry should run all
    tests from that suite rather than filtering them all out.
    """
    mode = get_mode_from_platform()

    # Step 1: Run the test normally to get a valid run_id from the test infra
    first_run = await buck.test(PYTHON_TEST_TARGET, mode)
    run_id_match = re.search(r"testinfra/testrun/(\d+)", first_run.stderr)
    assert run_id_match, "Could not extract run_id from test output"
    run_id = run_id_match.group(1)

    # Step 2: Create BRR file with suite-level "- main" entry, simulating a
    # run_as_bundle failure in the original run
    brr_file = tmp_path / "brr_input.json"
    brr_data = {
        "suite_name": PYTHON_TEST_TARGET,
        "fully_qualified_target_name": PYTHON_TEST_TARGET,
        "test_names": [f"{PYTHON_TEST_TARGET} - main"],
        "run_id": run_id,
        "test_type": 5,
        "test_config": {
            "config": "",
            "host": "linux",
            "mode": "@fbcode//mode/dev",
        },
    }
    brr_file.write_text(json.dumps(brr_data))

    # Step 3: Run with BRR retry — the fix converts "- main" to a prefix
    # matcher so individual test cases from the suite are included.
    # BRR mode may exit non-zero (code 32) even when tests pass, so we
    # catch BuckException and check stderr from whichever path we get.
    try:
        result = await buck.test(
            PYTHON_TEST_TARGET,
            mode,
            "--",
            "--base-rev-retry-with-input-file",
            str(brr_file),
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # Step 4: Assert tests actually ran (before the fix, this was "NO TESTS RAN")
    stderr = remove_ansi_escape_sequences(stderr)
    assert "NO TESTS RAN" not in stderr, (
        "BRR retry with suite-level '- main' entry should run tests, not skip them"
    )
    assert "Pass" in stderr


BROKEN_RUN_AS_BUNDLE_TARGET: str = (
    "fbcode//testinfra/playground/python/broken_run_as_bundle:broken_run_as_bundle_test"
)


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_roundtrip_run_as_bundle_failure(buck: Buck, tmp_path: Path) -> None:
    """
    End-to-end BRR roundtrip: a run_as_bundle test that fails produces a
    report with '- main', and feeding that report back via
    --base-rev-retry-with-input-file reproduces the failure (the retry
    report also contains '- main').
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"
    retry_report_file = tmp_path / "retry_report.json"

    # Step 1: Run the broken run_as_bundle target and save the failure report.
    # The test will fail (fatal error), so we expect BuckException.
    try:
        await buck.test(
            BROKEN_RUN_AS_BUNDLE_TARGET,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FATAL=1",
            "--save-failures-for-retry-in-file",
            str(report_file),
        )
        raise AssertionError("Expected BuckException from broken run_as_bundle target")
    except BuckException:
        pass

    # Step 2: Verify the report was written and contains a "- main" entry.
    assert report_file.exists(), "Failure report was not written"
    report = json.loads(report_file.read_text())
    test_names = report.get("test_names", [])
    has_main = any(name.endswith("- main") for name in test_names)
    assert has_main, f"Expected a '- main' entry in test_names, got: {test_names}"

    # Step 3: Feed the report back as a BRR retry input, saving the retry
    # output to a second report file so we can inspect it directly.
    try:
        await buck.test(
            BROKEN_RUN_AS_BUNDLE_TARGET,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FATAL=1",
            "--base-rev-retry-with-input-file",
            str(report_file),
            "--save-failures-for-retry-in-file",
            str(retry_report_file),
        )
    except BuckException:
        pass

    # Step 4: The retry must reproduce the failure.  Verify by
    # checking the retry report file rather than parsing stderr.
    assert retry_report_file.exists(), "Retry failure report was not written"
    retry_report = json.loads(retry_report_file.read_text())
    retry_test_names = retry_report.get("test_names", [])
    retry_has_main = any(name.endswith("- main") for name in retry_test_names)
    assert retry_has_main, (
        f"Expected a '- main' entry in retry report test_names, got: {retry_test_names}"
    )
    print(retry_report)


BROKEN_LISTING_TARGET: str = (
    "fbcode//testinfra/playground/python/broken_listing:broken_listing_test"
)


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_roundtrip_listing_failure(buck: Buck, tmp_path: Path) -> None:
    """
    End-to-end BRR roundtrip: a test whose listing fails (os._exit(1) at
    import time with supports_static_listing=False) produces a report with
    '- main', and feeding that report back via
    --base-rev-retry-with-input-file reproduces the listing failure (the
    retry report also contains '- main').
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"
    retry_report_file = tmp_path / "retry_report.json"

    # Step 1: Run the broken listing target and save the failure report.
    # With TPX_PLAYGROUND_FATAL=1, the test crashes at import time which
    # kills dynamic listing (supports_static_listing=False).
    try:
        await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FATAL=1",
            "--save-failures-for-retry-in-file",
            str(report_file),
        )
        raise AssertionError("Expected BuckException from broken listing target")
    except BuckException:
        pass

    # Step 2: Verify the report was written and contains a "- main" entry.
    assert report_file.exists(), "Failure report was not written"
    report = json.loads(report_file.read_text())
    test_names = report.get("test_names", [])
    has_main = any(name.endswith("- main") for name in test_names)
    assert has_main, f"Expected a '- main' entry in test_names, got: {test_names}"

    # Step 3: Feed the report back as a BRR retry input, saving the retry
    # output to a second report file so we can inspect it directly.
    try:
        await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FATAL=1",
            "--base-rev-retry-with-input-file",
            str(report_file),
            "--save-failures-for-retry-in-file",
            str(retry_report_file),
        )
    except BuckException:
        pass

    # Step 4: The retry must reproduce the listing failure.  Verify by
    # checking the retry report file rather than parsing stderr.
    assert retry_report_file.exists(), "Retry failure report was not written"
    retry_report = json.loads(retry_report_file.read_text())
    retry_test_names = retry_report.get("test_names", [])
    retry_has_main = any(name.endswith("- main") for name in retry_test_names)
    assert retry_has_main, (
        f"Expected a '- main' entry in retry report test_names, got: {retry_test_names}"
    )
    print(retry_report)


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_transient_listing_failure_runs_tests(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Transient listing failure: listing fails in the original run (producing
    '- main' in the BRR file), but succeeds on the retry. The fix in
    get_filter() converts '- main' to a prefix matcher so individual test
    cases from the suite pass through the filter and actually run, producing
    a result for the originally-failed test.

    Without the fix, RegexFilter::exact(["target - main"]) would not match
    any discovered test cases, causing NO TESTS RAN and no result produced
    for the failed test.
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"
    retry_report_file = tmp_path / "retry_report.json"

    # Step 1: Run with TPX_PLAYGROUND_FATAL=1 to make listing crash.
    try:
        await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FATAL=1",
            "--save-failures-for-retry-in-file",
            str(report_file),
        )
        raise AssertionError("Expected BuckException from broken listing target")
    except BuckException:
        pass

    # Step 2: Verify the report has "- main".
    assert report_file.exists(), "Failure report was not written"
    report = json.loads(report_file.read_text())
    test_names = report.get("test_names", [])
    has_main = any(name.endswith("- main") for name in test_names)
    assert has_main, f"Expected '- main' in test_names, got: {test_names}"

    # Step 3: Feed the report back WITHOUT TPX_PLAYGROUND_FATAL — listing
    # succeeds this time. The BRR filter must let tests through so that
    # a result is produced for the originally-failed test.
    try:
        result = await buck.test(
            BROKEN_LISTING_TARGET,
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
    # What matters is that the suite was not silently dropped by the filter.
    # Since listing succeeds on retry, the tests run and pass (they are
    # simple stubs), so we verify tests passed rather than checking for a
    # failure report — there are no failures to report.
    stderr = remove_ansi_escape_sequences(stderr)
    assert "NO TESTS RAN" not in stderr, (
        "BRR retry with transient listing failure should produce results, "
        "not silently skip the suite"
    )
    assert "Pass" in stderr
