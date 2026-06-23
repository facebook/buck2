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
from typing import cast

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform


def remove_ansi_escape_sequences(ansi_str: str) -> str:
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", ansi_str)


def read_brr_report(path: Path) -> dict[str, object]:
    """Read a JSONL BRR report file and merge all lines into a single dict.

    TPX writes one JSON object per line (one per acked failure). This helper
    parses every line and collects all test_names into a single combined dict
    so callers can inspect the aggregate report.
    """
    lines = [json.loads(line) for line in path.read_text().splitlines() if line.strip()]
    assert lines, f"BRR report file is empty: {path}"
    merged = dict(lines[0])
    all_test_names: list[str] = []
    for line in lines:
        all_test_names.extend(line.get("test_names", []))
    merged["test_names"] = all_test_names
    return merged


# status_detailed codes in the tpx event log. See test_result::TestResultStatus.
TEST_NOT_EXISTS_STATUS: int = 404
FAILED_STATUS: int = 501
FATAL_STATUS: int = 502


def event_log_has_test_not_exists(path: Path) -> bool:
    """True if any event in the JSONL event log is a TEST_NOT_EXISTS phantom."""
    if not path.exists():
        return False
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        if event.get("status_detailed") == TEST_NOT_EXISTS_STATUS:
            return True
    return False


def event_log_result_names(path: Path) -> set[str]:
    """Names of tests that produced a real result row (have a status_detailed)
    in the JSONL event log. Excludes non-result events like the run_id line."""
    names: set[str] = set()
    if not path.exists():
        return names
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        if "test_name" in event and event.get("status_detailed") is not None:
            names.add(cast(str, event["test_name"]))
    return names


def event_log_results(path: Path) -> dict[str, int]:
    """Map of test_name -> status_detailed for every result row in the event log."""
    results: dict[str, int] = {}
    if not path.exists():
        return results
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        status = event.get("status_detailed")
        if "test_name" in event and status is not None:
            results[cast(str, event["test_name"])] = cast(int, status)
    return results


def print_testrun_url(label: str, output: str) -> None:
    """Print the TestX testrun URL parsed from a buck.test output, labelled by
    run type (e.g. "target run" / "base-rev retry"). Each contract does two
    buck.test calls, so this surfaces both testruns for inspection."""
    m = re.search(r"testinfra/testrun/\d+", output)
    url = f"https://www.internalfb.com/intern/{m.group(0)}" if m else "<no testrun>"
    print(f"[BRR-contract] {label}: {url}")


PYTHON_TEST_TARGET: str = "fbcode//buck2/tests/targets/rules/python/test:test"

# Bundle-mode target with 3 test cases (test_a, test_b, test_c). Used for
# the SRR `--exact` tests below because the SRR path needs to demonstrate
# both single-test selection and suite-level expansion against a target
# with multiple discoverable tests.
PYTHON_MULTI_TESTS_TARGET: str = (
    "fbcode//buck2/tests/targets/rules/python/test:multi_tests"
)


# Full match-name for `multi_tests.TestCase.test_a` in the format tpx's
# `--exact` filter compares against: `<suite> - <display_name>`, where
# `<display_name>` is `<method> (<full.module.path.Class>)` per the legacy
# python unittest listing parser (tpx-buck/src/listing/python.rs).
TEST_A_MATCH_NAME: str = (
    f"{PYTHON_MULTI_TESTS_TARGET} - test_a "
    "(buck2.tests.targets.rules.python.test.multi_tests.TestCase)"
)
MAIN_MATCH_NAME: str = f"{PYTHON_MULTI_TESTS_TARGET} - main"


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_srr_exact_with_main_expands_to_full_suite(
    buck: Buck, tmp_path: Path
) -> None:
    # GIVEN: a bundle-mode python target with 3 discoverable test cases
    # (test_a, test_b, test_c) and the patched tpx whose `--exact` filter
    # converts synthetic `- main` / `- unmanaged` suffixes into prefix
    # regexes `^<suite> - ` (suite-level expansion).
    mode = get_mode_from_platform()

    # WHEN: SRR invokes tpx with two `--exact` names — a real existing
    # test case plus the synthetic suite-level `- main`. This is the SRR
    # shape produced when TARGET had both a specific test failure and a
    # suite-level fatal.
    try:
        result = await buck.test(
            PYTHON_MULTI_TESTS_TARGET,
            mode,
            "--",
            "--exact",
            TEST_A_MATCH_NAME,
            MAIN_MATCH_NAME,
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # THEN: the synthetic `- main` entry expands to `^<target> - ` and
    # matches all 3 discovered cases (test_a/b/c). tpx runs them as a
    # bundle so the bundle worker reports too — Pass 4 total (3 children +
    # main worker). SRR now uploads real AV data for the suite-level
    # failure instead of silently running just the specific test.
    stderr = remove_ansi_escape_sequences(stderr)
    assert "Pass 4" in stderr, (
        "Synthetic '- main' should expand to the whole suite, "
        f"running all 3 children + bundle worker = Pass 4, stderr was:\n{stderr}"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_srr_exact_single_test_stays_narrow(buck: Buck, tmp_path: Path) -> None:
    # GIVEN: a bundle-mode python target with 3 discoverable test cases
    # (test_a, test_b, test_c) and any tpx version — this test's
    # assertion holds in both pre-fix and post-fix states (regression
    # guard ensuring the fix only widens for synthetic suite-level names).
    mode = get_mode_from_platform()

    # WHEN: SRR invokes tpx with `--exact` on a single real existing test
    # case name. No synthetic `- main`/`- unmanaged` suffix is present.
    try:
        result = await buck.test(
            PYTHON_MULTI_TESTS_TARGET,
            mode,
            "--",
            "--exact",
            TEST_A_MATCH_NAME,
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # THEN: exactly the one named test runs. The suite-level expansion
    # must NOT accidentally widen the filter when no synthetic entry is
    # present, or `--exact` would silently grow the set of tests SRR
    # executes for any caller. Bundle mode emits a synthetic `- main`
    # aggregate alongside the executed case, so the observed count is
    # `Pass 2` (test_a + bundle `- main`) both pre-fix and post-fix.
    stderr = remove_ansi_escape_sequences(stderr)
    assert "Pass 2" in stderr, (
        "Regression guard: SRR --exact with a single real test name "
        f"should match only that one test, stderr was:\n{stderr}"
    )


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


# Auto-generated pyrefly type-checking target. It is UNMANAGED (non-listable):
# it reports per-file results and never goes through test-case listing — the same
# shape as the `:llama4x-static-type-check` target in the original Bug 7 report.
TYPE_CHECK_UNMANAGED_TARGET: str = (
    "fbcode//testinfra/playground/python/pytest:pytest_test-library-type-checking"
)


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_unmanaged_existing_and_synthetic_new_no_phantom(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Simulate, in a single BRR run, an unmanaged suite that has BOTH a real
    pre-existing entry AND a synthetic "new" entry (a case name the suite will
    never list). With the fix, the suite is cleared at discovery
    (FbTestSuite::is_unmanaged), so NEITHER entry yields a phantom
    TEST_NOT_EXISTS (404). Before the fix the synthetic per-case entry survived
    the not-listed tracker and produced a phantom 404.

    This also pins the known GAP: a genuinely-new unmanaged test cannot be
    reported as "new". New-test detection relies on listing, which unmanaged
    suites never emit, so the synthetic entry produces NO result at all (no 404,
    no skip) — it is simply dropped. Land-blocking is unaffected (a real new
    unmanaged failure still propagates as FAILED from the target run); only the
    new-test classification/reporting is missing. Restoring it (via execution-diff
    detection on the base rerun) is tracked as a follow-up.
    """
    mode = get_mode_from_platform()

    # Step 1: Run the unmanaged type-check target normally to get a valid run_id.
    first_run = await buck.test(TYPE_CHECK_UNMANAGED_TARGET, mode)
    run_id_match = re.search(r"testinfra/testrun/(\d+)", first_run.stderr)
    assert run_id_match, "Could not extract run_id from test output"
    run_id = run_id_match.group(1)

    # Step 2: BRR file with a real existing unmanaged entry plus a synthetic new
    # per-case entry the suite will never list.
    existing_name = f"{TYPE_CHECK_UNMANAGED_TARGET} - unmanaged"
    synthetic_new_name = f"{TYPE_CHECK_UNMANAGED_TARGET} - brr_synthetic_new_test.py"
    brr_file = tmp_path / "brr_input.json"
    brr_data = {
        "suite_name": TYPE_CHECK_UNMANAGED_TARGET,
        "fully_qualified_target_name": TYPE_CHECK_UNMANAGED_TARGET,
        "test_names": [existing_name, synthetic_new_name],
        "run_id": run_id,
        "test_type": 5,
        "test_config": {
            "config": "",
            "host": "linux",
            "mode": "@fbcode//mode/dev",
        },
    }
    brr_file.write_text(json.dumps(brr_data))

    # Step 3: Base-revision retry with new-test detection enabled, capturing the
    # tpx event log. BRR mode may exit non-zero, so inspect the log regardless.
    event_log = tmp_path / "events.ndjson"
    try:
        await buck.test(
            TYPE_CHECK_UNMANAGED_TARGET,
            mode,
            "--",
            "--base-rev-retry-with-input-file",
            str(brr_file),
            "--experiment",
            "report-new-test-not-exists-status-for-brr",
            "--event-log-file",
            str(event_log),
        )
    except BuckException:
        pass

    assert event_log.exists(), "Event log file was not written"

    # No phantom TEST_NOT_EXISTS for either entry (the fix clears unmanaged suites
    # at discovery). Pre-fix, the synthetic per-case entry produced a 404.
    assert not event_log_has_test_not_exists(event_log), (
        "Unmanaged suite must not produce a phantom TEST_NOT_EXISTS, even with a "
        f"synthetic new entry; event log was:\n{event_log.read_text()}"
    )

    # The existing unmanaged suite still RUNS on the base rerun (clearing the
    # tracker does not suppress execution): its `- unmanaged` result is present.
    # The synthetic new entry produces NO result row of any kind (no 404, no
    # skip) — it is simply dropped. So only the synthetic one fails to run.
    result_names = event_log_result_names(event_log)
    assert existing_name in result_names, (
        "Existing unmanaged test should still run on the BRR base rerun, but no "
        f"result was emitted for it. Results: {result_names}"
    )
    assert synthetic_new_name not in result_names, (
        "Synthetic new unmanaged entry must not run / be reported (dropped at "
        f"discovery). Results: {result_names}"
    )


# Landable unmanaged sh_test whose failure is driven on demand by the
# BRR_SIM_FAIL env var (passes in prod CI; the e2e flips it via `--env`). Used by
# the two BRR contracts below to simulate a *failing* unmanaged test.
BRR_SIM_UNMANAGED_TARGET: str = "fbcode//testinfra/playground/sh:brr_sim_unmanaged"


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_unmanaged_preexisting_failure(buck: Buck, tmp_path: Path) -> None:
    """
    CONTRACT — pre-existing failing unmanaged test.

    An unmanaged test that failed on the target run and *fails again* on the
    base-revision retry is a pre-existing failure (it exists on base). We drive
    the failure on demand with `--env BRR_SIM_FAIL=1` so the target is still safe
    to run in production (it passes by default).

    The retry runs the real, discovered unmanaged suite, so the fix clears it at
    discovery: there is NO phantom TEST_NOT_EXISTS, and the `- unmanaged` result
    is actually produced (it ran and failed on base) — i.e. pre-existing.
    """
    mode = get_mode_from_platform()
    fail_env = ["--env", "BRR_SIM_FAIL=1"]

    # Step 1: target run — the unmanaged test FAILS on demand. Capture its run_id.
    try:
        first_run = await buck.test(BRR_SIM_UNMANAGED_TARGET, mode, "--", *fail_env)
        stderr = first_run.stderr
    except BuckException as e:
        stderr = e.stderr
    run_id_match = re.search(r"testinfra/testrun/(\d+)", stderr)
    assert run_id_match, f"Could not extract run_id from test output:\n{stderr}"
    run_id = run_id_match.group(1)
    print_testrun_url("Contract 1 (pre-existing) target run", stderr)

    # Step 2: BRR file naming the suite-level `- unmanaged` entry that failed.
    existing_name = f"{BRR_SIM_UNMANAGED_TARGET} - unmanaged"
    brr_file = tmp_path / "brr_input.json"
    brr_file.write_text(
        json.dumps(
            {
                "suite_name": BRR_SIM_UNMANAGED_TARGET,
                "fully_qualified_target_name": BRR_SIM_UNMANAGED_TARGET,
                "test_names": [existing_name],
                "run_id": run_id,
                "test_type": 5,
                "test_config": {
                    "config": "",
                    "host": "linux",
                    "mode": "@fbcode//mode/dev",
                },
            }
        )
    )

    # Step 3: base-revision retry — still failing on base (BRR_SIM_FAIL=1).
    event_log = tmp_path / "events.ndjson"
    try:
        retry = await buck.test(
            BRR_SIM_UNMANAGED_TARGET,
            mode,
            "--",
            *fail_env,
            "--base-rev-retry-with-input-file",
            str(brr_file),
            "--experiment",
            "report-new-test-not-exists-status-for-brr",
            "--event-log-file",
            str(event_log),
        )
        retry_stderr = retry.stderr
    except BuckException as e:
        retry_stderr = e.stderr
    print_testrun_url("Contract 1 (pre-existing) base-rev retry", retry_stderr)

    assert event_log.exists(), "Event log file was not written"
    # Pre-existing: no phantom 404, and the suite actually ran on base.
    assert not event_log_has_test_not_exists(event_log), (
        "Pre-existing unmanaged failure must NOT be flagged TEST_NOT_EXISTS; "
        f"event log was:\n{event_log.read_text()}"
    )
    results = event_log_results(event_log)
    assert existing_name in results, (
        "Pre-existing unmanaged test should run on the base rerun and produce a "
        f"result. Results: {results}\nlog:\n{event_log.read_text()}"
    )
    # And it must be recorded as a real failure (it fails on base) — not passed,
    # skipped, or a phantom. Observed: status_detailed 501 (FAILED).
    assert results[existing_name] in (FAILED_STATUS, FATAL_STATUS), (
        "Pre-existing unmanaged result must be FAILED/FATAL on the base rerun, "
        f"got status_detailed={results[existing_name]} "
        f"(501=FAILED, 502=FATAL). log:\n{event_log.read_text()}"
    )


# Same on-demand-failing unmanaged sh_test as BRR_SIM_UNMANAGED_TARGET, but
# carrying the `tpx:skip-filtering-unmanaged-test-suite` label (cogwheel /
# audit_dependencies style). Used to prove the *labeled* case below.
BRR_SIM_UNMANAGED_SKIP_FILTERING_TARGET: str = (
    "fbcode//testinfra/playground/sh:brr_sim_unmanaged_skip_filtering"
)


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_unmanaged_cogwheel_label_preexisting_failure_no_phantom(
    buck: Buck, tmp_path: Path
) -> None:
    """
    CONTRACT — pre-existing failing unmanaged test that carries the
    `tpx:skip-filtering-unmanaged-test-suite` label (the cogwheel / audit case).

    This is the regression guard for removing the BRR-path
    `should_skip_filtering_unmanaged_test_suite` flag (was D107275705). That flag
    used to suppress phantom TEST_NOT_EXISTS rows for labeled unmanaged suites at
    emission time. It is gone; suppression now comes solely from the general
    discovery-time clear keyed on FbTestSuite::is_unmanaged, which is label- and
    test-type-agnostic. So a labeled cogwheel-style suite that fails on base must
    still produce NO phantom TEST_NOT_EXISTS, and its `- unmanaged` result must
    still run and be FAILED (pre-existing).
    """
    mode = get_mode_from_platform()
    fail_env = ["--env", "BRR_SIM_FAIL=1"]
    target = BRR_SIM_UNMANAGED_SKIP_FILTERING_TARGET

    # Step 1: target run — the labeled unmanaged test FAILS on demand. Capture run_id.
    try:
        first_run = await buck.test(target, mode, "--", *fail_env)
        stderr = first_run.stderr
    except BuckException as e:
        stderr = e.stderr
    run_id_match = re.search(r"testinfra/testrun/(\d+)", stderr)
    assert run_id_match, f"Could not extract run_id from test output:\n{stderr}"
    run_id = run_id_match.group(1)
    print_testrun_url("Cogwheel-label contract target run", stderr)

    # Step 2: BRR file naming the suite-level `- unmanaged` entry that failed.
    existing_name = f"{target} - unmanaged"
    brr_file = tmp_path / "brr_input.json"
    brr_file.write_text(
        json.dumps(
            {
                "suite_name": target,
                "fully_qualified_target_name": target,
                "test_names": [existing_name],
                "run_id": run_id,
                "test_type": 5,
                "test_config": {
                    "config": "",
                    "host": "linux",
                    "mode": "@fbcode//mode/dev",
                },
            }
        )
    )

    # Step 3: base-revision retry — still failing on base (BRR_SIM_FAIL=1).
    event_log = tmp_path / "events.ndjson"
    try:
        retry = await buck.test(
            target,
            mode,
            "--",
            *fail_env,
            "--base-rev-retry-with-input-file",
            str(brr_file),
            "--experiment",
            "report-new-test-not-exists-status-for-brr",
            "--event-log-file",
            str(event_log),
        )
        retry_stderr = retry.stderr
    except BuckException as e:
        retry_stderr = e.stderr
    print_testrun_url("Cogwheel-label contract base-rev retry", retry_stderr)

    assert event_log.exists(), "Event log file was not written"
    # The whole point: a LABELED unmanaged suite must NOT yield a phantom 404,
    # with the should_skip_filtering BRR suppression removed.
    assert not event_log_has_test_not_exists(event_log), (
        "Labeled (skip-filtering) unmanaged failure must NOT be flagged "
        f"TEST_NOT_EXISTS; event log was:\n{event_log.read_text()}"
    )
    results = event_log_results(event_log)
    assert existing_name in results, (
        "Labeled unmanaged test should still run on the base rerun and produce a "
        f"result. Results: {results}\nlog:\n{event_log.read_text()}"
    )
    assert results[existing_name] in (FAILED_STATUS, FATAL_STATUS), (
        "Labeled unmanaged result must be FAILED/FATAL on the base rerun, "
        f"got status_detailed={results[existing_name]} "
        f"(501=FAILED, 502=FATAL). log:\n{event_log.read_text()}"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_unmanaged_new_does_not_exist(buck: Buck, tmp_path: Path) -> None:
    """
    CONTRACT — new unmanaged test that does not exist on the base revision.

    The BRR file names a `- unmanaged` entry for a suite that is NOT discovered on
    the retry (a target that doesn't exist on base) — modelling a brand-new
    unmanaged test. Nothing runs for it: it produces no result row.

    (Whether TPX emits TEST_NOT_EXISTS for a *non-discovered* suite — which would
    be the legitimate "does not exist on this revision" signal — is asserted from
    the observed behavior below.)
    """
    mode = get_mode_from_platform()

    # Step 1: a normal run of the real target just to get a valid run_id.
    first_run = await buck.test(BRR_SIM_UNMANAGED_TARGET, mode)
    run_id_match = re.search(r"testinfra/testrun/(\d+)", first_run.stderr)
    assert run_id_match, "Could not extract run_id from test output"
    run_id = run_id_match.group(1)
    print_testrun_url("Contract 2 (new) target run", first_run.stderr)

    # Step 2: BRR file naming a `- unmanaged` entry for a target that does not
    # exist (never discovered on the retry) — i.e. a new unmanaged test.
    nonexistent_name = "fbcode//testinfra/playground/sh:brr_sim_unmanaged_new_does_not_exist - unmanaged"
    brr_file = tmp_path / "brr_input.json"
    brr_file.write_text(
        json.dumps(
            {
                "suite_name": BRR_SIM_UNMANAGED_TARGET,
                "fully_qualified_target_name": BRR_SIM_UNMANAGED_TARGET,
                "test_names": [nonexistent_name],
                "run_id": run_id,
                "test_type": 5,
                "test_config": {
                    "config": "",
                    "host": "linux",
                    "mode": "@fbcode//mode/dev",
                },
            }
        )
    )

    # Step 3: base-revision retry with new-test detection enabled.
    event_log = tmp_path / "events.ndjson"
    try:
        retry = await buck.test(
            BRR_SIM_UNMANAGED_TARGET,
            mode,
            "--",
            "--base-rev-retry-with-input-file",
            str(brr_file),
            "--experiment",
            "report-new-test-not-exists-status-for-brr",
            "--event-log-file",
            str(event_log),
        )
        retry_stderr = retry.stderr
    except BuckException as e:
        retry_stderr = e.stderr
    print_testrun_url("Contract 2 (new) base-rev retry", retry_stderr)

    assert event_log.exists(), "Event log file was not written"
    # Observed: the non-existent `- unmanaged` entry is silently dropped — nothing
    # runs for it and NO phantom TEST_NOT_EXISTS is emitted (the event log holds
    # only the run_id row).
    assert not event_log_has_test_not_exists(event_log), (
        "A non-existent unmanaged entry must not produce a phantom "
        f"TEST_NOT_EXISTS; event log was:\n{event_log.read_text()}"
    )
    result_names = event_log_result_names(event_log)
    assert nonexistent_name not in result_names, (
        "A new unmanaged entry that does not exist on base must not run. "
        f"Results: {result_names}\nlog:\n{event_log.read_text()}"
    )


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
    report = read_brr_report(report_file)
    test_names = cast(list[str], report.get("test_names", []))
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
    retry_report = read_brr_report(retry_report_file)
    retry_test_names = cast(list[str], retry_report.get("test_names", []))
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
    only '- listing' (not '- main'), and feeding that report back via
    --base-rev-retry-with-input-file reproduces the listing failure (the
    retry report also contains '- listing').
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

    # Step 2: Verify the report was written and contains a "- listing" entry.
    # Listing failures only produce "- listing" in the BRR file (not "- main"),
    # since "- listing" is the authoritative signal for listing health.
    assert report_file.exists(), "Failure report was not written"
    report = read_brr_report(report_file)
    test_names = cast(list[str], report.get("test_names", []))
    has_listing = any(name.endswith("- listing") for name in test_names)
    assert has_listing, f"Expected a '- listing' entry in test_names, got: {test_names}"
    has_main = any(name.endswith("- main") for name in test_names)
    assert not has_main, (
        f"Expected no '- main' entry in test_names (listing failures only produce '- listing'), got: {test_names}"
    )

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
    retry_report = read_brr_report(retry_report_file)
    retry_test_names = cast(list[str], retry_report.get("test_names", []))
    retry_has_listing = any(name.endswith("- listing") for name in retry_test_names)
    assert retry_has_listing, (
        f"Expected a '- listing' entry in retry report test_names, got: {retry_test_names}"
    )
    print(retry_report)


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_transient_listing_failure_suppresses_execution(
    buck: Buck, tmp_path: Path
) -> None:
    """
    Transient listing failure with per-target listing-only BRR: listing fails
    in the original run (producing '- listing' in the BRR file), but succeeds
    on the retry. TPX detects the '- listing' entries and suppresses execution
    for that target via ListOnlyDiscovery::for_targets(), so no tests run even
    though listing succeeds.
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"

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

    # Step 2: Verify the report has "- listing".
    assert report_file.exists(), "Failure report was not written"
    report = read_brr_report(report_file)
    test_names = cast(list[str], report.get("test_names", []))
    has_listing = any(name.endswith("- listing") for name in test_names)
    assert has_listing, f"Expected '- listing' in test_names, got: {test_names}"

    # Step 3: Feed the report back WITHOUT TPX_PLAYGROUND_FATAL — listing
    # succeeds this time. TPX detects the '- listing' entries and suppresses
    # execution for the target via ListOnlyDiscovery::for_targets().
    try:
        result = await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--base-rev-retry-with-input-file",
            str(report_file),
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # Step 4: Execution is suppressed for listing-only targets. Listing
    # runs (and succeeds), but no tests execute.
    stderr = remove_ansi_escape_sequences(stderr)
    assert "NO TESTS RAN" in stderr, (
        "BRR retry with listing-only target should suppress execution, "
        "but 'NO TESTS RAN' not found in stderr"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_listing_only_roundtrip(buck: Buck, tmp_path: Path) -> None:
    """
    End-to-end BRR roundtrip with --list-only: a listing failure produces a
    report with '- listing', and feeding that report back via
    --base-rev-retry-with-input-file with --list-only reproduces the listing
    failure without executing any tests.
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"
    retry_report_file = tmp_path / "retry_report.json"

    # Step 1: Run the broken listing target and save the failure report.
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

    # Step 2: Verify the report contains a "- listing" entry.
    assert report_file.exists(), "Failure report was not written"
    report = read_brr_report(report_file)
    test_names = cast(list[str], report.get("test_names", []))
    has_listing = any(name.endswith("- listing") for name in test_names)
    assert has_listing, f"Expected a '- listing' entry in test_names, got: {test_names}"

    # Step 3: Feed the report back with --list-only (simulating Citadel
    # passing --list-only for a listing-only BRR). Listing still fails
    # because TPX_PLAYGROUND_FATAL is set.
    try:
        await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--env",
            "TPX_PLAYGROUND_FATAL=1",
            "--list-only",
            "--base-rev-retry-with-input-file",
            str(report_file),
            "--save-failures-for-retry-in-file",
            str(retry_report_file),
        )
    except BuckException:
        pass

    # Step 4: The retry must reproduce the listing failure with --list-only.
    assert retry_report_file.exists(), "Retry failure report was not written"
    retry_report = read_brr_report(retry_report_file)
    retry_test_names = cast(list[str], retry_report.get("test_names", []))
    retry_has_listing = any(name.endswith("- listing") for name in retry_test_names)
    assert retry_has_listing, (
        f"Expected a '- listing' entry in retry report test_names, got: {retry_test_names}"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_listing_only_transient_failure(buck: Buck, tmp_path: Path) -> None:
    """
    Transient listing failure with --list-only: listing fails in the original
    run, but succeeds on the BRR retry with --list-only. No tests should
    execute (--list-only suppresses execution even when listing succeeds).
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"

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

    # Step 2: Verify the report has "- listing".
    assert report_file.exists(), "Failure report was not written"
    report = read_brr_report(report_file)
    test_names = cast(list[str], report.get("test_names", []))
    has_listing = any(name.endswith("- listing") for name in test_names)
    assert has_listing, f"Expected '- listing' in test_names, got: {test_names}"

    # Step 3: Feed the report back with --list-only WITHOUT
    # TPX_PLAYGROUND_FATAL — listing succeeds this time, but --list-only
    # should suppress test execution.
    try:
        result = await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--list-only",
            "--base-rev-retry-with-input-file",
            str(report_file),
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # Step 4: --list-only must suppress execution even when listing succeeds.
    stderr = remove_ansi_escape_sequences(stderr)
    assert "NO TESTS RAN" in stderr, (
        "BRR retry with --list-only should not execute tests, "
        "but 'NO TESTS RAN' not found in stderr"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_brr_per_target_listing_only(buck: Buck, tmp_path: Path) -> None:
    """
    Per-target listing-only BRR: when the BRR input file contains '- listing'
    entries, TPX automatically suppresses execution for those targets without
    needing --list-only. This tests the ListOnlyDiscovery::for_targets() path.
    """
    mode = get_mode_from_platform()
    report_file = tmp_path / "report.json"

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

    # Step 2: Verify the report has "- listing".
    assert report_file.exists(), "Failure report was not written"
    report = read_brr_report(report_file)
    test_names = cast(list[str], report.get("test_names", []))
    has_listing = any(name.endswith("- listing") for name in test_names)
    assert has_listing, f"Expected '- listing' in test_names, got: {test_names}"

    # Step 3: Feed the report back WITHOUT --list-only and WITHOUT
    # TPX_PLAYGROUND_FATAL — listing succeeds, but TPX should automatically
    # suppress execution because the BRR file has '- listing' entries.
    try:
        result = await buck.test(
            BROKEN_LISTING_TARGET,
            mode,
            "--",
            "--base-rev-retry-with-input-file",
            str(report_file),
        )
        stderr = result.stderr
    except BuckException as e:
        stderr = e.stderr

    # Step 4: TPX must suppress execution for listing-only targets even
    # without --list-only, via ListOnlyDiscovery::for_targets().
    stderr = remove_ansi_escape_sequences(stderr)
    assert "NO TESTS RAN" in stderr, (
        "BRR retry should suppress execution for listing-only targets, "
        "but 'NO TESTS RAN' not found in stderr"
    )


@buck_test(inplace=True, skip_for_os=["darwin", "windows"])
async def test_save_successes_for_retry(buck: Buck, tmp_path: Path) -> None:
    """
    --save-successes-for-retry-in-file writes all passing tests to an NDJSON
    file. Each line contains a single test_name entry. The file format is
    identical to --save-failures-for-retry-in-file so it can be consumed by
    --base-rev-retry-with-input-file.
    """
    mode = get_mode_from_platform()
    successes_file = tmp_path / "successes.ndjson"

    # Run a passing test target with --save-successes-for-retry-in-file.
    await buck.test(
        PYTHON_TEST_TARGET,
        mode,
        "--",
        "--save-successes-for-retry-in-file",
        str(successes_file),
    )

    # Verify the file was created and is non-empty.
    assert successes_file.exists(), "Successes file was not created"
    content = successes_file.read_text()
    lines = [line for line in content.splitlines() if line.strip()]
    assert len(lines) > 0, "Successes file is empty — expected passing tests"

    # Verify each line is valid JSON with exactly one test_name.
    for line in lines:
        entry = json.loads(line)
        test_names = entry.get("test_names", [])
        assert len(test_names) == 1, (
            f"Expected exactly 1 test_name per line, got {len(test_names)}: {test_names}"
        )
        assert entry.get("run_id"), "Missing run_id"
        assert entry.get("suite_name"), "Missing suite_name"
