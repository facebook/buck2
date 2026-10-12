# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import sys
from pathlib import Path
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import (
    golden,
    sanitize_build_report,
    sanitize_hashes,
    sanitize_python,
    sanitize_stderr,
    strip_waiting_on,
)
from buck2.tests.e2e_util.helper.utils import (
    read_streaming_report,
    replay_streaming_report,
)


def build_report_test(name: str, command: list[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        await expect_failure(
            buck.build(
                "--build-report",
                str(report),
                "--build-report-options",
                "fill-out-failures",
                *command,
            )
        )
        with open(report) as f:
            report = json.loads(f.read())

        sanitize_build_report(report)

        golden(
            output=sanitize_hashes(
                sanitize_python(json.dumps(report, indent=2, sort_keys=True), buck.cwd)
            ),
            rel_path="fixtures/" + name + ".golden.json",
        )
        pass

    globals()[name] = impl

    return buck_test()(impl)


build_report_test(
    "test_action_fail_one",
    ["//fail_action:fail_one"],
)

build_report_test(
    "test_action_fail_two",
    ["//fail_action:fail_two"],
)

build_report_test(
    "test_action_fail_shared_dep",
    ["//fail_action:fail_shared_dep"],
)

build_report_test(
    "test_action_fail_shared_across_targets",
    ["//fail_action:alias_a", "//fail_action:alias_b"],
)


def running_on_windows() -> bool:
    return sys.platform == "win32"


def running_on_mac() -> bool:
    return sys.platform == "darwin"


# TODO fix on windows and mac
if not running_on_windows() and not running_on_mac():
    build_report_test(
        "test_action_fail_with_stdout_stderr",
        ["//fail_action:fail_script"],
    )

    build_report_test(
        "test_action_fail_infra",
        ["//fail_action:fail_infra"],
    )

    build_report_test(
        "test_action_fail_one_with_error_handler",
        ["//fail_action:fail_one_with_error_handler"],
    )

    build_report_test(
        "test_action_fail_many_with_error_handler",
        ["//fail_action:fail_many_with_error_handler"],
    )

    build_report_test(
        "test_action_fail_one_with_error_handler_no_op",
        ["//fail_action:fail_one_with_error_handler_no_op"],
    )

    def sanitize_error_stderr(stderr: str, buck: Buck) -> str:
        return strip_waiting_on(sanitize_stderr(sanitize_python(stderr, buck.cwd)))

    @buck_test()
    async def test_stderr_with_empty_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(
            buck.build("//fail_action:fail_one_with_error_handler_no_op")
        )

        golden(
            output=sanitize_error_stderr(result.stderr, buck),
            rel_path="fixtures/test_stderr_with_empty_error_diagnostics.golden.txt",
        )

    @buck_test()
    async def test_stderr_with_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(
            buck.build("//fail_action:error_handler_produced_multiple_categories")
        )

        golden(
            output=sanitize_error_stderr(result.stderr, buck),
            rel_path="fixtures/test_stderr_with_error_diagnostics.golden.txt",
        )

    @buck_test()
    async def test_stderr_with_no_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(buck.build("//fail_action:fail_script"))

        golden(
            output=sanitize_error_stderr(result.stderr, buck),
            rel_path="fixtures/test_stderr_with_no_error_diagnostics.golden.txt",
        )

    @buck_test()
    async def test_stderr_could_not_produce_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(buck.build("//fail_action:error_handler_failed"))

        golden(
            output=sanitize_error_stderr(result.stderr, buck),
            rel_path="fixtures/test_stderr_could_not_produce_error_diagnostics.golden.txt",
        )

    build_report_test(
        "test_action_fail_error_handler_with_output_remote_only",
        ["//fail_action:fail_error_handler_with_output", "--remote-only"],
    )

    build_report_test(
        "test_action_fail_error_handler_with_output_local_only",
        ["//fail_action:fail_error_handler_with_output", "--local-only"],
    )

    build_report_test(
        "test_action_fail_error_handler_with_output_content_based_path_remote_only",
        [
            "//fail_action:fail_error_handler_with_output",
            "--remote-only",
            "-c",
            "test.use_content_based_path=true",
        ],
    )

    build_report_test(
        "test_action_fail_error_handler_with_output_content_based_path_local_only",
        [
            "//fail_action:fail_error_handler_with_output",
            "--local-only",
            "-c",
            "test.use_content_based_path=true",
        ],
    )

    build_report_test(
        "test_action_fail_error_handler_output_not_written_remote_only",
        ["//fail_action:fail_error_handler_output_not_written", "--remote-only"],
    )

    build_report_test(
        "test_action_fail_error_handler_output_not_written_local_only",
        ["//fail_action:fail_error_handler_output_not_written", "--local-only"],
    )


build_report_test(
    "test_analysis_fail",
    ["//fail_analysis:fail_analysis"],
)

build_report_test(
    "test_analysis_fail_multi",
    ["//fail_analysis:fail_analysis", "//fail_analysis:alias"],
)

build_report_test(
    "test_analysis_and_action",
    ["//fail_analysis:fail_analysis", "//fail_action:fail_one"],
)

build_report_test(
    "test_configuration_fail",
    ["//fail_config:cat_only"],
)

build_report_test(
    "test_missing",
    ["//missing:missing"],
)

build_report_test(
    "test_load_fail",
    ["//fail_load:first", "//fail_load:second"],
)

# This does not show up in the build report, because there's nowhere we could put the error
build_report_test(
    "test_load_fail_full_package",
    ["//fail_load:"],
)

build_report_test(
    "test_partially_missing",
    ["//missing:missing", "//missing:available"],
)

ONE_OF_EACH = [
    "//success:success",
    "//missing:missing",
    "//fail_load:first",
    "//fail_config:cat_only",
    "//fail_analysis:fail_analysis",
    "//fail_action:fail_one",
]

build_report_test("test_one_of_each", ONE_OF_EACH)


def _outcomes(results: dict[str, Any]) -> dict[str, Any]:
    """Each entry's status and error count, and those of each of its configurations."""
    return {
        key: (
            entry["success"],
            len(entry["errors"]),
            {
                cfg: (configured["success"], len(configured["errors"]))
                for cfg, configured in entry["configured"].items()
            },
        )
        for key, entry in results.items()
    }


def _statuses(results: dict[str, Any]) -> dict[tuple[str, str | None], str]:
    """Each entry's status, keyed `(key, None)`, and each configuration's, `(key, cfg)`."""
    return {
        **{(key, None): entry["success"] for key, entry in results.items()},
        **{
            (key, cfg): configured["success"]
            for key, entry in results.items()
            for cfg, configured in entry["configured"].items()
        },
    }


@buck_test()
async def test_streaming_one_of_each(buck: Buck, tmp_path: Path) -> None:
    report = tmp_path / "build-report.json"
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await expect_failure(
        buck.build(
            "--build-report",
            str(report),
            "--streaming-build-report",
            str(streaming_report),
            *ONE_OF_EACH,
        )
    )

    reports = read_streaming_report(streaming_report)
    # One line for the output of `//fail_action:fail_one`, one each when the 3 targets that
    # configure finish their configured build, and one each when the 6 targets finish.
    assert len(reports) == 10, f"Expected 10 lines, got {len(reports)}"
    for line in reports:
        assert len(line["results"]) == 1, (
            f"Expected one target per line, got {sorted(line['results'])}"
        )
    final = json.loads(report.read_text())["results"]
    assert _outcomes(replay_streaming_report(reports)) == _outcomes(final)

    failed = {key for key, entry in final.items() if entry["success"] == "FAIL"}
    assert len(failed) == len(ONE_OF_EACH) - 1, (
        f"Expected every target but `//success:success` to fail, got {sorted(failed)}"
    )
    streamed_failures = {
        key
        for line in reports
        for key, entry in line["results"].items()
        if entry["success"] == "FAIL"
    }
    assert streamed_failures == failed

    # `//success:success` has no outputs. Its configured target always finishes before the
    # target does, so one line reads the configuration SUCCESS and the target IN_PROGRESS.
    success_history = [
        (entry["success"], [c["success"] for c in entry["configured"].values()])
        for report in reports
        if (entry := report["results"].get("root//success:success"))
    ]
    assert ("IN_PROGRESS", ["SUCCESS"]) in success_history, success_history
    assert success_history[-1] == ("SUCCESS", ["SUCCESS"]), success_history

    failed_statuses = {
        key for key, status in _statuses(final).items() if status == "FAIL"
    }
    read_success_before_failing = {
        key
        for report in reports
        for key, status in _statuses(report["results"]).items()
        if key in failed_statuses and status == "SUCCESS"
    }
    assert read_success_before_failing == set()


@buck_test()
async def test_streaming_target_without_outputs(buck: Buck, tmp_path: Path) -> None:
    """`//success:success` has no outputs, so only its build-finished lines report it."""
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await buck.build(
        "//success:success", "--streaming-build-report", str(streaming_report)
    )

    reports = read_streaming_report(streaming_report)
    # The configured target finishing, then the target.
    assert len(reports) == 2, f"Expected 2 lines, got {len(reports)}"
    results = replay_streaming_report(reports)
    assert results["root//success:success"]["success"] == "SUCCESS"


build_report_test(
    "test_no_terminal_colors",
    [
        "//terminal_colors:terminal_colors",
    ],
)


@buck_test(setup_eden=True)
async def test_two_action_dep_failures(buck: Buck, tmp_path: Path) -> None:
    # When we pass `--keep-going`, we should get error reports for both dependencies of the action.
    # However, we don't. Instead, we just get one error non-deterministically. This is also why we
    # can't use a `build_report_test` for this test.
    report = tmp_path / "build-report.json"
    await expect_failure(
        buck.build(
            "--keep-going", "--build-report", str(report), "//fail_action:fail_two_deps"
        ),
        stderr_regex="Failed to build 'root//fail_action:fail_two_deps",
    )
    with open(report) as f:
        report = json.loads(f.read())
    errors = list(
        report["results"]["root//fail_action:fail_two_deps"]["configured"].values()
    )[0]["errors"]
    assert len(errors) == 1

    strings_cache = report["strings"]
    error_message_index = str(errors[0]["message_content"])

    assert strings_cache[error_message_index].startswith(
        "Action failed: root//fail_action:fail_two_deps"
    )
    assert "fail_two_deps" in errors[0]["action_error"]["key"]["owner"]


@buck_test()
async def test_error_handler_failed(buck: Buck, tmp_path: Path) -> None:
    # Starlark error messages change across different modes for some reason (ex: opt-asan vs opt).
    # We have a fair amount of coverage for other functionalities of error handler/build report,
    # so let's just add a simple test here.
    report = tmp_path / "build-report.json"

    await expect_failure(
        buck.build(
            "--build-report",
            str(report),
            "//fail_action:error_handler_failed",
        ),
        stderr_regex="Failed to build 'root//fail_action:error_handler_failed",
    )

    with open(report) as f:
        report = f.read()

    assert "Error handler failed" in report
    assert "fail: something went wrong" in report


@buck_test()
async def test_error_handler_wrong_return_type(buck: Buck, tmp_path: Path) -> None:
    # Starlark error messages change across different modes for some reason (ex: opt-asan vs opt).
    # We have a fair amount of coverage for other functionalities of error handler/build report,
    # so let's just add a simple test here.
    report = tmp_path / "build-report.json"

    await expect_failure(
        buck.build(
            "--build-report",
            str(report),
            "//fail_action:error_handler_wrong_return_type",
        ),
        stderr_regex="Failed to build 'root//fail_action:error_handler_wrong_return_type",
    )

    with open(report) as f:
        report = f.read()

    assert "Error handler failed" in report
    assert (
        "Expected return type `list[ActionSubError]`, got value with type `int`"
        in report
    )


@buck_test()
async def test_missing_report_on_wrong_package(buck: Buck, tmp_path: Path) -> None:
    # If we specify a non-existent package, we don't get an error report
    report = tmp_path / "build-report.json"
    await expect_failure(
        buck.build("--build-report", str(report), "//nopackage/..."),
        stderr_regex="Error resolving recursive target pattern",
    )
    if report.exists():
        raise AssertionError("Expected no report to be written")


@buck_test()
async def test_streaming_fail_fast_streams_the_stopping_error(
    buck: Buck, tmp_path: Path
) -> None:
    # The analysis error stops the build before any output exists, so no output line can carry it.
    # `//slow:slow` keeps the build running: if the build ended on its own, `--fail-fast` would not
    # stop it.
    streaming_report = tmp_path / "streaming-build-report.jsonl"
    await expect_failure(
        buck.build(
            "--fail-fast",
            "--local-only",
            "--no-remote-cache",
            "--streaming-build-report",
            str(streaming_report),
            "//fail_analysis:fail_analysis",
            "//slow:slow",
        )
    )

    lines = streaming_report.read_text().splitlines()
    assert lines, "the error that stopped the build must reach the streaming report"
    report = json.loads(lines[-1])
    assert list(report["results"]) == ["root//fail_analysis:fail_analysis"]
    configured = report["results"]["root//fail_analysis:fail_analysis"]["configured"][
        "<unspecified>"
    ]
    assert configured["success"] == "FAIL"
    (error,) = configured["errors"]
    assert "Failure during analysis" in report["strings"][str(error["message_content"])]


# TODO fix on windows and mac
if not running_on_windows() and not running_on_mac():

    @buck_test(write_invocation_record=True)
    async def test_exclude_action_error_diagnostics(buck: Buck, tmp_path: Path) -> None:
        # Test that --build-report-options=exclude-action-error-diagnostics removes
        # error_diagnostics from the build report.
        report = tmp_path / "build-report.json"
        res = await expect_failure(
            buck.build(
                "--build-report",
                str(report),
                "--build-report-options",
                "fill-out-failures,exclude-action-error-diagnostics",
                "//fail_action:fail_one_with_error_handler",
            )
        )
        with open(report) as f:
            report_data = json.loads(f.read())

        errors = report_data["results"][
            "root//fail_action:fail_one_with_error_handler"
        ]["configured"]["<unspecified>"]["errors"]
        assert len(errors) == 1
        # category_key must match what buck2 logs to its error telemetry.
        telemetry_error = res.invocation_record().single_error()
        assert errors[0]["category_key"] == telemetry_error["category_key"], (
            errors[0],
            telemetry_error,
        )

        sanitize_build_report(report_data)

        golden(
            output=sanitize_hashes(
                sanitize_python(
                    json.dumps(report_data, indent=2, sort_keys=True), buck.cwd
                )
            ),
            rel_path="fixtures/test_exclude_action_error_diagnostics.golden.json",
        )

    @buck_test()
    async def test_truncate_error_content(buck: Buck, tmp_path: Path) -> None:
        # Test that --build-report-options=truncate-error-content truncates
        # error message content in the build report when errors exceed 20KB.
        # Uses a target that produces a 25KB+ error message.
        report = tmp_path / "build-report.json"
        await expect_failure(
            buck.build(
                "--build-report",
                str(report),
                "--build-report-options",
                "fill-out-failures,truncate-error-content",
                "//fail_action:fail_large_error",
            )
        )
        with open(report) as f:
            report_data = json.loads(f.read())

        sanitize_build_report(report_data)

        golden(
            output=sanitize_hashes(
                sanitize_python(
                    json.dumps(report_data, indent=2, sort_keys=True), buck.cwd
                )
            ),
            rel_path="fixtures/test_truncate_error_content.golden.json",
        )
