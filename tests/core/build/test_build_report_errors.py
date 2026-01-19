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

build_report_test(
    "test_one_of_each",
    [
        "//success:success",
        "//missing:missing",
        "//fail_load:first",
        "//fail_config:cat_only",
        "//fail_analysis:fail_analysis",
        "//fail_action:fail_one",
    ],
)

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
