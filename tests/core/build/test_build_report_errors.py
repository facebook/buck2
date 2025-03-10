# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import re
import sys
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


def _sanitize(s: str) -> str:
    # Remote message hashes
    s = re.sub(r"\b[0-9]{16,}\b", "<STRING_HASH>", s)
    # Remove configuration hashes
    # This is so bad... we don't force these hashes to print as 16
    # characters... and that's hard to fix because we don't allow changes to
    # change action digests.
    s = re.sub(r"\b[0-9a-f]{12,16}\b", "<HASH>", s)
    # And action digests
    return re.sub(r"\b[0-9a-f]{40}:[0-9]{1,3}\b", "<DIGEST>", s)


def _sanitize_stderr(s: str) -> str:
    # Remove all timestamps
    s = re.sub(r"\[.{29}\]", "[<TIMESTAMP>]", s)
    # Remove all UUIDs
    s = re.sub(
        r"\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\b", "<UUID>", s
    )
    # Remove "Commands" line
    s = re.sub(r"Commands: .+", "Commands: <COMMAND_STATS>", s)
    # Remove "Cache hits" percentage
    s = re.sub(r"Cache hits: .+", "Cache hits: <CACHE_STATS>", s)
    # Remove "Network" line
    s = re.sub(r"Network: .+", "Network: <NETWORK_STATS>", s)
    return _sanitize(s)


def build_report_test(name: str, command: List[str]) -> None:
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
        del report["trace_id"]
        del report["project_root"]

        # string cache keys can vary due to differences in platform hashes within the message,
        # so do something dumb here to still be able to use golden tests on all platforms:
        #
        # 1. sort by sanitized values
        # 2. create a new dict where the keys are 1 + a large number so that we can
        #    sanitize it using the message regex above
        strings = dict(
            sorted(
                report["strings"].items(),
                key=lambda item: _sanitize(item[1]),
            )
        )
        updated_strings = {}
        start = 10000000000000000
        for i, v in enumerate(strings.values()):
            updated_strings[i + start] = v

        report["strings"] = updated_strings

        golden(
            output=_sanitize(json.dumps(report, indent=2, sort_keys=True)),
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


# TODO(@wendyy) - windows adds some extra characters to stdout/stderr.
# Python reports compile errors with the full path on mac as well, which
# breaks golden tests.
# Fix for both os types later.
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

    @buck_test()
    async def test_stderr_with_empty_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(
            buck.build("//fail_action:fail_one_with_error_handler_no_op")
        )

        golden(
            output=_sanitize_stderr(result.stderr),
            rel_path="fixtures/test_stderr_with_empty_error_diagnostics.golden.txt",
        )

    @buck_test()
    async def test_stderr_with_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(
            buck.build("//fail_action:error_handler_produced_multiple_categories")
        )

        golden(
            output=_sanitize_stderr(result.stderr),
            rel_path="fixtures/test_stderr_with_error_diagnostics.golden.txt",
        )

    @buck_test()
    async def test_stderr_with_no_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(buck.build("//fail_action:fail_script"))

        golden(
            output=_sanitize_stderr(result.stderr),
            rel_path="fixtures/test_stderr_with_no_error_diagnostics.golden.txt",
        )

    @buck_test()
    async def test_stderr_could_not_produce_error_diagnostics(buck: Buck) -> None:
        result = await expect_failure(buck.build("//fail_action:error_handler_failed"))

        golden(
            output=_sanitize_stderr(result.stderr),
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
