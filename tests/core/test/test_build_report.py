# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
from pathlib import Path
from typing import Any, List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import (
    golden,
    sanitize_build_report,
    sanitize_build_report_error,
)
from buck2.tests.e2e_util.helper.utils import read_streaming_report


def build_report_test(name: str, command: List[str], should_fail: bool) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        if should_fail:
            await expect_failure(
                buck.test(
                    "--build-report",
                    str(report),
                    "--build-report-options",
                    "fill-out-failures",
                    *command,
                )
            )
        else:
            await buck.test("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())

        sanitize_build_report(report)

        golden(
            output=sanitize_build_report_error(
                json.dumps(report, indent=2, sort_keys=True)
            ),
            rel_path="fixtures/" + name + ".golden.json",
        )
        pass

    globals()[name] = impl

    return buck_test()(impl)


build_report_test(
    "test_build_report_format",
    ["//:ok", "//:fail_test"],
    True,
)

build_report_test(
    "test_build_report_skip_unconfigured",
    ["//:ok", "-c", "build_report.print_unconfigured_section=false"],
    False,
)

build_report_test(
    "test_failed_build_has_build_report",
    ["//:fail_build1"],
    True,
)

build_report_test(
    "test_target_doesnt_exist",
    ["//:doesnt_exist"],
    True,
)

build_report_test(
    "test_multiple_failures_included",
    ["//:fail_build1", "//:fail_build2"],
    True,
)

build_report_test(
    "test_build_report_with_build_default_info",
    ["//...", "--build-default-info"],
    True,
)

build_report_test(
    "test_build_report_with_build_run_info",
    ["//...", "--build-run-info"],
    True,
)


def _configured_statuses(reports: list[dict[str, Any]]) -> dict[tuple[str, str], str]:
    """Each configured entry's status across report lines; FAIL wins.

    A `buck test` streaming line carries one provider label's build, so a
    later line for the same entry can omit an earlier failure.
    """
    statuses: dict[tuple[str, str], str] = {}
    for report in reports:
        for key, entry in report["results"].items():
            for cfg, configured in entry["configured"].items():
                if statuses.get((key, cfg)) != "FAIL":
                    statuses[(key, cfg)] = configured["success"]
    return statuses


@buck_test()
async def test_streaming_build_report_target_completion(
    buck: Buck, tmp_path: Path
) -> None:
    """`//:ok` has no outputs, so only its build-finished lines report it."""
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await buck.test("//:ok", "--streaming-build-report", str(streaming_report))

    reports = read_streaming_report(streaming_report)
    # The configured target finishing, then the target.
    assert len(reports) == 2, f"Expected 2 lines, got {len(reports)}"
    assert all(set(report["results"]) == {"root//:ok"} for report in reports)


@buck_test()
async def test_streaming_build_report_target_completion_with_failures(
    buck: Buck, tmp_path: Path
) -> None:
    report = tmp_path / "build-report.json"
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await expect_failure(
        buck.test(
            "--build-report",
            str(report),
            "--streaming-build-report",
            str(streaming_report),
            "//:ok",
            "//:fail_test",
            "//:fail_build1",
        )
    )

    streamed = _configured_statuses(read_streaming_report(streaming_report))
    # `//:fail_build1` fails in analysis, before `buck test` builds it, so no
    # line reports it.
    assert {key for key, _cfg in streamed} == {"root//:ok", "root//:fail_test"}
    final = _configured_statuses([json.loads(report.read_text())])
    assert streamed == {
        (key, cfg): status
        for (key, cfg), status in final.items()
        if key != "root//:fail_build1"
    }
