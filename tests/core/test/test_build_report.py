# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import re
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


def _sanitize(s: str) -> str:
    # Simplify analysis error message (Can change due to line number changes)
    s = re.sub(
        r"Error running analysis for.*\"", 'Error running analysis for <IRRELEVANT>"', s
    )
    # Simplify the Unknown target error (Can change due to number of targets in TARGETS.fixture)
    s = re.sub(
        r"Unknown target `.*` from package .*\"",
        'Unknown target `<TARGET>` from package <IRRELEVANT>"',
        s,
    )
    return s


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
        del report["trace_id"]
        del report["project_root"]

        # Build report errors can change based on minor test changes such as
        # 1. Adding a target in TARGETS.fixture
        # 2. Line number changing due to code moving around
        # Sanitize so that we only check the important bits of the error message
        golden(
            output=_sanitize(json.dumps(report, indent=2, sort_keys=True)),
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
