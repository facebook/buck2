# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden
from buck2.tests.e2e_util.helper.utils import replace_digest, replace_hash


def build_report_test(name: str, command: List[str]) -> None:
    async def impl(buck: Buck, tmp_path: Path) -> None:
        report = tmp_path / "build-report.json"
        await buck.build("--build-report", str(report), *command)

        with open(report) as file:
            report = json.loads(file.read())
        del report["trace_id"]
        del report["project_root"]

        # Build report errors can change based on minor test changes such as
        # 1. Adding a target in TARGETS.fixture
        # 2. Line number changing due to code moving around
        # Sanitize so that we only check the important bits of the error message
        golden(
            output=replace_digest(
                replace_hash(json.dumps(report, indent=2, sort_keys=True))
            ),
            rel_path="fixtures/" + name + ".golden.json",
        )

    globals()[name] = impl

    return buck_test()(impl)


build_report_test(
    "test_build_report_format",
    [
        "//:rule1",
        "//:rule2",
        "//:rule2[out1]",
        "-c",
        "buck2.log_configured_graph_size=true",
    ],
)

build_report_test(
    "test_build_report_format_skip_unconfigured",
    [
        "//:rule1",
        "-c",
        "build_report.print_unconfigured_section=false",
    ],
)

build_report_test(
    "test_build_report_format_package_relative_paths",
    [
        "//:rule1",
        "//subdir:rule",
        "--build-report-options",
        "package-project-relative-paths",
    ],
)


build_report_test(
    "test_build_report_format_artifact_hash_information",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "--build-report-options",
        "include-artifact-hash-information",
    ],
)

build_report_test(
    "test_build_report_format_configured_graph_sketch",
    [
        "//:rule1",
        "//:dir1",
        "//subdir:rule",
        "-c",
        "buck2.log_configured_graph_sketch=true",
    ],
)


@buck_test()
async def test_build_report_non_existent_directory(buck: Buck) -> None:
    build_report = "non_existent_dir/report"

    await buck.build(
        "//:rule1",
        "--build-report",
        build_report,
    )

    with open(buck.cwd / build_report) as file:
        report = json.load(file)
        assert report["success"]
