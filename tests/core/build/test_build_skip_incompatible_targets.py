#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import (
    read_streaming_report,
    streaming_status_history,
)


@buck_test()
async def test_build_skip_incompatible(buck: Buck) -> None:
    targetA = "root//:compatible-with-A"
    targetB = "root//:compatible-with-B"
    platformA = "root//:platA"

    await expect_failure(
        buck.build(
            targetA,
            targetB,
            f"--target-platforms={platformA}",
        ),
        stderr_regex=rf"{targetB}\s*is incompatible with {platformA}",
    )

    result = await buck.build(
        targetA,
        targetB,
        f"--target-platforms={platformA}",
        "--skip-incompatible-targets",
    )
    assert "Skipped 1 incompatible target" in result.stderr
    assert targetB in result.stderr

    report = result.get_build_report()
    assert len(report.results) == 2
    assert len(report.results[targetA]["configured"]) == 1
    assert len(report.results[targetB]["configured"]) == 0


@buck_test()
async def test_streaming_build_report_skip_incompatible(
    buck: Buck, tmp_path: Path
) -> None:
    """A target whose only configuration is skipped still finishes, with no configurations."""
    targetA = "root//:compatible-with-A"
    targetB = "root//:compatible-with-B"
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await buck.build(
        targetA,
        targetB,
        "--target-platforms=root//:platA",
        "--skip-incompatible-targets",
        "--streaming-build-report",
        str(streaming_report),
    )

    reports = read_streaming_report(streaming_report)
    assert streaming_status_history(reports) == {
        targetA: ["IN_PROGRESS", "SUCCESS"],
        targetB: ["IN_PROGRESS", "SUCCESS"],
    }
    assert all(
        report["results"][targetB]["configured"] == {}
        for report in reports
        if targetB in report["results"]
    )
