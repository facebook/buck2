# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import asyncio
import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import (
    read_streaming_report,
    replay_streaming_report,
)


@buck_test()
async def test_overall_timeout(buck: Buck, tmp_path: Path) -> None:
    build_report = tmp_path / "build-report.json"

    await expect_failure(
        buck.build(
            ":slow",
            "--overall-timeout",
            "1s",
            "--build-report",
            str(build_report),
        ),
        stderr_regex="Build timed out",
        exit_code=ExitCodeV2.USER_ERROR,
    )

    with open(build_report) as f:
        build_report_json = json.load(f)
        assert build_report_json["success"] is False

        # One result.
        (result,) = build_report_json["results"].values()
        (configured,) = result["configured"].values()
        assert configured["errors"][0]["error_tags"] == ["BUILD_DEADLINE_EXPIRED"]


@buck_test()
async def test_overall_timeout_streaming_build_report(
    buck: Buck, tmp_path: Path
) -> None:
    """A timed-out output adds no line, so only the build-finished lines report it."""
    streaming_report = tmp_path / "streaming-build-report.jsonl"

    await expect_failure(
        buck.build(
            ":slow",
            "--overall-timeout",
            "1s",
            "--streaming-build-report",
            str(streaming_report),
        ),
        stderr_regex="Build timed out",
        exit_code=ExitCodeV2.USER_ERROR,
    )

    reports = read_streaming_report(streaming_report)
    # The configured target finishing, then the target.
    assert len(reports) == 2, f"Expected 2 lines, got {len(reports)}"
    (result,) = replay_streaming_report(reports).values()
    (configured,) = result["configured"].values()
    assert configured["errors"][0]["error_tags"] == ["BUILD_DEADLINE_EXPIRED"]


@buck_test()
async def test_overall_timeout_with_artifact_path_sketch(
    buck: Buck, tmp_path: Path
) -> None:
    # Artifact path sketching runs *after* the build and re-`ensure_artifact_group`s every
    # top-level output. When the deadline fires mid-build, that post-build re-ensure must not
    # rebuild the action the timeout just cancelled: otherwise the command runs unbounded past
    # its `--overall-timeout`. The `:slow` action sleeps for far longer than the deadline, so a
    # regression would re-run it post-deadline and hang. Bound the wall clock so that failure is
    # fast and legible instead of a multi-minute hang.
    build_report = tmp_path / "build-report.json"

    try:
        await asyncio.wait_for(
            expect_failure(
                buck.build(
                    ":slow",
                    "--overall-timeout",
                    "1s",
                    "-c",
                    "buck2.log_artifact_size_sketch=true",
                    "--build-report",
                    str(build_report),
                ),
                stderr_regex="Build timed out",
                exit_code=ExitCodeV2.USER_ERROR,
            ),
            timeout=60,
        )
    except asyncio.TimeoutError:
        raise AssertionError(
            "`--overall-timeout` did not stop the build within 60s with artifact path "
            "sketching enabled: a post-build phase is re-running an action the deadline "
            "already cancelled."
        )

    with open(build_report) as f:
        build_report_json = json.load(f)
        assert build_report_json["success"] is False

        (result,) = build_report_json["results"].values()
        (configured,) = result["configured"].values()
        assert configured["errors"][0]["error_tags"] == ["BUILD_DEADLINE_EXPIRED"]
