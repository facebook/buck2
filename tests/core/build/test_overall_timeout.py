# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import json
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


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
