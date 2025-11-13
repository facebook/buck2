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

from .test_coverage_utils import collect_coverage_for


@buck_test(inplace=True)
async def test_go_test_selective_coverage_doesnt_produce_coverage(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            *[
                "--config",
                "code_coverage.enable=filtered",
                "--config",
                "code_coverage.file_path_filter=fbcode/testing_frameworks/code_coverage/go/add.go",
                "fbcode//testing_frameworks/code_coverage/go:test",
                "--",
                "--collect-coverage",
            ]
        ),
        stderr_regex=r"""2 TESTS FATALS
  ⚠ fbcode//testing_frameworks/code_coverage/go:test - Test(Add|Sub)
  ⚠ fbcode//testing_frameworks/code_coverage/go:test - Test(Add|Sub)""",
    )


@buck_test(inplace=True)
async def test_junit_test_selective_coverage_doesnt_produce_coverage(
    buck: Buck, tmp_path: Path
) -> None:
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/junit/com/facebook/testing_frameworks:test",
        folder_filter=[],
        file_filter=[
            "testing_frameworks/code_coverage/junit/com/facebook/testing_frameworks/AddTest.java"
        ],
    )

    assert len(paths) == 0, str(paths)
