# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from pathlib import Path
from typing import Optional

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

from .test_coverage_utils import collect_coverage_for

JAVA_TEST_TARGET = "fbsource//xplat/test_frameworks/coverage/java/playground:SimpleTest"

EXTRA_BUCK_ARGS = [
    "--config",
    "junit_selective_coverage_rollout.is_enabled=true",
    # currently selective coverage is enabled via this flag
]


@pytest.mark.parametrize("mode", [None, "@fbcode//mode/dev", "@fbcode//mode/opt"])
@buck_test(inplace=True)
async def test_java_coverage_file_filter(
    buck: Buck, tmp_path: Path, mode: Optional[str]
) -> None:
    file_to_collect_coverage = "xplat/test_frameworks/coverage/java/playground/java_test/src/test/com/facebook/playground/SimpleTest.java"
    result = await collect_coverage_for(
        buck,
        tmp_path,
        target=JAVA_TEST_TARGET,
        mode=mode,
        folder_filter=[],
        file_filter=[file_to_collect_coverage],
        extra_buck_args=EXTRA_BUCK_ARGS,
    )

    assert set(result) == {file_to_collect_coverage}, (
        f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result)}"
    )


@pytest.mark.parametrize("mode", [None, "@fbcode//mode/dev", "@fbcode//mode/opt"])
@buck_test(inplace=True)
async def test_java_coverage_folder_filter(
    buck: Buck, tmp_path: Path, mode: Optional[str]
) -> None:
    folder_to_collect_coverage = "xplat/test_frameworks/coverage/java/playground/java_test/src/test/com/facebook/playground/nested"
    result = await collect_coverage_for(
        buck,
        tmp_path,
        target=JAVA_TEST_TARGET,
        mode=mode,
        folder_filter=[folder_to_collect_coverage],
        file_filter=[],
        extra_buck_args=EXTRA_BUCK_ARGS,
    )

    expected_files = {
        "xplat/test_frameworks/coverage/java/playground/java_test/src/test/com/facebook/playground/nested/Adder.java"
    }
    assert set(result) == expected_files, (
        f"Only {expected_files} should have coverage, instead got coverage for {str(result)}"
    )


@buck_test(inplace=True)
async def test_junit_test_selective_coverage_doesnt_produce_coverage(
    buck: Buck, tmp_path: Path
) -> None:
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/junit/com/facebook/testing_frameworks:test",
        file_filter=[
            "testing_frameworks/code_coverage/junit/com/facebook/testing_frameworks/AddTest.java"
        ],
        folder_filter=[],
    )

    assert len(paths) == 0, str(paths)
