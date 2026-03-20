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

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

from .test_coverage_utils import collect_coverage_for


@buck_test(inplace=True)
async def test_go_test_dbgo_cov(buck: Buck, tmp_path: Path) -> None:
    coverage_file = tmp_path / "coverage.txt"
    await buck.test(
        "@fbcode//mode/dbgo-cov",
        "fbcode//test_frameworks/gotest/playground:simple_add_test",
        "--",
        f"--coverage-output={coverage_file}",
    )
    paths = []
    with open(coverage_file) as results:
        for line in results:
            paths.append(json.loads(line)["filepath"])

    assert "fbcode/test_frameworks/gotest/playground/simple.go" in paths, str(paths)
    assert "fbcode/test_frameworks/gotest/playground/simple_add_test.go" in paths, str(
        paths
    )


@buck_test(inplace=True)
async def test_go_test_filtered_coverage_by_folder(buck: Buck, tmp_path: Path) -> None:
    """Test that filtered coverage produces coverage data when folder_path_filter matches."""
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//test_frameworks/gotest/playground:simple_add_test",
        folder_filter=["fbcode/test_frameworks/gotest/playground"],
        file_filter=[],
    )

    assert "fbcode/test_frameworks/gotest/playground/simple.go" in paths, str(paths)
    assert "fbcode/test_frameworks/gotest/playground/simple_add_test.go" in paths, str(
        paths
    )


@buck_test(inplace=True)
async def test_go_test_filtered_coverage_by_file(buck: Buck, tmp_path: Path) -> None:
    """Test that filtered coverage produces coverage data when file_path_filter matches."""
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//test_frameworks/gotest/playground:simple_add_test",
        folder_filter=[],
        file_filter=["fbcode/test_frameworks/gotest/playground/simple.go"],
    )

    assert "fbcode/test_frameworks/gotest/playground/simple.go" in paths, str(paths)


@buck_test(inplace=True)
async def test_go_test_selective_coverage_by_file(buck: Buck, tmp_path: Path) -> None:
    """Test selective coverage with file_path_filter on a different test target."""
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/go:math_test",
        folder_filter=[],
        file_filter=[
            "fbcode/testing_frameworks/code_coverage/go/add.go",
        ],
    )

    assert "fbcode/testing_frameworks/code_coverage/go/add.go" in paths, str(paths)


@buck_test(inplace=True)
async def test_go_test_filtered_coverage_not_matching(
    buck: Buck, tmp_path: Path
) -> None:
    """Test that filtered coverage produces no coverage data when folder_path_filter doesn't match."""
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//test_frameworks/gotest/playground:simple_add_test",
        folder_filter=["some/other/path"],
        file_filter=[],
    )

    assert len(paths) == 0, f"Expected no coverage data but got: {paths}"


@buck_test(inplace=True)
async def test_go_test_dbgo_cov_on_remote_execution(buck: Buck, tmp_path: Path) -> None:
    coverage_file = tmp_path / "coverage.txt"
    await buck.test(
        "@fbcode//mode/dbgo-cov",
        "fbcode//test_frameworks/gotest/playground:simple_add_test_re",
        "--",
        f"--coverage-output={coverage_file}",
    )
    paths = []
    with open(coverage_file) as results:
        for line in results:
            paths.append(json.loads(line)["filepath"])

    assert "fbcode/test_frameworks/gotest/playground/simple.go" in paths, str(paths)
    assert "fbcode/test_frameworks/gotest/playground/simple_add_test.go" in paths, str(
        paths
    )
