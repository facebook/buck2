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
async def test_cpp_test_coverage(buck: Buck, tmp_path: Path) -> None:
    coverage_file = tmp_path / "coverage.txt"
    await buck.test(
        "@fbcode//mode/dbgo-cov",
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        "--",
        "--collect-coverage",
        f"--coverage-output={coverage_file}",
    )
    paths = []
    with open(coverage_file) as results:
        for line in results:
            paths.append(json.loads(line)["filepath"])

    assert "fbcode/buck2/tests/targets/rules/cxx/cpp_test_pass.cpp" in paths, str(paths)
    assert "fbcode/common/gtest/LightMain.cpp" in paths, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_path_outside_target(
    buck: Buck,
    tmp_path: Path,
) -> None:
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        folder_filter=["fbcode/folly"],
        file_filter=[],
    )

    expected_paths = [p for p in paths if p.startswith("fbcode/folly")]
    assert len(expected_paths) > 0, str(paths)
    unexpected_paths = [p for p in paths if not p.startswith("fbcode/folly")]
    assert len(unexpected_paths) == 0, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_path_of_target(
    buck: Buck, tmp_path: Path
) -> None:
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        folder_filter=["fbcode/buck2/tests"],
        file_filter=[],
    )

    expected_paths = [p for p in paths if p.startswith("fbcode/buck2/tests")]
    assert len(expected_paths) > 0, str(paths)
    unexpected_paths = [p for p in paths if not p.startswith("fbcode/buck2/tests")]
    assert len(unexpected_paths) == 0, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_path_of_target_with_dev_lg(
    buck: Buck,
    tmp_path: Path,
) -> None:
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        mode="@fbcode//mode/dev-lg",
        folder_filter=["fbcode/buck2/tests"],
        file_filter=[],
    )

    expected_paths = [p for p in paths if p.startswith("fbcode/buck2/tests")]
    assert len(expected_paths) > 0, str(paths)
    unexpected_paths = [p for p in paths if not p.startswith("fbcode/buck2/tests")]
    assert len(unexpected_paths) == 0, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_path_in_link_group_with_dev_lg(
    buck: Buck,
    tmp_path: Path,
) -> None:
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        mode="@fbcode//mode/dev-lg",
        folder_filter=["fbcode/folly"],
        file_filter=[],
    )

    expected_paths = [p for p in paths if p.startswith("fbcode/folly")]
    assert len(expected_paths) > 0, str(paths)
    unexpected_paths = [p for p in paths if not p.startswith("fbcode/folly")]
    assert len(unexpected_paths) == 0, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_file_of_target_with_dev_lg(
    buck: Buck,
    tmp_path: Path,
) -> None:
    source_name = "fbcode/buck2/tests/targets/rules/cxx/cpp_test_pass.cpp"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        mode="@fbcode//mode/dev-lg",
        folder_filter=[],
        file_filter=[source_name],
    )

    expected_paths = [p for p in paths if p == source_name]
    assert len(expected_paths) > 0, str(paths)
    unexpected_paths = [p for p in paths if p != source_name]
    assert len(unexpected_paths) == 0, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_source_file_in_link_group_with_dev_lg(
    buck: Buck,
    tmp_path: Path,
) -> None:
    source_name = "fbcode/folly/String.cpp"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        mode="@fbcode//mode/dev-lg",
        folder_filter=[],
        file_filter=[source_name],
    )

    expected_paths = [p for p in paths if p == source_name]
    assert len(expected_paths) > 0, str(paths)
    unexpected_paths = [
        p for p in paths if not p == source_name and not p.endswith(".h")
    ]
    assert len(unexpected_paths) == 0, str(paths)


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_header_file_in_link_group_with_dev_lg(
    buck: Buck,
    tmp_path: Path,
) -> None:
    header_name = "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightRightOnlyUsedHere.h"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground/link_groups:test_with_link_groups",
        mode="@fbcode//mode/dev-lg",
        folder_filter=[],
        file_filter=[header_name],
    )

    assert len(paths) == 5, str(paths)
    # because it belongs to a target that has a header file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightRight.cpp"
        in paths
    )
    # because it belongs to a target that has a dependency that contains a header
    # file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRight.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/TestWithLinkGroups.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/TestWithLinkGroups.h"
        in paths
    )
    # because it has executable code used by LibraryRight.cpp
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeftUsedInOtherLinkGroup.h"
        in paths
    )
    # because they are not in the transitive rdeps of the cxx_library that has the header
    # that was selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeft.cpp"
        not in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryLeft.cpp"
        not in paths
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_header_file_defined_in_one_link_group_and_used_in_another(
    buck: Buck,
    tmp_path: Path,
) -> None:
    header_name = "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeftUsedInOtherLinkGroup.h"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground/link_groups:test_with_header_used_in_different_link_group",
        mode="@fbcode//mode/dev-lg",
        folder_filter=[],
        file_filter=[header_name],
    )

    assert len(paths) == 5, str(paths)
    # because it belongs to a target that has a header file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeft.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeftUsedInOtherLinkGroup.h"
        in paths
    )
    # because it belongs to a target that has a dependency that contains a header
    # file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRight.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/TestWithLinkGroups.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/TestWithLinkGroups.h"
        in paths
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_header_file_defined_in_one_link_group_and_used_in_test_binary_link_group(
    buck: Buck,
    tmp_path: Path,
) -> None:
    header_name = "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeftUsedInOtherLinkGroup.h"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground/link_groups:test_with_header_used_in_different_link_group",
        mode="@fbcode//mode/dev-lg",
        folder_filter=[],
        file_filter=[header_name],
    )

    assert len(paths) == 5, str(paths)
    # because it belongs to a target that has a header file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeft.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRightLeftUsedInOtherLinkGroup.h"
        in paths
    )
    # because it belongs to a target that has a dependency that contains a header
    # file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/LibraryRight.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/TestWithLinkGroups.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/link_groups/TestWithLinkGroups.h"
        in paths
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_file(buck: Buck, tmp_path: Path) -> None:
    source_name = "fbcode/buck2/tests/targets/rules/cxx/cpp_test_pass.cpp"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        folder_filter=[],
        file_filter=[source_name],
    )

    assert len(paths) == 1, str(paths)
    assert paths[0] == source_name


@buck_test(inplace=True)
async def test_cpp_test_coverage_when_filter_by_test_binary_header_file(
    buck: Buck, tmp_path: Path
) -> None:
    header_name = "fbcode/testing_frameworks/code_coverage/playground/Test.h"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground:test",
        folder_filter=[],
        file_filter=[header_name],
    )

    assert len(paths) == 2, str(paths)
    assert header_name in paths
    source_name = "fbcode/testing_frameworks/code_coverage/playground/Test.cpp"
    assert source_name in paths


@buck_test(inplace=True)
async def test_cpp_test_coverage_when_filter_by_library_header_file(
    buck: Buck, tmp_path: Path
) -> None:
    header_name = "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDep.h"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground:test",
        folder_filter=[],
        file_filter=[header_name],
    )

    assert len(paths) == 9, str(paths)
    # because it belongs to a target that has a header file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDep.cpp" in paths
    )
    # because it's header file selected for coverage and it has executable code
    assert "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDep.h" in paths
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDepPrivate.h"
        in paths
    )
    # because it belongs to a target that has a dependency that contains a header
    # file selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/SecondLevelDepIncludesThirdLevelDep.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/SecondLevelDepDoesNotIncludeThirdLevelDep.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/FirstLevelDep.cpp" in paths
    )
    assert "fbcode/testing_frameworks/code_coverage/playground/Test.cpp" in paths
    # because a cpp file (FirstLvelDep.cpp) that included it called a template function
    # defined inside it
    assert "fbcode/testing_frameworks/code_coverage/playground/FirstLevelDep.h" in paths
    assert "fbcode/testing_frameworks/code_coverage/playground/Test.h" in paths

    # because they are not in the transitive rdeps of the cxx_library that has the header
    # that was selected for coverage
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/SecondLevelDepWithoutDeps.cpp"
        not in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/SecondLevelDepWithoutDeps.h"
        not in paths
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_when_filter_by_library_private_header_file(
    buck: Buck, tmp_path: Path
) -> None:
    private_header_name = (
        "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDepPrivate.h"
    )
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground:test",
        folder_filter=[],
        file_filter=[private_header_name],
    )

    assert len(paths) == 3, str(paths)
    assert private_header_name in paths
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDep.cpp" in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/ThirdLevelDepPrivate.h"
        in paths
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_when_filter_by_header_file_in_headers_only_library(
    buck: Buck, tmp_path: Path
) -> None:
    header_name = (
        "fbcode/testing_frameworks/code_coverage/playground/LibraryWithOnlyHeaders.h"
    )
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//testing_frameworks/code_coverage/playground:test_with_dep_with_only_headers",
        folder_filter=[],
        file_filter=[header_name],
    )

    assert len(paths) == 3, str(paths)
    assert header_name in paths
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/TestWithDepWithOnlyHeaders.cpp"
        in paths
    )
    assert (
        "fbcode/testing_frameworks/code_coverage/playground/TestWithDepWithOnlyHeaders.h"
        in paths
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_file_with_opt_mode(
    buck: Buck,
    tmp_path: Path,
) -> None:
    source_name = "fbcode/buck2/tests/targets/rules/cxx/cpp_test_pass.cpp"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        target="fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        folder_filter=[],
        file_filter=[source_name],
        mode="@fbcode//mode/opt",
    )

    assert len(paths) == 1, str(paths)
    assert paths[0] == source_name


@buck_test(inplace=True)
async def test_cpp_test_coverage_filter_by_file_and_path(
    buck: Buck, tmp_path: Path
) -> None:
    source_name = "fbcode/buck2/tests/targets/rules/cxx/cpp_test_pass.cpp"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        folder_filter=["folly"],
        file_filter=[source_name],
    )

    folly_paths = [p for p in paths if p.startswith("fbcode/folly")]
    assert len(folly_paths) > 0, f"expected to find some folly sources in {paths}"

    source_paths = [p for p in paths if p == source_name]
    assert len(source_paths) == 1, f"expected to find {source_name} in {paths}"

    unexpected_paths = [
        p for p in paths if p != source_name and not p.startswith("fbcode/folly")
    ]
    assert len(unexpected_paths) == 0, (
        f"Only coverage for the test source file and files under fbcode/folly should have been collected, but got {unexpected_paths}"
    )


@buck_test(inplace=True)
async def test_cpp_test_coverage_xplat_filter_by_file_path(
    buck: Buck,
    tmp_path: Path,
) -> None:
    file_to_collect_coverage = "xplat/testinfra/playground/cpp/ExampleTest.cpp"
    paths = await collect_coverage_for(
        buck,
        tmp_path,
        "fbsource//xplat/testinfra/playground/cpp:example_testFbcode",
        folder_filter=[],
        file_filter=[file_to_collect_coverage],
    )

    assert set(paths) == {
        file_to_collect_coverage,
    }, (
        f"Should collect coverage only for {file_to_collect_coverage} but got coverage for {paths}"
    )
