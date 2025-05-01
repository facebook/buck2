# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import tempfile
from dataclasses import dataclass, field
from typing import List, Optional

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_python_coverage(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as covfile:
        await buck.test(
            "@fbcode//mode/dbgo-cov",
            "fbcode//buck2/tests/targets/rules/python/coverage:test",
            "--",
            "--collect-coverage",
            f"--coverage-output={covfile.name}",
        )
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])
    assert "fbcode/buck2/tests/targets/rules/python/coverage/lib.py" in paths, str(
        paths
    )


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_folder(buck: Buck) -> None:
    folder_to_collect = "buck2/tests/targets/rules/python/coverage"
    with tempfile.NamedTemporaryFile("w") as covfile:
        await buck.test(
            "@fbcode//mode/dbgo-cov",
            "fbcode//buck2/tests/targets/rules/python/coverage:test",
            "-c",
            f"fbcode.cxx_coverage_only={folder_to_collect}",
            "--",
            "--collect-coverage",
            f"--coverage-output={covfile.name}",
        )
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])
    assert (
        set(paths)
        == {
            f"fbcode/{folder_to_collect}/lib.py",
            f"fbcode/{folder_to_collect}/test.py",
        }
    ), f"Only folder fbcode/{folder_to_collect} should have coverage, instead got coverage for {str(paths)}"


@dataclass
class PythonCoverageResult:
    using_new_testpilot_interface_paths: List[str] = field(default_factory=list)


async def python_collect_coverage_for(
    buck: Buck,
    filter: List[str],
    target: str = "fbcode//buck2/tests/targets/rules/python/coverage:test",
    mode: Optional[str] = None,
) -> PythonCoverageResult:
    filter_str = " ".join(filter)
    result = PythonCoverageResult()
    with tempfile.NamedTemporaryFile("w") as covfile:
        buck_args = [mode] if mode else []
        buck_args.extend(
            [
                "--config",
                "fbcode.coverage_selective=true",
                "--config",
                f"fbcode.cxx_coverage_only={filter_str}",
                target,
                "fbcode//buck2/tests/targets/rules/python/coverage:test",
                "--",
                "--collect-coverage",
                f"--coverage-output={covfile.name}",
            ]
        )
        await buck.test(*buck_args)
        paths = []
        with open(covfile.name) as results:
            for line in results:
                paths.append(json.loads(line)["filepath"])

        result.using_new_testpilot_interface_paths = paths

    return result


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_file(buck: Buck) -> None:
    file_to_collect_coverage = "buck2/tests/targets/rules/python/coverage/lib.py"
    result = await python_collect_coverage_for(buck, [file_to_collect_coverage])

    fbcode_filename = f"fbcode/{file_to_collect_coverage}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_file_with_base_module_remap(
    buck: Buck,
) -> None:
    file_to_collect_coverage = "buck2/tests/targets/rules/python/coverage/lib.py"
    result = await python_collect_coverage_for(
        buck,
        target="fbcode//buck2/tests/targets/rules/python/coverage:test_using_base_module_remapped_lib",
        filter=[file_to_collect_coverage],
    )

    fbcode_filename = f"fbcode/{file_to_collect_coverage}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_file_with_opt_mode(buck: Buck) -> None:
    file_to_collect_coverage = "buck2/tests/targets/rules/python/coverage/lib.py"
    result = await python_collect_coverage_for(
        buck,
        filter=[file_to_collect_coverage],
        mode="@fbcode//mode/opt",
    )

    fbcode_filename = f"fbcode/{file_to_collect_coverage}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_file_on_cinder_target(buck: Buck) -> None:
    file_to_collect_coverage = "buck2/tests/targets/rules/python/coverage/test.py"
    result = await python_collect_coverage_for(
        buck,
        target="fbcode//buck2/tests/targets/rules/python/coverage:simple_cinder_unittest",
        filter=[file_to_collect_coverage],
    )

    fbcode_filename = f"fbcode/{file_to_collect_coverage}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_source_file_on_cpp_dep(buck: Buck) -> None:
    file_to_collect_coverage = "buck2/tests/targets/rules/python/coverage/cpp_lib.cpp"
    result = await python_collect_coverage_for(
        buck,
        target="fbcode//buck2/tests/targets/rules/python/coverage:test_with_cpp_dep",
        filter=[file_to_collect_coverage],
    )

    fbcode_filename = f"fbcode/{file_to_collect_coverage}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_header_file_on_cpp_dep(buck: Buck) -> None:
    header_file = "buck2/tests/targets/rules/python/coverage/cpp_lib.h"
    source_file = header_file.replace(".h", ".cpp")
    result = await python_collect_coverage_for(
        buck,
        target="fbcode//buck2/tests/targets/rules/python/coverage:test_with_cpp_dep",
        filter=[header_file],
    )

    fbcode_filename = f"fbcode/{source_file}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {source_file} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"


@buck_test(inplace=True)
async def test_python_coverage_filtering_by_file_on_ligen_cpp_dep(buck: Buck) -> None:
    file_to_collect_coverage = (
        "buck2/tests/targets/rules/python/coverage/adder_ligen.cpp"
    )
    result = await python_collect_coverage_for(
        buck,
        target="fbcode//buck2/tests/targets/rules/python/coverage:test_with_ligen_cpp_dep",
        filter=[file_to_collect_coverage],
    )

    fbcode_filename = f"fbcode/{file_to_collect_coverage}"
    assert (
        set(result.using_new_testpilot_interface_paths) == {fbcode_filename}
    ), f"Only {file_to_collect_coverage} should have coverage, instead got coverage for {str(result.using_new_testpilot_interface_paths)}"
