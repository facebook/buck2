# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import difflib
import re
import shutil
import tempfile
from pathlib import Path
from typing import Pattern

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

BXL_LABEL = "fbsource//xplat/buck2/intellij_project/main.bxl:generate_intellij_project"
EXPECTED_DIR_RELATIVE_PATH = "../xplat/buck2/tests/intellij_project/testdata"

# To replace the entire buck-out path for generated files
BUCK_OUT_HASH_PATTERN: Pattern[str] = re.compile(r"buck-out/[^\"!]*")
BUCK_OUT_HASH_REPLACE_TXT = "buck-out-path"


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_android_apk(buck: Buck) -> None:
    expected_dir_name = "sample_intellij_project"
    test_targets = "fbsource//xplat/buck2/tests/intellij_project/testdata/sample_intellij_project/apk:apk"
    await run_and_verify_project(buck, expected_dir_name, test_targets)


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_android_library(buck: Buck) -> None:
    expected_dir_name = "android_library"
    test_targets = (
        "fbsource//xplat/buck2/tests/intellij_project/testdata/android_library/..."
    )
    await run_and_verify_project(buck, expected_dir_name, test_targets)


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_android_binary(buck: Buck) -> None:
    expected_dir_name = "android_binary"
    test_targets = (
        "fbsource//xplat/buck2/tests/intellij_project/testdata/android_binary/..."
    )
    await run_and_verify_project(buck, expected_dir_name, test_targets)


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_project1(buck: Buck) -> None:
    expected_dir_name = "project1"
    test_targets = "fbsource//xplat/buck2/tests/intellij_project/testdata/project1/..."
    await run_and_verify_project(buck, expected_dir_name, test_targets)


async def run_and_verify_project(
    buck: Buck, expected_dir_name: str, test_targets: str
) -> None:
    expected_dir_path = Path(EXPECTED_DIR_RELATIVE_PATH).resolve() / expected_dir_name
    buck_config_path = expected_dir_path / "configfile"
    idea_dir_path = expected_dir_path / ".idea"

    result = await buck.bxl(
        BXL_LABEL,
        "--config-file",
        str(buck_config_path),
        "--",
        "--targets",
        test_targets,
    )
    output_dir = Path(result.stdout.strip())

    xml_output_temp_dir = tempfile.mkdtemp()
    config_args_map = {
        "intellij.default_min_android_sdk_version": 19,
        "intellij.default_android_manifest_package_name": "com.facebook",
        "intellij.android_generated_files_directory": ".idea/android_gen",
    }
    config_args = []
    for name, value in config_args_map.items():
        config_args.append("-c")
        config_args.append(f"{name}={value}")
    root = await buck.root()
    await buck.run(
        "fbsource//xplat/buck2/intellij_project/tools/project_writer:project_writer",
        "--",
        "--input_path",
        output_dir.as_posix(),
        "--output_path",
        xml_output_temp_dir,
        "--repo_path",
        root.stdout.strip(),
        "--manifests-to-copy-json",
        (output_dir.parent / "manifests_to_copy").as_posix(),
        *config_args,
        rel_cwd=Path(".."),
    )
    verify_expected_files(idea_dir_path, expected_dir_name, Path(xml_output_temp_dir))
    shutil.rmtree(xml_output_temp_dir)


def verify_expected_files(
    expected_dir_path: Path, expected_dir_name: str, output_dir: Path
) -> None:
    for cur_file in expected_dir_path.rglob("*.expected"):
        relative_path = cur_file.relative_to(expected_dir_path).parent
        file_name = cur_file.stem

        generated_file = output_dir / relative_path / file_name
        assert generated_file.is_file(), "File does not exist: {}".format(
            generated_file
        )

        cur_lines = [line.strip() for line in open(cur_file).readlines()]
        generated_lines = [line.strip() for line in open(generated_file).readlines()]

        generated_lines = [
            BUCK_OUT_HASH_PATTERN.sub(BUCK_OUT_HASH_REPLACE_TXT, line)
            for line in generated_lines
        ]

        is_match = cur_lines == generated_lines
        diff = difflib.context_diff(cur_lines, generated_lines, n=2)
        assert (
            is_match
        ), "Generated file: {} does not match expected file at: {}\n{}".format(
            generated_file, cur_file, "".join(diff)
        )


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
