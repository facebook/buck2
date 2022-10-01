# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import difflib
import re
import tempfile
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

BXL_LABEL = "fbsource//xplat/buck2/intellij_project/main.bxl:generate_intellij_project"
EXPECTED_DIR_RELATIVE_PATH = "../xplat/buck2/tests/intellij_project/testdata"

# To replace the entire buck-out path for generated files
BUCK_OUT_HASH_PATTERN = re.compile(r"buck-out/[^\"]*")
BUCK_OUT_HASH_REPLACE_TXT = "buck-out-path"


def incompatible_with_v2(test_func):
    """Decorator to disable the tests until project generation logic is implemented."""
    pass


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_if_windows=True)
async def test_android_apk(buck: Buck) -> None:
    expected_dir_name = "sample_intellij_project"
    test_targets = "fbsource//xplat/buck2/tests/intellij_project/testdata/sample_intellij_project/apk:apk"
    await run_and_verify_project(buck, expected_dir_name, test_targets)


@incompatible_with_v2
@buck_test(inplace=True)
async def test_android_library(buck: Buck) -> None:
    expected_dir_name = "android_library"
    test_targets = (
        "fbsource//xplat/buck2/tests/intellij_project/testdata/android_library/..."
    )
    await run_and_verify_project(buck, expected_dir_name, test_targets)


@incompatible_with_v2
@buck_test(inplace=True)
async def test_android_binary(buck: Buck) -> None:
    expected_dir_name = "android_binary"
    test_targets = (
        "fbsource//xplat/buck2/tests/intellij_project/testdata/android_binary/..."
    )
    await run_and_verify_project(buck, expected_dir_name, test_targets)


@incompatible_with_v2
@buck_test(inplace=True)
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

    with tempfile.TemporaryDirectory() as xml_output_temp_dir:
        await buck.run(
            "fbsource//xplat/buck2/intellij_project/tools/project_writer:project_writer",
            "--",
            "--input_path",
            output_dir.as_posix(),
            "--output_path",
            xml_output_temp_dir,
        )
        verify_expected_files(
            idea_dir_path, expected_dir_name, Path(xml_output_temp_dir)
        )


def verify_expected_files(
    expected_dir_path: Path, expected_dir_name: str, output_dir: Path
) -> None:
    for cur_file in expected_dir_path.rglob("*.expected"):
        relative_path = cur_file.relative_to(expected_dir_path).parent
        file_name = cur_file.stem

        generated_file = output_dir / relative_path / file_name
        assert generated_file.is_file(), "File does not exist:{}".format(generated_file)

        sanitized_gen_file_content = BUCK_OUT_HASH_PATTERN.sub(
            BUCK_OUT_HASH_REPLACE_TXT, generated_file.read_text()
        )

        is_match = cur_file.read_text() == sanitized_gen_file_content
        # TODO: remove diff report for buckout paths
        diff = difflib.context_diff(
            open(cur_file).readlines(), open(generated_file).readlines(), n=0
        )
        assert (
            is_match
        ), "Generated file:{} does not match expected file at: {}\n{}".format(
            generated_file, cur_file, "".join(diff)
        )


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
