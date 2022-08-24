import difflib
import re
import tempfile
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

BXL_LABEL = "fbsource//xplat/buck2/intellij_project/main.bxl:generate_intellij_project"
EXPECTED_DIR_RELATIVE_PATH = "buck2/tests/e2e/intellij_project/testdata"

# TODO: Find a more general pattern: This regex work only if target is in fbandroid.
# Or replace with just general re.compile(r"\b[0-9a-f]{16}\b") if the path doesn't
# need sanitizing for other segments.
BUCK_OUT_HASH_PATTERN = re.compile(r"buck-out/(.*)/fbandroid")
BUCK_OUT_HASH_REPLACE_TXT = "buck-out/buck-out-hash/fbandroid"


@buck_test(inplace=True)
async def test_android_apk(buck: Buck) -> None:
    expected_dir_name = "apk"
    test_target = "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/apk:apk"
    await run_and_verify_project(buck, expected_dir_name, test_target)


async def run_and_verify_project(
    buck: Buck, expected_dir_name: str, test_target: str
) -> None:
    expected_dir_path = (
        buck.cwd / EXPECTED_DIR_RELATIVE_PATH / expected_dir_name / ".idea"
    )
    result = await buck.bxl(
        BXL_LABEL,
        "--",
        "--targets",
        test_target,
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
        verify_expected_files(expected_dir_path, Path(xml_output_temp_dir))


def verify_expected_files(expected_dir: Path, output_dir: Path) -> None:
    for cur_file in expected_dir.rglob("*.expected"):
        relative_path = cur_file.relative_to(expected_dir).parent
        generated_file = output_dir / relative_path / cur_file.stem
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
