import json
import re
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

bxl_label = "fbcode//buck2/prelude/intellij_project/main.bxl:generate_intellij_project"
expected_dir_relative_path = "buck2/tests/e2e/intellij_project/testdata"


@buck_test(inplace=True)
async def test_generate_intellij_project(buck: Buck) -> None:
    expected_dir = buck.cwd / expected_dir_relative_path / "apk" / ".idea"
    result = await buck.bxl(
        bxl_label,
        "--",
        "--targets",
        "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/apk:apk",
    )
    output_dir = Path(result.stdout.strip())
    assert verify_expected_files(expected_dir, output_dir)


def _sanitize_buck_out_paths(data):
    if type(data) == dict:
        return {key: _sanitize_buck_out_paths(value) for key, value in data.items()}
    elif type(data) == list:
        return [_sanitize_buck_out_paths(x) for x in data]
    elif type(data) == str:
        return re.sub(r"buck-out/[^\"]*", "buck-out-path", data)
    else:
        raise Exception(
            "Unknown data type for _sanitize_buck_out_paths: {}".format(type(data))
        )


def verify_expected_files(expected_dir: Path, output_dir: Path) -> bool:
    for cur_file in expected_dir.rglob("*.expected"):
        relative_path = cur_file.relative_to(expected_dir).parent
        generated_file = output_dir / relative_path / cur_file.stem

        if generated_file.suffix == ".json":
            with cur_file.open("r") as cur_json_file, generated_file.open(
                "r"
            ) as gen_json_file:
                expected = json.load(cur_json_file)
                actual = _sanitize_buck_out_paths(json.load(gen_json_file))
                if actual != expected:
                    return False
        else:
            if cur_file.read_text() != generated_file.read_text():
                return False
    return True
