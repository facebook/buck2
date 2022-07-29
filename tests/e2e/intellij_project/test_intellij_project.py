import json
import re
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_generate_intellij_project(buck: Buck) -> None:
    bxl_label = (
        "fbcode//buck2/prelude/intellij_project/main.bxl:generate_intellij_project"
    )
    result = await buck.bxl(
        bxl_label,
        "--",
        "--targets",
        "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/apk:apk",
    )

    output_dir = Path(result.stdout.strip())
    assert (
        (output_dir / "modules.xml").read_text()
        == """\
<?xml version="1.0" encoding="UTF-8"?>
<project version="4">
  <component name="ProjectModuleManager">
    <modules>
    </modules>
  </component>
</project>
"""
    )

    libraries_dir = output_dir / "libraries"
    assert len(list(libraries_dir.iterdir())) == 1

    with (
        libraries_dir
        / "__fbandroid_buck2_tests_good_sample_intellij_project_prebuilt_jar_prebuilt__.json"
    ).open("r") as json_file:
        assert json.load(json_file) == {
            "name": "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt",
            "type": "DEFAULT",
            "binaryJars": [
                "fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/prebuilt.jar"
            ],
            "javadocUrls": ["http://prebuilt_jar_javadoc.url"],
            "sourceJars": [
                "fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/prebuilt-sources.jar"
            ],
        }

    modules_dir = output_dir / "modules"
    assert len(list(modules_dir.iterdir())) == 2

    with (
        modules_dir
        / "fbandroid_buck2_tests_good_sample_intellij_project_android_main.json"
    ).open("r") as json_file:
        assert _sanitize_buck_out_paths(json.load(json_file)) == {
            "dependencies": {
                "LIBRARY": [
                    {
                        "name": "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt"
                    }
                ],
                "MODULE": [
                    {
                        "name": "fbandroid_buck2_tests_good_sample_intellij_project_java_single_lib"
                    }
                ],
                "MODULE-LIBRARY": [
                    {
                        "library": {
                            "binaryJars": [
                                "buck-out-path",
                            ],
                            "classPaths": [
                                "buck-out-path",
                            ],
                            "name": "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/android_prebuilt_aar:aar",
                            "sourceJars": [],
                            "type": "DEFAULT",
                        }
                    },
                    {
                        "library": {
                            "binaryJars": [
                                "fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar_no_sources/prebuilt_no_sources.jar"
                            ],
                            "name": "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar_no_sources:prebuilt_no_sources",
                            "sourceJars": [],
                            "type": "DEFAULT",
                        }
                    },
                ],
            }
        }

    with (
        modules_dir
        / "fbandroid_buck2_tests_good_sample_intellij_project_java_single_lib.json"
    ).open("r") as json_file:
        assert json.load(json_file) == {
            "dependencies": {
                "LIBRARY": [
                    {
                        "name": "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt"
                    }
                ],
                "MODULE": [],
                "MODULE-LIBRARY": [
                    {
                        "library": {
                            "binaryJars": [
                                "fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/another_prebuilt.jar"
                            ],
                            "name": "fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt_in_same_package",
                            "sourceJars": [],
                            "type": "DEFAULT",
                        }
                    }
                ],
            }
        }


@buck_test(inplace=True)
async def test_generate_sample_project(buck: Buck) -> None:
    bxl_label = "fbcode//buck2/prelude/intellij_project/sample.bxl:project_gen"
    await buck.bxl(
        bxl_label,
        "--",
        "--roots",
        "fbcode//buck2/gazebo/gazebo:gazebo",
        "--mode",
        "foo",
    )


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
