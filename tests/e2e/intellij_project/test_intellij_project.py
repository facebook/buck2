import json
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
            "sourceJars": [
                "fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/prebuilt-sources.jar"
            ],
        }

    modules_dir = output_dir / "modules"
    assert len(list(modules_dir.iterdir())) == 2

    assert (
        (
            modules_dir
            / "fbandroid_buck2_tests_good_sample_intellij_project_android_main.iml"
        ).read_text()
        == """\
<?xml version="1.0" encoding="UTF-8"?>
<module type="JAVA_MODULE" version="4">
  <component name="NewModuleRootManager" inherit-compiler-output="false">
    <orderEntry type="module" module-name="fbandroid_buck2_tests_good_sample_intellij_project_java_single_lib" scope="COMPILE" />
    <orderEntry type="module-library" scope="COMPILE">
      <library name="fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar_no_sources:prebuilt_no_sources">
        <CLASSES>
          <root url="jar://$PROJECT_DIR$/fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar_no_sources/prebuilt_no_sources.jar!/" />
        </CLASSES>
        <JAVADOC />
      </library>
    </orderEntry>
    <orderEntry type="library" name="fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt" scope="COMPILE" level="project" />
  </component>
</project>
"""
    )

    assert (
        (
            modules_dir
            / "fbandroid_buck2_tests_good_sample_intellij_project_java_single_lib.iml"
        ).read_text()
        == """\
<?xml version="1.0" encoding="UTF-8"?>
<module type="JAVA_MODULE" version="4">
  <component name="NewModuleRootManager" inherit-compiler-output="false">
    <orderEntry type="module-library" scope="COMPILE">
      <library name="fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt_in_same_package">
        <CLASSES>
          <root url="jar://$PROJECT_DIR$/fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/another_prebuilt.jar!/" />
        </CLASSES>
        <JAVADOC />
      </library>
    </orderEntry>
    <orderEntry type="library" name="fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt" scope="COMPILE" level="project" />
  </component>
</project>
"""
    )


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
