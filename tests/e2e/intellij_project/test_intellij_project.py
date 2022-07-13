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

    assert (
        (
            output_dir
            / "libraries/__fbandroid_buck2_tests_good_sample_intellij_project_prebuilt_jar_prebuilt__.xml"
        ).read_text()
        == """\
<component name="libraryTable">
  <library name="fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar:prebuilt">
    <CLASSES>
      <root url="jar://$PROJECT_DIR$/fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/prebuilt.jar!/" />
    </CLASSES>
    <SOURCES>
      <root url="jar://$PROJECT_DIR$/fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar/prebuilt-sources.jar!/" />
    </SOURCES>
    <JAVADOC />
  </library>
</component>
"""
    )

    assert (
        (
            output_dir
            / "libraries/__fbandroid_buck2_tests_good_sample_intellij_project_prebuilt_jar_no_sources_prebuilt_no_sources__.xml"
        ).read_text()
        == """\
<component name="libraryTable">
  <library name="fbsource//fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar_no_sources:prebuilt_no_sources">
    <CLASSES>
      <root url="jar://$PROJECT_DIR$/fbandroid/buck2/tests/good/sample_intellij_project/prebuilt_jar_no_sources/prebuilt_no_sources.jar!/" />
    </CLASSES>
    <JAVADOC />
  </library>
</component>
"""
    )

    assert (
        (
            output_dir
            / "modules/fbandroid_buck2_tests_good_sample_intellij_project_android_main.iml"
        ).read_text()
        == """\
<?xml version="1.0" encoding="UTF-8"?>
<module type="JAVA_MODULE" version="4">
  <component name="NewModuleRootManager" inherit-compiler-output="false">
  </component>
</project>
"""
    )

    assert (
        (
            output_dir
            / "modules/fbandroid_buck2_tests_good_sample_intellij_project_java_single_lib.iml"
        ).read_text()
        == """\
<?xml version="1.0" encoding="UTF-8"?>
<module type="JAVA_MODULE" version="4">
  <component name="NewModuleRootManager" inherit-compiler-output="false">
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
