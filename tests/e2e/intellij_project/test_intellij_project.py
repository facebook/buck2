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
    )

    output_file = result.stdout.strip()
    assert (
        Path(output_file).read_text()
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
