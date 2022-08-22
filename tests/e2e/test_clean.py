import os

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

TARGET = "fbcode//buck2/tests/targets/rules/genrule:executable_helper"


@buck_test(inplace=True)
async def test_clean(buck: Buck) -> None:
    build_result = await buck.build(TARGET)
    build_report = build_result.get_build_report()
    build_report_outputs = [
        str(output) for output in build_report.outputs_for_target(TARGET)
    ]

    clean_result = await buck.clean()
    clean_paths = tuple(filter(None, clean_result.stderr.split("\n")))

    for output in build_report_outputs:
        assert output.startswith(clean_paths)

    for output in build_report_outputs:
        assert os.path.exists(output) is False


@buck_test(inplace=True)
async def test_clean_dry_run(buck: Buck) -> None:
    build_result = await buck.build(TARGET, "--show-output")
    build_report = build_result.get_build_report()
    build_report_outputs = [
        str(output) for output in build_report.outputs_for_target(TARGET)
    ]

    clean_result = await buck.clean("--dry-run")
    clean_paths = tuple(filter(None, clean_result.stderr.split("\n")))

    for output in build_report_outputs:
        assert output.startswith(clean_paths)

    for output in build_report_outputs:
        assert os.path.exists(output) is True
