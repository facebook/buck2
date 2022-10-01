import os
import platform
from typing import Iterable

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

    assert_all_paths_do_not_exist(build_report_outputs)


@buck_test(inplace=True)
async def test_clean_dry_run(buck: Buck) -> None:
    build_result = await buck.build(TARGET, "--show-output")
    build_report = build_result.get_build_report()
    build_report_outputs = [
        str(output) for output in build_report.outputs_for_target(TARGET)
    ]

    dry_clean_result = await buck.clean("--dry-run")

    dry_clean_paths = set(
        filter(
            is_buck_path,
            dry_clean_result.stderr.split("\n"),
        )
    )
    assert_all_paths_exist(dry_clean_paths)

    dry_clean_paths = tuple(i for i in dry_clean_paths)
    for output in build_report_outputs:
        assert output.startswith(dry_clean_paths)

    assert_all_paths_exist(build_report_outputs)

    # Run clean without dry-run and make sure all files are removed now
    clean_result = await buck.clean()
    clean_paths = set(
        filter(
            is_buck_path,
            clean_result.stderr.split("\n"),
        )
    )
    # dry_clean_paths and clean_paths should be the same
    for clean_path in clean_paths:
        assert clean_path in dry_clean_paths
    for dry_clean_path in dry_clean_paths:
        assert dry_clean_path in clean_paths

    assert_all_paths_do_not_exist(clean_paths)


def is_buck_path(x: str):
    if platform.system() == "Windows":
        return "\\.buck\\buckd\\" in x or "\\buck-out\\" in x
    else:
        return "/.buck/buckd/" in x or "/buck-out/" in x


def assert_all_paths_exist(paths: Iterable):
    for path in paths:
        assert os.path.exists(path) is True


def assert_all_paths_do_not_exist(paths: Iterable):
    for path in paths:
        assert os.path.exists(path) is False
