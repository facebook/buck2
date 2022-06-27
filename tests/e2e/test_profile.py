import os

import pytest
from py._path.local import LocalPath
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

PROFILERS = [
    "heap-flame",
    "heap-summary",
    "time-flame",
    "statement",
    "bytecode",
    "bytecode-pairs",
]


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_analysis(buck: Buck, tmpdir: LocalPath, profiler: str) -> None:
    file_path = tmpdir.join("profile")

    await buck.profile(
        "analysis",
        "--mode",
        profiler,
        "fbcode//buck2/tests/targets/rules/sh_test:test",
        file_path,
    )

    assert os.path.exists(file_path)


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_loading(buck: Buck, tmpdir: LocalPath, profiler: str) -> None:
    file_path = tmpdir.join("profile")

    await buck.profile(
        "loading",
        "--mode",
        profiler,
        "fbcode//buck2/tests/targets/rules/sh_test:",
        file_path,
    )

    assert os.path.exists(file_path)


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
async def test_profile_shows_errors(buck: Buck) -> None:
    expect_failure(
        buck.profile(
            "loading",
            "--mode",
            "statement",
            "invalid",
            "/dev/null",
        ),
        stderr_regex="use a package",
    )
