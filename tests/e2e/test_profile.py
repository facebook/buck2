import os

import pytest
from py._path.local import LocalPath
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

PROFILERS = [
    "heap-flame",
    "heap-flame-retained",
    "heap-summary",
    "heap-summary-retained",
    "time-flame",
    "statement",
    "bytecode",
    "bytecode-pairs",
]


@buck_test(inplace=True)
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
        "fbcode//buck2/tests/targets/profile:test",
        "--output",
        file_path,
    )

    assert os.path.exists(file_path)


@buck_test(inplace=True)
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_loading(buck: Buck, tmpdir: LocalPath, profiler: str) -> None:
    file_path = tmpdir.join("profile")

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "fbcode//buck2/tests/targets/profile:",
        "--output",
        file_path,
    )

    if profiler.endswith("-retained"):
        await expect_failure(
            command,
            stderr_regex="Retained memory profiling is available only for analysis profile",
        )
    else:
        await command

        assert os.path.exists(file_path)


@buck_test(inplace=True)
async def test_profile_shows_errors(buck: Buck) -> None:
    await expect_failure(
        buck.profile(
            "loading",
            "--mode",
            "statement",
            "invalid",
            "--output",
            "/dev/null",
        ),
        stderr_regex="use a package",
    )
