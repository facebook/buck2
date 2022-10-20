import os

import pytest
from py._path.local import LocalPath
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

PROFILERS = [
    "heap-flame-allocated-src",
    "heap-flame-retained-src",
    "heap-flame-allocated",
    "heap-flame-retained",
    "heap-summary-allocated",
    "heap-summary-retained",
    "time-flame-src",
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
async def test_profile_analysis_last(
    buck: Buck, tmpdir: LocalPath, profiler: str
) -> None:
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
async def test_profile_analysis_recursive(
    buck: Buck, tmpdir: LocalPath, profiler: str
) -> None:
    file_path = tmpdir.join("profile")

    command = buck.profile(
        "analysis",
        "--mode",
        profiler,
        "fbcode//buck2/tests/targets/profile:test",
        "--output",
        file_path,
        "--recursive",
    )
    if (
        profiler.endswith("-retained")
        or profiler.endswith("-retained-src")
        or profiler.startswith("bytecode")
        or profiler.endswith("-allocated")
        or profiler.endswith("-allocated-src")
        or profiler == "time-flame"
        or profiler == "time-flame-src"
    ):
        await command

        assert os.path.exists(file_path)
    else:
        await expect_failure(
            command,
            stderr_regex="Merge of profile data for profile mode `.*` is not implemented",
        )


@buck_test(inplace=True)
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_loading_last(
    buck: Buck, tmpdir: LocalPath, profiler: str
) -> None:
    file_path = tmpdir.join("profile")

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "fbcode//buck2/tests/targets/profile:",
        "--output",
        file_path,
    )

    if profiler.endswith("-retained") or profiler.endswith("-retained-src"):
        await expect_failure(
            command,
            stderr_regex="Retained memory profiling is available only for analysis profile",
        )
    else:
        await command

        assert os.path.exists(file_path)


@buck_test(inplace=True)
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_loading_recursive(
    buck: Buck, tmpdir: LocalPath, profiler: str
) -> None:
    file_path = tmpdir.join("profile")

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "fbcode//buck2/tests/targets/profile:",
        "--output",
        file_path,
        "--recursive",
    )

    await expect_failure(
        command,
        stderr_regex="Recursive profiling is not supported for loading profiling",
    )


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


@buck_test(inplace=True)
async def test_profile_no_buckd(
    buck: Buck,
    tmpdir: LocalPath,
) -> None:
    file_path = tmpdir.join("profile")

    command = await buck.profile(
        "loading",
        "--mode",
        "statement",
        "fbcode//buck2/tests/targets/profile:",
        "--output",
        file_path,
        "--no-buckd",
    )

    assert "Total retained bytes:" in command.stdout
    assert os.path.exists(file_path)
