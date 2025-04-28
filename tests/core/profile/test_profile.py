# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import os
from pathlib import Path

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, BuckResult
from buck2.tests.e2e_util.api.process import Process
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

PROFILERS = [
    "heap-flame-allocated",
    "heap-flame-retained",
    "heap-summary-allocated",
    "heap-summary-retained",
    "time-flame",
    "statement",
    "bytecode",
    "bytecode-pairs",
    "typecheck",
    "coverage",
]


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_analysis_for_self_transition(
    buck: Buck, tmp_path: Path, profiler: str
) -> None:
    file_path = tmp_path / "profile"

    await buck.profile(
        "analysis",
        "--target-platforms=//self_transition:p",
        "--mode",
        profiler,
        "//self_transition:zzz",
        "--output",
        str(file_path),
    )

    assert os.path.exists(file_path)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_analysis_last(buck: Buck, tmp_path: Path, profiler: str) -> None:
    file_path = tmp_path / "profile"

    await buck.profile(
        "analysis",
        "--mode",
        profiler,
        "//simple:test",
        "--output",
        str(file_path),
    )

    assert os.path.exists(file_path)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_analysis_recursive(
    buck: Buck, tmp_path: Path, profiler: str
) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "analysis",
        "--mode",
        profiler,
        "//simple:test",
        "--output",
        str(file_path),
        "--recursive",
    )
    await assert_flame_outputs(command, file_path, profiler)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_analysis_recursive_transition(
    buck: Buck, tmp_path: Path, profiler: str
) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "analysis",
        "--mode",
        profiler,
        "//recursive_transition:ccc",
        "--output",
        str(file_path),
        "--recursive",
    )

    await assert_flame_outputs(command, file_path, profiler)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_loading_last(buck: Buck, tmp_path: Path, profiler: str) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "//simple:",
        "--output",
        str(file_path),
    )

    await _assertions_for_profile_without_frozen_module(command, file_path, profiler)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_query_profile(buck: Buck, tmp_path: Path, profiler: str) -> None:
    file_path = tmp_path / "profile"

    command = buck.cquery(
        "--profile-mode",
        profiler,
        "deps(//query/a:a)",
        "--profile-output",
        str(file_path),
    )

    await _assertions_for_profile_without_frozen_module(command, file_path, profiler)

    if not profiler.endswith("-retained"):
        with open(buck.cwd / file_path / "targets.txt", "r") as f:
            lines = [x.rstrip() for x in sorted(f.readlines())]
            assert [
                "loading:root//query/a",
                "loading:root//query/b",
            ] == lines
    else:
        assert not os.path.exists(buck.cwd / file_path)


@buck_test()
async def test_profile_loading_last_single_target(buck: Buck, tmp_path: Path) -> None:
    file_path = tmp_path / "profile"

    profiler = "statement"

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "//simple:a",
        "--output",
        str(file_path),
    )

    await _assertions_for_profile_without_frozen_module(command, file_path, profiler)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
@pytest.mark.parametrize(
    "recursive",
    [True, False],
)
async def test_profile_analysis_pattern(
    buck: Buck, tmp_path: Path, profiler: str, recursive: bool
) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "analysis",
        "--mode",
        profiler,
        "//simple/...",  # We test this.
        "--output",
        str(file_path),
        *(["--recursive"] if recursive else []),
    )

    await assert_flame_outputs(command, file_path, profiler)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_loading_recursive(
    buck: Buck, tmp_path: Path, profiler: str
) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "//simple:",
        "--output",
        str(file_path),
        "--recursive",
    )

    await expect_failure(
        command,
        stderr_regex="Recursive profiling is not supported for loading profiling",
    )


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_bxl_with_actions(
    buck: Buck, tmp_path: Path, profiler: str
) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "bxl",
        "--mode",
        profiler,
        "//bxl/profile.bxl:profile_with_actions",
        "--output",
        str(file_path),
    )

    await assert_flame_outputs(command, file_path, profiler)


@buck_test()
@pytest.mark.parametrize(
    "profiler",
    PROFILERS,
)
async def test_profile_bxl_without_actions(
    buck: Buck, tmp_path: Path, profiler: str
) -> None:
    file_path = tmp_path / "profile"

    command = buck.profile(
        "bxl",
        "--mode",
        profiler,
        "//bxl/profile.bxl:profile_without_actions",
        "--output",
        str(file_path),
    )

    await assert_flame_outputs(command, file_path, profiler)


@buck_test(setup_eden=True)
async def test_profile_no_buckd(
    buck: Buck,
    tmp_path: Path,
) -> None:
    file_path = tmp_path / "profile"

    command = await buck.profile(
        "loading",
        "--mode",
        "statement",
        "//simple:",
        "--output",
        str(file_path),
        "--no-buckd",
    )

    assert "Total retained bytes:" in command.stdout
    assert os.path.exists(file_path)


@buck_test()
async def test_profile_loading_recursive_target_pattern(
    buck: Buck, tmp_path: Path
) -> None:
    file_path = tmp_path / "profile"
    profiler = "time-flame"

    command = buck.profile(
        "loading",
        "--mode",
        profiler,
        "//simple/...",
        "--output",
        str(file_path),
    )

    await _assertions_for_profile_without_frozen_module(command, file_path, profiler)


async def _assertions_for_profile_without_frozen_module(
    command: Process[BuckResult, BuckException],
    file_path: Path,
    profiler: str,
) -> None:
    if profiler.endswith("-retained"):
        await expect_failure(
            command,
            stderr_regex="Retained memory profiling is available only for analysis profile",
        )
    else:
        await assert_flame_outputs(command, file_path, profiler)


async def assert_flame_outputs(
    command: Process[BuckResult, BuckException],
    file_path: Path,
    profiler: str,
) -> None:
    await command

    assert os.path.exists(file_path)
    if "flame" in profiler:
        assert os.path.exists(file_path / "flame.src")
        assert os.path.exists(file_path / "flame.svg")
    else:
        assert os.path.exists(file_path / "profile.csv")

    assert os.path.exists(file_path / "targets.txt")
