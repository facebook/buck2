# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import random
import string
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/actions.bxl:artifact_test",
    )

    assert "<source bin/TARGETS.fixture>" in result.stdout
    assert "[<source bin/TARGETS.fixture>]" in result.stdout


@buck_test(inplace=False, data_dir="bxl/actions")
async def test_bxl_create_build_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//actions_test:actions.bxl:build_actions_test",
        "--",
        "--content",
        "my_content",
    )
    assert (buck.cwd / Path(result.stdout.strip())).read_text() == "my_content"


@buck_test(inplace=False, data_dir="bxl/actions")
async def test_resolve(buck: Buck) -> None:
    result = await buck.bxl(
        "//resolve_test:resolve.bxl:resolve_test",
    )

    assert "a-string\n" == result.stdout


@buck_test(inplace=False, data_dir="bxl/execution_platforms")
async def test_bxl_exec_platform_dynamic_output(buck: Buck) -> None:
    result = await buck.bxl(
        "//executor_fallback_tests/dynamic.bxl:test_dynamic_output",
        "-c",
        f"test.cache_buster={random_string()}",
        "--local-only",
    )

    output = result.stdout.splitlines()[0]
    assert os.path.exists(buck.cwd / Path(output))

    await expect_failure(
        buck.bxl(
            "//executor_fallback_tests/dynamic.bxl:test_dynamic_output",
            "-c",
            f"test.cache_buster={random_string()}",
            "--remote-only",
        ),
        stderr_regex="Incompatible executor preferences",
    )


@buck_test(inplace=False, data_dir="bxl/execution_platforms")
async def test_bxl_execution_platforms(buck: Buck) -> None:
    result = await buck.bxl(
        "//executor_fallback_tests/test.bxl:test_exec_platforms",
        "-c",
        f"test.cache_buster={random_string()}",
        "--",
        "--exec_deps",
        "//executor_fallback_tests:remote_only",
    )

    output = result.stdout.splitlines()[0]
    assert os.path.exists(buck.cwd / Path(output))

    await expect_failure(
        buck.bxl(
            "//executor_fallback_tests/test.bxl:test_exec_platforms",
            "-c",
            f"test.cache_buster={random_string()}",
            "--",
            "--exec_deps",
            "//executor_fallback_tests:local_only",
        )
    )

    result = await buck.bxl(
        "//executor_fallback_tests/test.bxl:test_exec_platforms",
        "-c",
        f"test.cache_buster={random_string()}",
        "--",
        "--toolchains",
        "//executor_fallback_tests:remote_only_toolchain",
    )

    output = result.stdout.splitlines()[0]
    assert os.path.exists(buck.cwd / Path(output))

    await expect_failure(
        buck.bxl(
            "//executor_fallback_tests/test.bxl:test_exec_platforms",
            "-c",
            f"test.cache_buster={random_string()}",
            "--",
            "--toolchains",
            "//executor_fallback_tests:local_only_toolchain",
        )
    )

    result = await buck.bxl(
        "//executor_fallback_tests/test.bxl:test_exec_compatible_with",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    output = result.stdout.splitlines()[0]
    assert os.path.exists(buck.cwd / Path(output))

    await expect_failure(
        buck.bxl(
            "//executor_fallback_tests/test.bxl:test_exec_compatible_with",
            "-c",
            f"test.cache_buster={random_string()}",
            "--remote-only",
        )
    )


@buck_test(inplace=False, skip_for_os=["windows"], data_dir="bxl/actions")
async def test_bxl_declared_artifact_path(buck: Buck) -> None:
    result = await buck.bxl(
        "//actions_test/declared_artifact_path.bxl:declared_artifact_path_test",
    )

    output = result.stdout.splitlines()
    # first line is result of get_path_without_materialization, second line is output of ctx.output.ensure
    assert output[0] == output[1]


def random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))


@buck_test(inplace=False, data_dir="bxl/actions")
async def test_bxl_build_and_write(buck: Buck) -> None:
    # Performs a failed build and a successful action.
    res = await buck.bxl(
        "//actions_test:actions.bxl:build_and_write",
        "--",
        "--target",
        "actions_test:fail",
    )

    assert res.process.returncode == 0
    assert "BXL SUCCEEDED" in res.stderr
