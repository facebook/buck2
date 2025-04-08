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
from buck2.tests.e2e_util.helper.utils import read_invocation_record


@buck_test()
async def test_bxl_exec_platform_dynamic_output(buck: Buck, tmp_path: Path) -> None:
    result = await buck.bxl(
        "//executor_fallback_tests/dynamic.bxl:test_dynamic_output",
        "-c",
        f"test.cache_buster={random_string()}",
        "--local-only",
    )

    output = result.stdout.splitlines()[0]
    assert os.path.exists(buck.cwd / Path(output))

    record_path = tmp_path / "record.json"

    await expect_failure(
        buck.bxl(
            "//executor_fallback_tests/dynamic.bxl:test_dynamic_output",
            "-c",
            f"test.cache_buster={random_string()}",
            "--remote-only",
            "--unstable-write-invocation-record",
            str(record_path),
        ),
        stderr_regex="Incompatible executor preferences",
    )

    record = read_invocation_record(record_path)
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["category"] == "USER"


@buck_test()
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


def random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))
