# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_bxl_actions(buck: Buck) -> None:
    result = await buck.bxl(
        "//artifact_test/artifacts.bxl:artifact_test",
    )

    # FIXME(JakobDegen): The first assert doesn't test anything the second doesn't cover
    assert "<source artifact_test/TARGETS.fixture>" in result.stdout
    assert "[<source artifact_test/TARGETS.fixture>]" in result.stdout


@buck_test()
async def test_bxl_create_build_actions(buck: Buck) -> None:
    result = await buck.bxl(
        "//actions_test:actions.bxl:build_actions_test",
        "--",
        "--content",
        "my_content",
    )
    assert (buck.cwd / Path(result.stdout.strip())).read_text() == "my_content"


@buck_test()
async def test_resolve(buck: Buck) -> None:
    result = await buck.bxl(
        "//resolve_test:resolve.bxl:resolve_test",
    )

    assert "a-string\n" == result.stdout


@buck_test(skip_for_os=["windows"])
async def test_bxl_declared_artifact_path(buck: Buck) -> None:
    result = await buck.bxl(
        "//actions_test/declared_artifact_path.bxl:declared_artifact_path_test",
    )

    output = result.stdout.splitlines()
    # first line is result of get_path_without_materialization, second line is output of ctx.output.ensure
    assert output[0] == output[1]


@buck_test()
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
