# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import subprocess
import tempfile
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, is_deployed_buck2


@buck_test(inplace=True)
async def test_executable_genrule(buck: Buck) -> None:
    result = await buck.run(
        "fbcode//buck2/tests/targets/rules/genrule:executable_helper"
    )
    assert result.stdout.strip() == "hello"


if not is_deployed_buck2():

    @buck_test(inplace=True, skip_for_os=["windows"])
    async def test_emit_shell(buck: Buck) -> None:
        result = await buck.run(
            "fbcode//buck2/tests/targets/rules/genrule:executable_helper",
            "--emit-shell",
        )

        out = subprocess.check_output(result.stdout, shell=True, encoding="utf-8")
        assert out.strip() == "hello"


@buck_test(inplace=True)
async def test_non_executable_genrule(buck: Buck) -> None:
    await expect_failure(
        buck.run("fbcode//buck2/tests/targets/rules/genrule:executable"),
        stderr_regex=r"Target `[^`]+` is not a binary rule \(only binary rules can be `run`\)",
    )


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_executable_genrule_arguments(buck: Buck) -> None:
    echo: str = "fbcode//buck2/tests/targets/rules/genrule:executable_echo_args"

    async def f(args1: List[str], args2: List[str]) -> None:
        result = await buck.run(echo, *args1, *args2)
        assert result.stdout.strip() == " ".join(args2)

    await f(["--"], ["val", "--long", "-s", "spa  ces"])
    await f(["--"], ["val", "--", "test"])
    await f([], ["val", "--long"])  # Would fail in Buck1 (--long not found)
    await f([], ["val", "--", "x"])  # Would work differently in Buck1 (no -- to user)
    await expect_failure(
        buck.run(echo, "--not-a-flag"),
        stderr_regex=r"unexpected argument '--not-a-flag'",
    )


@buck_test(inplace=True)
async def test_executable_fail_to_build(buck: Buck) -> None:
    await expect_failure(
        buck.run("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_3"),
        stderr_regex=r"Failed to build",
    )


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_input(buck: Buck) -> None:
    await buck.run("fbcode//buck2/tests/targets/run:expect", input=b"test")


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_change_cwd(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as tmpdirname:
        result = await buck.run(
            "fbcode//buck2/tests/targets/rules/command_alias:print_cwd",
            f"--chdir={tmpdirname}",
        )
        # e.g. in CI it's like this
        # assert '/var/folders/jq/7h2_h68s0ndbmc43k9cgf2zw000xbj/T/tmp2dk9jc68'
        # in '/private/var/folders/jq/7h2_h68s0ndbmc43k9cgf2zw000xbj/T/tmp2dk9jc68'
        assert tmpdirname in result.stdout.strip()


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_dont_change_cwd(buck: Buck) -> None:
    result = await buck.run(
        "fbcode//buck2/tests/targets/rules/command_alias:print_cwd",
    )
    assert os.getcwd() == result.stdout.strip()
