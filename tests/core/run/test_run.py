# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import subprocess
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_run_executable(buck: Buck) -> None:
    result = await buck.run("root//:print_hello")
    assert result.stdout.strip() == "hello"


@buck_test(skip_for_os=["windows"])
async def test_emit_shell(buck: Buck) -> None:
    result = await buck.run(
        "root//:print_hello",
        "--emit-shell",
    )

    out = subprocess.check_output(result.stdout, shell=True, encoding="utf-8")
    assert out.strip() == "hello"


@buck_test(write_invocation_record=True)
async def test_run_non_executable_fails(buck: Buck) -> None:
    res = await expect_failure(
        buck.run(
            "root//:no_run_info",
        ),
        stderr_regex=r"Target `[^`]+` is not a binary rule \(only binary rules can be `run`\)",
    )

    record = res.invocation_record()
    [error] = record["errors"]

    assert error["category_key"] == "RunCommandError::NonBinaryRule"
    assert error["category"] == "USER"


@buck_test(write_invocation_record=True)
async def test_run_exit_result(buck: Buck) -> None:
    res = await buck.run(
        "root//:print_hello",
    )
    record = res.invocation_record()
    assert record["exit_result_name"] == "EXEC"


@buck_test(allow_soft_errors=True)
async def test_passing_arguments(buck: Buck) -> None:
    async def f(args1: List[str], args2: List[str]) -> None:
        result = await buck.run("root//:echo_args", *args1, *args2)
        assert result.stdout.strip() == " ".join(args2)

    await f(["--"], ["val", "--long", "-s", "spa  ces"])
    await f(["--"], ["val", "--", "test"])
    # Without --, a deprecation warning is emitted but command still succeeds
    result = await buck.run("root//:echo_args", "val", "--long")
    assert result.stdout.strip() == "val --long"
    assert "will require" in result.stderr
    await f([], ["val", "--", "x"])  # Would work differently in Buck1 (no -- to user)
    await expect_failure(
        buck.run("root//:echo_args", "--not-a-flag"),
        stderr_regex=r"unexpected argument '--not-a-flag'",
    )


@buck_test()
async def test_executable_fail_to_build(buck: Buck) -> None:
    await expect_failure(
        buck.run("root//:build_fail"),
        stderr_regex=r"Failed to build",
    )


@buck_test(allow_soft_errors=True)
async def test_run_args_without_separator_warning(buck: Buck) -> None:
    result = await buck.run("root//:echo_args", "my_arg")
    assert result.stdout.strip() == "my_arg"
    assert "will require" in result.stderr


@buck_test()
async def test_input(buck: Buck) -> None:
    await buck.run("root//:check_input_test", input=b"test")


@buck_test()
async def test_change_cwd(buck: Buck, tmp_path: Path) -> None:
    result = await buck.run(
        "root//:print_cwd",
        f"--chdir={tmp_path}",
    )
    assert tmp_path.resolve() == Path(result.stdout.strip()).resolve()


@buck_test()
async def test_dont_change_cwd(buck: Buck) -> None:
    result = await buck.run("root//:print_cwd")
    assert buck.cwd == Path(result.stdout.strip())
