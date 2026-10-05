# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import platform
import re

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import ExitCodeV2
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_run_with_source_macros(buck: Buck) -> None:
    sep = "\\" if platform.system() == "Windows" else "/"
    result = await buck.run("//source:echo_file")
    assert result.stdout.endswith(f"source{sep}foo.txt\n")

    result = await buck.run("//source:echo_dir")
    assert result.stdout.endswith(f"source{sep}bar\n")

    result = await buck.run("//source:cat_file")
    assert result.stdout == "foo file\n"

    result = await buck.run("//source:cat_dir")
    assert result.stdout == "bar file\n"


@buck_test()
async def test_no_dep_in_source(buck: Buck) -> None:
    await expect_failure(
        buck.build("//dep_as_source:uses_dep"),
        stderr_regex="Source file `:trivial` does not exist",
    )


@buck_test(allow_soft_errors=True)
@pytest.mark.parametrize(
    "macro, name, arg_count",
    [
        ("output foo.yaml", "output", 1),
        ("find . -name foo", "find", 3),
    ],
)
async def test_unrecognized_macro_fails_loading(
    buck: Buck, macro: str, name: str, arg_count: int
) -> None:
    (buck.cwd / "TARGETS.fixture").write_text(
        'load("//source:defs.bzl", "echo_rule")\n'
        f'echo_rule(name = "invalid", arg = "$({macro})")\n'
    )
    failure = await expect_failure(
        buck.uquery("//:invalid"),
        exit_code=ExitCodeV2.USER_ERROR,
        stderr_regex=re.escape(f"Unrecognized macro `{name}` (with {arg_count} args)"),
    )
    assert f"escape it as `\\$({name} ...)`" in failure.stderr
