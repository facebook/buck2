# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import platform

from buck2.tests.e2e_util.api.buck import Buck
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
