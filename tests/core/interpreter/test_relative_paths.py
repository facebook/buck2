# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_relative_path_basic(buck: Buck) -> None:
    assert "//foo/bar:test_basic" in (await buck.targets("//foo/bar:")).stdout


@buck_test()
async def test_relative_path_left_allowed_dir(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//foo/baz:"),
        stderr_regex="Relative import path `../../defs.bzl` is not allowed at the current location.",
    )


@buck_test()
async def test_relative_path_has_symlink(buck: Buck) -> None:
    os.symlink(buck.cwd, os.path.join(buck.cwd, "foo/sym"), target_is_directory=True)
    await expect_failure(
        buck.targets("//foo/sym/foo/bar:"),
        stderr_regex="Symlink found on the way from current dir `root//foo/sym/foo/bar` to allowed relative dir `root//foo`: `root//foo/sym`.",
    )
