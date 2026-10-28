# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import unittest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


class PathValidityTest(BuckTestCase):
    # These tests assert that buck2 refuses to read source files when a path element
    # along the way is "invalid" in some sense: wrong-case, a symlink, ignored, or
    # crossing a cell boundary. Each test sets up a `TARGETS.fixture` reachable via
    # such a path and asks buck to read it; the assertion is that buck rejects.
    #
    # A test marked `expectedFailure` describes a check buck does not perform yet;
    # closing the gap makes the marker fail and it can be dropped.

    @buck_test()
    async def test_target_pattern_wrong_case(self, buck: Buck) -> None:
        # `realdir/TARGETS.fixture` exists; querying with `Realdir` is rejected on
        # every platform, whatever the file system makes of the spelling.
        res = await expect_failure(buck.targets("//Realdir:", "-v0"))
        golden(
            output=sanitize_stderr(res.stderr),
            rel_path="golden/target_pattern_wrong_case.golden.stderr",
        )

    # Symlinks in a target pattern's path are read through.
    @unittest.expectedFailure
    @buck_test(allow_soft_errors=True)
    async def test_target_pattern_symlink(self, buck: Buck) -> None:
        os.symlink("realdir", buck.cwd / "linkdir")
        await expect_failure(buck.targets("//linkdir:"))

    @buck_test()
    async def test_target_pattern_ignored(self, buck: Buck) -> None:
        # `ignored*` is listed in `[project] ignore` (matching the dir but not its
        # contents); a `TARGETS.fixture` reachable only by traversing through an
        # ignored directory is rejected. NB: a bare `ignore = ignored` entry is
        # silently rewritten to also cover `ignored/**`, which would not exercise
        # the path-element check — hence the glob form.
        res = await expect_failure(buck.targets("//ignored/inside:", "-v0"))
        golden(
            output=sanitize_stderr(res.stderr),
            rel_path="golden/target_pattern_ignored.golden.stderr",
        )

    @buck_test()
    async def test_target_pattern_cell_traverse(self, buck: Buck) -> None:
        # `subcell/` is its own cell. A `root//` target pattern must not be
        # able to reach into it.
        res = await expect_failure(buck.targets("root//subcell/subdir:", "-v0"))
        golden(
            output=sanitize_stderr(res.stderr),
            rel_path="golden/target_pattern_cell_traverse.golden.stderr",
        )

    @buck_test()
    async def test_load_cell_traverse(self, buck: Buck) -> None:
        # The analogue of `test_target_pattern_cell_traverse` for `load()`.
        (buck.cwd / "loader").mkdir()
        (buck.cwd / "loader" / "TARGETS.fixture").write_text(
            'load("//subcell/subdir:lib.bzl", "value")\nstub(name = "loader_" + value)\n'
        )
        res = await expect_failure(buck.targets("//loader:", "-v0"))
        golden(
            output=sanitize_stderr(res.stderr),
            rel_path="golden/load_cell_traverse.golden.stderr",
        )

    # Symlinks in a `load()` path are read through.
    @unittest.expectedFailure
    @buck_test()
    async def test_load_symlink(self, buck: Buck) -> None:
        os.symlink("realdir", buck.cwd / "linkdir")
        (buck.cwd / "loader").mkdir()
        (buck.cwd / "loader" / "TARGETS.fixture").write_text(
            'load("//linkdir:lib.bzl", "value")\nstub(name = "loader_" + value)\n'
        )
        await expect_failure(buck.targets("//loader:"))
