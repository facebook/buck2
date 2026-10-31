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


# The report of an ignored read is a quiet soft error; this puts it on stderr.
SHOW_IGNORED_READS = {"BUCK2_SHOW_SOFT_ERRORS": "only=path_check_ignored_read"}


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

    @buck_test(allow_soft_errors=True)
    async def test_source_ignored(self, buck: Buck) -> None:
        # Ignored source files named explicitly are read, with a soft error saying
        # why the read will be refused.
        (buck.cwd / "ignored_src").write_text("HELLO\n")
        (buck.cwd / "defs.bzl").write_text(
            "def _impl(ctx):\n"
            '    return [DefaultInfo(default_output = ctx.actions.copy_file("out", ctx.attrs.src))]\n'
            "cp = rule(impl = _impl, attrs = {'src': attrs.source()})\n"
        )
        (buck.cwd / "TARGETS.fixture").write_text(
            'load(":defs.bzl", "cp")\ncp(name = "cp", src = "ignored_src")\n'
        )
        res = await buck.build("//:cp", env=SHOW_IGNORED_READS)
        self.assertIn(
            "`ignored_src` matches the `project.ignore` pattern `ignored*`", res.stderr
        )
        self.assertIn("reading ignored files will become an error", res.stderr)

    @buck_test(allow_soft_errors=True)
    async def test_source_ignored_behind_symlink(self, buck: Buck) -> None:
        # One report, naming the symlink, for an ignored source reached through it.
        (buck.cwd / "real").mkdir()
        (buck.cwd / "real" / "f").write_text("HELLO\n")
        (buck.cwd / "ignored_dir").mkdir()
        os.symlink(
            os.path.join("..", "real"),
            buck.cwd / "ignored_dir" / "link",
            target_is_directory=True,
        )
        (buck.cwd / "defs.bzl").write_text(
            "def _impl(ctx):\n"
            '    return [DefaultInfo(default_output = ctx.actions.copy_file("out", ctx.attrs.src))]\n'
            "cp = rule(impl = _impl, attrs = {'src': attrs.source()})\n"
        )
        (buck.cwd / "TARGETS.fixture").write_text(
            'load(":defs.bzl", "cp")\ncp(name = "cp", src = "ignored_dir/link/f")\n'
        )
        res = await buck.build("//:cp", env=SHOW_IGNORED_READS)
        self.assertEqual(
            res.stderr.count("matches the `project.ignore` pattern"), 1, res.stderr
        )
        self.assertIn("`ignored_dir/link` is under `ignored_dir`", res.stderr)

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
    @buck_test(allow_soft_errors=True)
    async def test_load_symlink(self, buck: Buck) -> None:
        os.symlink("realdir", buck.cwd / "linkdir")
        (buck.cwd / "loader").mkdir()
        (buck.cwd / "loader" / "TARGETS.fixture").write_text(
            'load("//linkdir:lib.bzl", "value")\nstub(name = "loader_" + value)\n'
        )
        await expect_failure(buck.targets("//loader:"))
