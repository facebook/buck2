# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


# These tests pin down the user-facing error messages buck produces when a
# target pattern references a path that doesn't resolve to a package — missing
# directory, missing build file, path-element-is-a-file, typo'd directory name,
# and wrong-cell suggestions. The ignored-directory and cross-cell-traversal
# variants live in `test_path_validity.py`.
#
# Windows error messages differ in detail and aren't covered here.


async def _check(buck: Buck, pattern: str, golden_name: str) -> None:
    res = await expect_failure(buck.uquery(pattern, "-v=0", "--console=none"))
    golden(
        output=sanitize_stderr(res.stderr),
        rel_path=f"golden/{golden_name}.golden.stderr",
    )


class ErrorsTest(BuckTestCase):
    @buck_test(skip_for_os=["windows"])
    async def test_missing_dir(self, buck: Buck) -> None:
        await _check(buck, "//completely_missing/foo/x/y/lmnop:target", "missing_dir")

    @buck_test(skip_for_os=["windows"])
    async def test_missing_targets_file(self, buck: Buck) -> None:
        await _check(buck, "//missing_targets_file:target", "missing_targets_file")

    @buck_test(skip_for_os=["windows"])
    async def test_path_is_file_leaf(self, buck: Buck) -> None:
        await _check(buck, "//data.file:target", "path_is_file_leaf")

    @buck_test(skip_for_os=["windows"])
    async def test_path_is_file_intermediate(self, buck: Buck) -> None:
        await _check(buck, "//data.file/subdir:target", "path_is_file_intermediate")

    @buck_test(skip_for_os=["windows"])
    async def test_top_level_typo(self, buck: Buck) -> None:
        # `top_level_present/` is a real directory; the query mistypes it.
        await _check(buck, "//top_level_presents:target", "top_level_typo")

    @buck_test(skip_for_os=["windows"])
    async def test_subdir_typo(self, buck: Buck) -> None:
        # `typo_subdir/subdir/` is real; the query mistypes the leaf.
        await _check(buck, "//typo_subdir/subdirs:target", "subdir_typo")

    @buck_test(skip_for_os=["windows"])
    async def test_wrong_cell_suggestion(self, buck: Buck) -> None:
        # `something/` exists at the root of the `cell` cell, not in `root`.
        await _check(buck, "root//something:target", "wrong_cell_suggestion")
