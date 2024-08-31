# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import os

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_exists(buck: Buck) -> None:
    await buck.bxl("//bxl:fs.bxl:exists", "--", "--root_path", str(buck.cwd))


@buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
async def test_bxl_fs_exists_symlink(buck: Buck) -> None:
    link_path = buck.cwd / "symlink/foo/bar"
    if not os.path.islink(link_path):
        os.unlink(link_path)
        os.symlink("../bar", link_path)
    await buck.bxl("//bxl:fs.bxl:exists_symlink")


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_list(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:fs.bxl:list_relative_path",
    )

    assert result.stdout.splitlines() == [
        "root//bin/TARGETS.fixture",
        "root//bin/kind",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_absolute_path", "--", "--root_path", str(buck.cwd)
    )

    assert result.stdout.splitlines() == [
        "root//bin/TARGETS.fixture",
        "root//bin/kind",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_source_artifact",
    )

    assert result.stdout.splitlines() == [
        "root//bin/kind/TARGETS.fixture",
        "root//bin/kind/rules.bzl",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_file_node",
    )

    assert result.stdout.splitlines() == [
        "root//bin/kind/TARGETS.fixture",
        "root//bin/kind/rules.bzl",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_dirs_only",
    )

    assert result.stdout.splitlines() == [
        "root//bin/kind",
    ]

    result = await buck.bxl("//bxl:fs.bxl:list_cell_path")

    expected_output = [
        "root//bin/TARGETS.fixture",
        "root//bin/kind",
    ]

    output = json.loads(result.stdout)
    assert output["@root//bin"] == expected_output
    assert output["root//bin"] == expected_output
    assert output["//bin"] == expected_output


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_is_file(buck: Buck) -> None:
    await buck.bxl("//bxl:fs.bxl:is_file", "--", "--root_path", str(buck.cwd))


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_is_dir(buck: Buck) -> None:
    await buck.bxl("//bxl:fs.bxl:is_dir", "--", "--root_path", str(buck.cwd))


@buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
async def test_bxl_fs_project_rel_path(buck: Buck) -> None:
    result = await buck.bxl("//bxl:fs.bxl:project_rel_path")

    assert result.stdout.splitlines() == [
        "bin/kind/TARGETS.fixture",
        "bin/kind/rules.bzl",
    ]


@buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
async def test_bxl_fs_abs_path_unsafe(buck: Buck) -> None:
    result = await buck.bxl("//bxl:fs.bxl:abs_path_unsafe")

    assert result.stdout.splitlines() == [
        str(buck.cwd / "bin/kind/TARGETS.fixture"),
        str(buck.cwd / "bin/kind/rules.bzl"),
    ]


@buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
async def test_bxl_fs_source(buck: Buck) -> None:
    await buck.bxl("//bxl:fs.bxl:source")

    await expect_failure(
        buck.bxl("//bxl:fs.bxl:source_invalid_path"),
        stderr_regex="Inferred package path `root//fs` is not a valid package within the given file path `root//this/path/does/not/exist",
    )
    await expect_failure(
        buck.bxl("//bxl:fs.bxl:source_invalid_hint"),
        stderr_regex="Inferred package path `root//bin/kind` is not a valid package within the given file path `root//fs/src/source",
    )
    await expect_failure(
        buck.bxl("//bxl:fs.bxl:source_too_many_hints"),
        stderr_regex="Expected a single target hint, not an iterable",
    )


@buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
async def test_bxl_file_set_ops(buck: Buck) -> None:
    await buck.bxl("//bxl/fs.bxl:file_set_operations")
