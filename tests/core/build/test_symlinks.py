# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import shutil
import tempfile
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import expect_exec_count


def setup_symlink(symlink_path: Path, target: Path) -> None:
    symlink_path.parent.mkdir(parents=True, exist_ok=True)

    if not os.path.islink(symlink_path) and os.path.isdir(symlink_path):
        shutil.rmtree(symlink_path)
    else:
        symlink_path.unlink(missing_ok=True)

    os.symlink(target, symlink_path)


@buck_test(extra_buck_config={"buck2": {"use_correct_source_symlink_reading": "true"}})
async def test_symlink_target_tracked_for_rebuild(buck: Buck) -> None:
    setup_symlink(buck.cwd / "src" / "link", Path("../dir"))

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)

    await buck.build("//:cp")
    await expect_exec_count(buck, 0)

    with open(buck.cwd / "dir/file", "w") as file:
        file.write("GOODBYE\n")

    # This isn't really behavior  we want to guarantee and we'd rather users
    # don't use symlinks, but this is very observable (and it's not worse than
    # just reading the files then pretending they are never used!)
    await buck.build("//:cp")
    await expect_exec_count(buck, 1)


@buck_test(
    setup_eden=True,
    extra_buck_config={"buck2": {"use_correct_source_symlink_reading": "true"}},
)
async def test_symlinks_redirection(buck: Buck) -> None:
    setup_symlink(buck.cwd / "src" / "link", Path("../dir"))

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)

    await buck.build("//:cp")
    await expect_exec_count(buck, 0)

    # We change the symlink which should invalidate all files depending on it
    setup_symlink(buck.cwd / "src" / "link", Path("../dir2"))

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)


@buck_test(
    setup_eden=True,
    extra_buck_config={"buck2": {"use_correct_source_symlink_reading": "true"}},
)
async def test_symlinks_external(buck: Buck) -> None:
    top_level = Path(tempfile.mkdtemp())

    (top_level / "nested1").mkdir()
    (top_level / "nested2").mkdir()
    (top_level / "nested1" / "file").write_text("HELLO")
    (top_level / "nested2" / "file").write_text("GOODBYE")

    setup_symlink(buck.cwd / "ext" / "link", top_level / "nested1")

    await buck.build("//:ext")
    await expect_exec_count(buck, 1)

    await buck.build("//:ext")
    await expect_exec_count(buck, 0)

    setup_symlink(buck.cwd / "ext" / "link", top_level / "nested2")

    await buck.build("//:ext")
    await expect_exec_count(buck, 1)


@buck_test(extra_buck_config={"buck2": {"use_correct_source_symlink_reading": "true"}})
async def test_no_read_through_symlinks(buck: Buck) -> None:
    res = await buck.build_without_report(
        "//:stat_symlink",
        "--out",
        "-",
        "--remote-only",
    )
    # Just check that we don't always return `True`
    assert res.stdout.strip() == "False"

    setup_symlink(buck.cwd / "src" / "link", Path("..") / "dir")

    res = await buck.build_without_report(
        "//:stat_symlink",
        "--out",
        "-",
        "--remote-only",
    )
    assert res.stdout.strip() == "True"

    res = await buck.build_without_report(
        "//:stat_symlink_in_dir",
        "--out",
        "-",
        "--remote-only",
    )
    assert res.stdout.strip() == "True"


@buck_test(extra_buck_config={"buck2": {"use_correct_source_symlink_reading": "true"}})
async def test_no_read_through_source_symlinks_to_file(buck: Buck) -> None:
    res = await buck.build_without_report(
        "//:stat_symlink",
        "--out",
        "-",
        "--remote-only",
    )
    # Just check that we don't always return `True`
    assert res.stdout.strip() == "False"

    setup_symlink(
        buck.cwd / "src" / "link",
        Path("..") / "dir" / "file",
    )

    res = await buck.build_without_report(
        "//:stat_symlink",
        "--out",
        "-",
        "--remote-only",
    )
    assert res.stdout.strip() == "True"


@buck_test(setup_eden=True)
async def test_eden_io_read_symlink_dir_build_target(buck: Buck) -> None:
    setup_symlink(buck.cwd / "testlink", buck.cwd / "symdir" / "dir")

    await buck.build("//:symlink_dep")


@buck_test(setup_eden=True)
async def test_eden_io_read_symlink_dir_list_target(buck: Buck) -> None:
    setup_symlink(buck.cwd / "testlink", buck.cwd / "symdir")

    await buck.targets("//testlink/dir:")
