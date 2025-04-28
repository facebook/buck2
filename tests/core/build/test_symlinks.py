# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import shutil
import sys
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import expect_exec_count


@buck_test()
async def test_symlinks(buck: Buck) -> None:
    # We want to check in a symlink but given Buck is running this and symlinks
    # do not exist we need to put it back and make it be an actual symlink.
    symlink_path = os.path.join(buck.cwd, "src", "link")

    if os.path.isdir(symlink_path):
        shutil.rmtree(symlink_path)
    else:
        os.remove(symlink_path)

    src = "..\\dir" if sys.platform == "win32" else "../dir"
    os.symlink(src, symlink_path, target_is_directory=True)

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


@buck_test(setup_eden=True)
async def test_symlinks_redirection(buck: Buck) -> None:
    symlink_path = os.path.join(buck.cwd, "src", "link")

    if os.path.isdir(symlink_path):
        shutil.rmtree(symlink_path)
    else:
        os.remove(symlink_path)

    src = "..\\dir" if sys.platform == "win32" else "../dir"
    os.symlink(src, symlink_path)

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)

    await buck.build("//:cp")
    await expect_exec_count(buck, 0)

    # We change the symlink which should invalidate all files depending on it
    os.remove(symlink_path)
    src2 = "..\\dir2" if sys.platform == "win32" else "../dir2"
    os.symlink(src2, symlink_path)

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)


@buck_test(setup_eden=True)
async def test_symlinks_external(buck: Buck) -> None:
    symlink_path = os.path.join(buck.cwd, "ext", "link")
    shutil.rmtree(symlink_path)
    top_level = tempfile.mkdtemp()

    os.mkdir(os.path.join(top_level, "nested1"))
    os.mkdir(os.path.join(top_level, "nested2"))
    with open(os.path.join(top_level, "nested1", "file"), "w") as f:
        f.write("HELLO")
    with open(os.path.join(top_level, "nested2", "file"), "w") as f:
        f.write("GOODBYE")

    os.symlink(os.path.join(top_level, "nested1"), symlink_path)

    await buck.build("//:ext")
    await expect_exec_count(buck, 1)

    await buck.build("//:ext")
    await expect_exec_count(buck, 0)

    os.remove(symlink_path)
    os.symlink(os.path.join(top_level, "nested2"), symlink_path)

    await buck.build("//:ext")
    await expect_exec_count(buck, 1)
