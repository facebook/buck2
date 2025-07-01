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

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_unhashed_putputs(buck: Buck) -> None:
    await buck.build("//pack:trivial_build")

    p = buck.cwd / "buck-out" / "v2" / "gen" / "root" / "pack" / "foo.txt"
    assert p.exists()
    assert p.is_symlink()


@buck_test()
async def test_projected_output(buck: Buck) -> None:
    await buck.build("//:projected_output")

    p = buck.cwd / "buck-out" / "v2" / "gen" / "root" / "dir"
    assert p.exists()
    assert p.is_symlink()
    assert (p / "file").is_file()


@buck_test()
async def test_build_symlink_does_not_traverse_existing_symlinks(buck: Buck) -> None:
    await buck.build("//pack:trivial_build")
    symlink_folder = buck.cwd / "buck-out" / "v2" / "gen" / "root" / "pack"

    # Now, overwrite part of the symlink path with something we cannot traverse.
    path = symlink_folder.parent
    shutil.rmtree(path)
    # On Windows this is just non existing path.
    os.symlink("/dev/null", path)

    # Can we still build? If we delete the symlink when walking up the path, we
    # can. If we traverse it, we can't.
    await buck.build("//pack:trivial_build")
