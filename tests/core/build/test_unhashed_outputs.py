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


@buck_test()
async def test_conflict_with_content_based_paths(buck: Buck) -> None:
    # TODO(jtbraun) some of the logic below needs to be updated once
    # content-based paths get deconflicted. For now, this tests the conflicting
    # behavior.
    symlink_path = (
        buck.cwd / "buck-out" / "v2" / "gen" / "root" / "conflict" / "shared_name"
    )
    content_based_path = (
        buck.cwd / "buck-out" / "v2" / "cbp" / "root" / "conflict" / "shared_name"
    )
    # sanity check that we're starting from a clean state
    assert not symlink_path.exists()
    assert not content_based_path.exists()

    #
    # Build just the subtarget. Esnsure that the subtarget output exists and is
    # reacable, and that it lives in the place we expect.
    #
    res = await buck.build(
        "//conflict/shared_name:subtarget",
        "--config",
        "buck2.create_unhashed_links=false",
    )
    subtarget_output = res.get_build_report().output_for_target(
        "root//conflict/shared_name:subtarget"
    )
    assert symlink_path.is_dir()  # TODO(jtbraun): should be not symlink_path.exists() and content_based_path.is_dir()
    assert subtarget_output.is_symlink()
    assert subtarget_output.resolve().is_relative_to(
        symlink_path
    )  # TODO(jtbraun) should be (content_based_path)
    assert subtarget_output.resolve().is_file()
    # Verify we can read the contents of the file
    with open(subtarget_output) as f:
        f.read()

    #
    # Build the conflicting target w/o unhashed links. This should leave the
    # subtarget_output alone, which should remain readable.
    #
    await buck.build(
        "//conflict:shared_name",
        "--config",
        "buck2.create_unhashed_links=false",
    )
    # TODO(jtbraun): this should instead ensure the symlink does NOT exist, and the content_based_path does and is a folder
    assert symlink_path.exists()  # should be not, but the cbp is here for now
    assert symlink_path.is_dir()  # remove when cbp exists
    assert not content_based_path.is_dir()  # should be true when cbp turned on
    # The subtarget should still be available as we haven't overwritten it yet
    assert subtarget_output.is_symlink()
    assert subtarget_output.resolve().is_relative_to(
        symlink_path
    )  # TODO(jtbraun) should be (content_based_path)
    assert subtarget_output.resolve().is_file()
    # Verify we can read the contents of the file
    with open(subtarget_output) as f:
        f.read()

    #
    # Build the conflicting target with unhashed links. This will overwrite the
    # subtarget with a directory, and the symlink_path will now exist.
    #
    await buck.build("//conflict:shared_name")
    assert (
        not subtarget_output.is_dir()
    )  # TODO: should pass all the same checks as above
    assert symlink_path.is_symlink()
    assert symlink_path.readlink().is_file()
    assert (
        not content_based_path.exists()
    )  # TODO(jtbraun): this should be assert content_based_path.is_dir()
