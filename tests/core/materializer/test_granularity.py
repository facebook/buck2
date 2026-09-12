# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""
Materializing an artifact must not leave content from a previous layout
underneath it. These tests move a target's artifact boundaries between two
builds - a directory artifact becomes files underneath it, or the reverse - and
check that the second build's outputs are correct on disk and that the first
build's layout does not survive underneath them.

Both directions also run with a daemon restart between the two builds, so that
the second build has only persisted state to work from.
"""

from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import replace_in_file


def use_second_layout(buck: Buck, name: str) -> None:
    """Switch `//:{name}` to the `{name}_2` layout defined in `defs.bzl`."""
    replace_in_file(
        f'variant = "{name}_1"',
        f'variant = "{name}_2"',
        buck.cwd / "TARGETS.fixture",
    )


async def build_outputs(buck: Buck, target: str) -> List[Path]:
    result = await buck.build(target)
    return sorted(result.get_build_report().outputs_for_target(target))


async def _dir_to_files(buck: Buck, restart: bool) -> None:
    target = "root//:dir_to_files"

    (out,) = await build_outputs(buck, target)
    assert (out / "a").read_text() == "A1"
    assert (out / "sub" / "keep").read_text() == "K1"
    assert (out / "sub" / "stale").read_text() == "S1"

    use_second_layout(buck, "dir_to_files")
    if restart:
        await buck.kill()

    # The new artifacts land inside the path the directory artifact used to
    # occupy; without this the checks below would be vacuous.
    assert await build_outputs(buck, target) == [out / "a", out / "sub"]
    assert (out / "a").read_text() == "A2"
    assert (out / "sub" / "keep").read_text() == "K2"
    assert not (out / "sub" / "stale").exists()


async def _files_to_dir(buck: Buck, restart: bool) -> None:
    target = "root//:files_to_dir"

    a, b = await build_outputs(buck, target)
    out = a.parent
    assert [a, b] == [out / "a", out / "b"]
    assert a.read_text() == "A1"
    assert b.read_text() == "B1"

    use_second_layout(buck, "files_to_dir")
    if restart:
        await buck.kill()

    assert await build_outputs(buck, target) == [out]
    assert (out / "a").read_text() == "A2"
    assert (out / "c").read_text() == "C2"
    # `b` is not part of the directory artifact, so it must not be visible to
    # anything reading the directory.
    assert not (out / "b").exists()


@buck_test()
async def test_directory_artifact_becomes_files_inside_it(buck: Buck) -> None:
    await _dir_to_files(buck, restart=False)


@buck_test()
async def test_directory_artifact_becomes_files_inside_it_across_restart(
    buck: Buck,
) -> None:
    await _dir_to_files(buck, restart=True)


@buck_test()
async def test_files_become_a_directory_artifact_around_them(buck: Buck) -> None:
    await _files_to_dir(buck, restart=False)


@buck_test()
async def test_files_become_a_directory_artifact_around_them_across_restart(
    buck: Buck,
) -> None:
    await _files_to_dir(buck, restart=True)


@buck_test()
async def test_directory_artifact_contents_shrink(buck: Buck) -> None:
    target = "root//:shrinking_dir"

    (out,) = await build_outputs(buck, target)
    assert (out / "keep.txt").read_text() == "KEEP1"
    assert (out / "stale.txt").read_text() == "STALE"

    use_second_layout(buck, "shrinking_dir")

    assert await build_outputs(buck, target) == [out]
    assert (out / "keep.txt").read_text() == "KEEP2"
    assert not (out / "stale.txt").exists()


@buck_test()
async def test_local_consumer_does_not_see_removed_entry(buck: Buck) -> None:
    # The tests above look at the disk state a final materialization leaves
    # behind. This one looks at what a local action consuming the directory
    # artifact as an input can see in it.
    target = "root//:listed_dir_listing"

    result = await buck.build(target)
    listing = result.get_build_report().output_for_target(target)
    assert listing.read_text() == "keep,stale"

    use_second_layout(buck, "listed_dir")

    result = await buck.build(target)
    assert result.get_build_report().output_for_target(target).read_text() == "keep"
