# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


async def _build_output(buck: Buck, target: str) -> Path:
    result = await buck.build(target, "--show-output")
    path = result.get_target_to_build_output().get(target)
    assert path is not None
    return buck.cwd / path


# Symlink materialization assertions are unreliable on Windows.
@buck_test(skip_for_os=["windows"])
async def test_assembled_dir_mixes_copies_and_symlinks(buck: Buck) -> None:
    out = await _build_output(buck, "root//:mixed")
    assert out.is_dir()

    # `assembled_dir.copy` entries are materialized as real files, with the
    # source's bytes, at their entry paths (including nested ones).
    for name, data in [
        ("bin/exe", "exe-bytes"),
        ("bin/exe.resources.json", "manifest-bytes"),
        ("src_copy.txt", "source-file-bytes\n"),
    ]:
        entry = out / name
        assert entry.is_file(), name
        assert not entry.is_symlink(), name
        assert entry.read_text() == data, name

    # `assembled_dir.symlink` entries are materialized as symlinks that
    # resolve to the source artifact's bytes.
    for name, data in [
        ("deep/nested/link", "exe-bytes"),
        ("src_link.txt", "source-file-bytes\n"),
    ]:
        entry = out / name
        assert entry.is_symlink(), name
        assert entry.read_text() == data, name

    # A symlinked directory artifact resolves as a directory.
    res_dir = out / "res"
    assert res_dir.is_symlink()
    assert (res_dir / "data.txt").read_text() == "resource-bytes"


@buck_test(skip_for_os=["windows"])
async def test_assembled_dir_is_a_usable_input(buck: Buck) -> None:
    # A downstream action can read both copied and symlinked entries through
    # the assembled dir (i.e. the entries' sources are tracked as inputs).
    out = await _build_output(buck, "root//:consumed")
    assert out.read_text() == "exe-bytes|source-file-bytes\n"


@buck_test()
async def test_assembled_dir_rejects_overlapping_paths(buck: Buck) -> None:
    await expect_failure(
        buck.build("root//:overlap_fail"),
        stderr_regex="must be non-overlapping",
    )


@buck_test()
async def test_assembled_dir_rejects_empty_path(buck: Buck) -> None:
    await expect_failure(
        buck.build("root//:empty_path_fail"),
        stderr_regex="must not be empty",
    )


@buck_test()
async def test_assembled_dir_rejects_untyped_entries(buck: Buck) -> None:
    # A bare artifact is not a valid entry: contents values must be built
    # with `assembled_dir.copy(...)` / `assembled_dir.symlink(...)`.
    await expect_failure(
        buck.build("root//:untyped_entry_fail"),
        stderr_regex="AssembledDirEntry",
    )
