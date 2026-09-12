# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

"""
An artifact that is (or contains) a symlink carries the values its symlinks
point at alongside it, so that materializing the symlink also puts something at
the other end of it. `test_modify_dep_materialization` in `test_materializer.py`
covers the single-daemon case; these tests cover the cases around it - a daemon
restart in the middle, targets that are source files rather than build
artifacts, and bare symlink artifacts rather than directories of symlinks.

Every read through a symlink is done by a local action copying what it read to
its own output, so that the assertion is on what a consumer actually saw.
"""

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


async def read_through(buck: Buck, target: str) -> str:
    """Build a `check` target and return what its action read."""
    result = await buck.build(target)
    return result.get_build_report().output_for_target(target).read_text().strip()


def write(buck: Buck, name: str, content: str) -> None:
    (buck.cwd / name).write_text(content + "\n", encoding="utf-8")


@buck_test(skip_for_os=["windows"])
async def test_symlinked_dir_to_artifact_across_restart(buck: Buck) -> None:
    assert await read_through(buck, "root//:check_dir_to_artifact") == "TEXT"

    # After the restart nothing the previous daemon held in memory is available,
    # so serving the new content has to work from the persisted state alone.
    await buck.kill()
    write(buck, "text", "TEXT2")
    assert await read_through(buck, "root//:check_dir_to_artifact") == "TEXT2"

    await buck.kill()
    write(buck, "text", "TEXT3")
    assert await read_through(buck, "root//:check_dir_to_artifact") == "TEXT3"


@buck_test(skip_for_os=["windows"])
async def test_restarted_daemon_materializes_symlink_and_its_target(buck: Buck) -> None:
    result = await buck.build(
        "root//:dir_to_artifact", "root//:remote_text", "--materializations=None"
    )
    report = result.get_build_report()
    link_dir = report.output_for_target("root//:dir_to_artifact")
    target = report.output_for_target("root//:remote_text")
    assert not link_dir.exists()
    assert not target.exists()

    await buck.kill()

    # Neither end of the symlink is on disk, so the restarted daemon has to put
    # both there before the consumer can read through it.
    assert await read_through(buck, "root//:check_dir_to_artifact") == "TEXT"
    assert (link_dir / "link").read_text().strip() == "TEXT"
    assert target.exists()


@buck_test(skip_for_os=["windows"])
async def test_symlinked_dir_to_source_file(buck: Buck) -> None:
    # Nothing in buck-out covers a source file, so there is no artifact for the
    # materializer to chase; the symlink resolves because the source is already
    # where it points.
    assert await read_through(buck, "root//:check_dir_to_source") == "SOURCE"

    write(buck, "source.txt", "SOURCE2")
    assert await read_through(buck, "root//:check_dir_to_source") == "SOURCE2"

    await buck.kill()

    write(buck, "source.txt", "SOURCE3")
    assert await read_through(buck, "root//:check_dir_to_source") == "SOURCE3"


@buck_test(skip_for_os=["windows"])
async def test_symlink_artifact_to_build_artifact(buck: Buck) -> None:
    assert await read_through(buck, "root//:check_file_to_artifact") == "TEXT"

    write(buck, "text", "TEXT2")
    assert await read_through(buck, "root//:check_file_to_artifact") == "TEXT2"

    await buck.kill()

    write(buck, "text", "TEXT3")
    assert await read_through(buck, "root//:check_file_to_artifact") == "TEXT3"


@buck_test(skip_for_os=["windows"])
async def test_symlink_artifact_to_source_file(buck: Buck) -> None:
    assert await read_through(buck, "root//:check_file_to_source") == "SOURCE"

    write(buck, "source.txt", "SOURCE2")
    assert await read_through(buck, "root//:check_file_to_source") == "SOURCE2"

    await buck.kill()

    write(buck, "source.txt", "SOURCE3")
    assert await read_through(buck, "root//:check_file_to_source") == "SOURCE3"


@buck_test(skip_for_os=["windows"])
async def test_symlink_artifact_target_escapes_the_artifact(buck: Buck) -> None:
    """The symlink's own value is a relative path leading out of it."""
    result = await buck.build("root//:file_to_artifact", "root//:remote_text")
    report = result.get_build_report()
    link = report.output_for_target("root//:file_to_artifact")
    target = report.output_for_target("root//:remote_text")

    assert link.is_symlink()
    assert not link.readlink().is_absolute()
    assert link.resolve() == target.resolve()
    assert link.read_text().strip() == "TEXT"
