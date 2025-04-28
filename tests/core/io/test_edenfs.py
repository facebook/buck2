# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import os
import subprocess
from pathlib import Path

from buck2.tests.core.common.io.file_watcher import (
    FileWatcherEvent,
    FileWatcherEventType,
    FileWatcherKind,
    FileWatcherProvider,
    get_file_watcher_events,
)

from buck2.tests.core.common.io.file_watcher_dir_tests import (
    run_create_directory_test,
    run_remove_directory_test,
    run_rename_directory_test,
)
from buck2.tests.core.common.io.file_watcher_file_tests import (
    run_create_file_test,
    run_modify_file_test,
    run_remove_file_test,
    run_rename_file_test,
    run_replace_file_test,
)
from buck2.tests.core.common.io.file_watcher_scm_tests import (
    run_checkout_mergebase_changes_test,
    run_checkout_with_mergebase_test,
    run_rebase_with_mergebase_test,
    run_restack_with_mergebase_test,
    setup_file_watcher_scm_test,
)
from buck2.tests.core.common.io.file_watcher_tests import (
    FileSystemType,
    setup_file_watcher_test,
    verify_results,
)

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events


@buck_test(setup_eden=True)
async def test_edenfs_create_file(buck: Buck) -> None:
    await run_create_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_modify_file(buck: Buck) -> None:
    await run_modify_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_remove_file(buck: Buck) -> None:
    await run_remove_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_rename_file(buck: Buck) -> None:
    await run_rename_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


# File replace is not supported on Windows
@buck_test(setup_eden=True, skip_for_os=["windows"])
async def test_edenfs_replace_file(buck: Buck) -> None:
    await run_replace_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_create_directory(buck: Buck) -> None:
    await run_create_directory_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_remove_directory(buck: Buck) -> None:
    await run_remove_directory_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_rename_directory(buck: Buck) -> None:
    await run_rename_directory_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_checkout_mergebase_changes(buck: Buck) -> None:
    await run_checkout_mergebase_changes_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_checkout_with_mergebase(buck: Buck) -> None:
    await run_checkout_with_mergebase_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_rebase_with_mergebase(buck: Buck) -> None:
    await run_rebase_with_mergebase_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_restack_with_mergebase(buck: Buck) -> None:
    await run_restack_with_mergebase_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.EDEN_FS
    )


@buck_test(setup_eden=True)
async def test_edenfs_truncate_journal(buck: Buck) -> None:
    await setup_file_watcher_test(buck)
    subprocess.run(["edenfsctl", "debug", "flush_journal"], cwd=buck.cwd)

    is_fresh_instance, _ = await get_file_watcher_events(buck)
    assert is_fresh_instance


@buck_test(setup_eden=True)
async def test_edenfs_file_watcher_stats(buck: Buck) -> None:
    await setup_file_watcher_test(buck)

    file_stats = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "FileWatcher",
        "stats",
    )

    file_stats = file_stats[0]
    assert file_stats["fresh_instance"]
    assert file_stats["branched_from_revision"] is not None
    assert file_stats["branched_from_revision_timestamp"] is not None
    # we don't have global revision for test repo
    assert file_stats["branched_from_global_rev"] is None
    assert file_stats["eden_version"] is not None


@buck_test(setup_eden=True)
async def test_edenfs_files_report_on_fresh_instance(buck: Buck) -> None:
    await setup_file_watcher_test(buck)
    await setup_file_watcher_scm_test(buck)
    await buck.kill()

    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/ghi"
        ),
        FileWatcherEvent(
            FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/jkl"
        ),
    ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert is_fresh_instance
    verify_results(results, required)


@buck_test(
    setup_eden=True,
    # the test has subproject and creates buck-out in subproject,
    # when we setup eden we assume that buck-out is in project root dir
    # and redirect only that buck-out and not buck-out in subproject.
    # So, ignore soft errors that buck-out isn't redirected
    allow_soft_errors=True,
)
async def test_edenfs_changes_in_subproject(buck: Buck) -> None:
    cwd = Path("subproject")
    await buck.targets("cell//:", rel_cwd=cwd)

    with open(buck.cwd / "subproject" / "abc", "a"):
        pass

    _, results = await get_file_watcher_events(
        buck, target_pattern="cell//:", rel_cwd=cwd
    )
    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE,
            FileWatcherKind.FILE,
            "cell//abc",
        ),
    ]
    verify_results(results, required)


@buck_test(
    setup_eden=True,
    # the test has subproject and creates buck-out in subproject,
    # when we setup eden we assume that buck-out is in project root dir
    # and redirect only that buck-out and not buck-out in subproject.
    # So, ignore soft errors that buck-out isn't redirected
    allow_soft_errors=True,
)
async def test_edenfs_changes_outside_subproject(buck: Buck) -> None:
    cwd = Path("subproject")
    await buck.targets("cell//:", rel_cwd=cwd)

    with open(buck.cwd / "subproject" / "abc", "a"):
        pass

    with open(buck.cwd / "cde", "a"):
        pass

    _, results = await get_file_watcher_events(
        buck, target_pattern="cell//:", rel_cwd=cwd
    )
    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE,
            FileWatcherKind.FILE,
            "cell//abc",
        ),
    ]
    verify_results(results, required)


def create_and_commit(cwd: Path, path: str) -> str:
    os.makedirs(os.path.dirname(cwd / path), exist_ok=True)
    with open(cwd / path, "a"):
        pass

    subprocess.run(["sl", "commit", "--addremove", "-m", path], cwd=cwd)
    return subprocess.check_output(["sl", "whereami"], cwd=cwd).decode()


@buck_test(setup_eden=True)
async def test_edenfs_checkout_dir_changes(buck: Buck) -> None:
    # We want to create the following commit tree,
    # so when we move between f2 and f1 the mergbase doesn't change:
    #
    # o  xxxxxxxxf3 main
    # │
    # │ o  xxxxxxxxf2
    # ├─╯  files/d2/f2
    # │
    # o  xxxxxxxxf1

    f1 = create_and_commit(buck.cwd, "files/d1/f1")
    f2 = create_and_commit(buck.cwd, "files/d2/f2")
    subprocess.run(["sl", "co", f1], cwd=buck.cwd)
    subprocess.run(["sl", "bookmark", "main"], cwd=buck.cwd, check=True)
    create_and_commit(buck.cwd, "files/d3/f3")
    subprocess.run(["sl", "co", f2], cwd=buck.cwd)

    await buck.targets("root//:")
    subprocess.run(["sl", "co", f1], cwd=buck.cwd)
    is_fresh_instance, results = await get_file_watcher_events(buck)
    required = [
        FileWatcherEvent(
            FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/d2/f2"
        ),
        FileWatcherEvent(
            FileWatcherEventType.DELETE, FileWatcherKind.DIRECTORY, "root//files/d2"
        ),
        FileWatcherEvent(
            FileWatcherEventType.MODIFY, FileWatcherKind.DIRECTORY, "root//files"
        ),
    ]
    assert not is_fresh_instance
    verify_results(results, required)


@buck_test(setup_eden=True)
async def test_edenfs_directory_rename(buck: Buck) -> None:
    (buck.cwd / "d1").mkdir()
    (buck.cwd / "d1" / "TARGETS.fixture").touch()
    await buck.targets("root//d1:")

    (buck.cwd / "d1").rename(buck.cwd / "d2")
    # FIXME(JakobDegen): Bug: This directory doesn't exist.
    # Note: Also repros with watchman
    await buck.targets("root//d1:")
