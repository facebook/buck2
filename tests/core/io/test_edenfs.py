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
