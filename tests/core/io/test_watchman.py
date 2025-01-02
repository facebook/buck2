# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from buck2.tests.core.common.io.file_watcher import FileWatcherProvider
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
    run_checkout_wtih_mergebase_test,
    run_rebase_wtih_mergebase_test,
    run_restack_wtih_mergebase_test,
)
from buck2.tests.core.common.io.file_watcher_tests import FileSystemType

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(setup_eden=False)
async def test_watchman_create_file_no_eden(buck: Buck) -> None:
    await run_create_file_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_create_file_eden(buck: Buck) -> None:
    await run_create_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_modify_file_no_eden(buck: Buck) -> None:
    await run_modify_file_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_modify_file_eden(buck: Buck) -> None:
    await run_modify_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_remove_file_no_eden(buck: Buck) -> None:
    await run_remove_file_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_remove_file_eden(buck: Buck) -> None:
    await run_remove_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_rename_file_no_eden(buck: Buck) -> None:
    await run_rename_file_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_rename_file_eden(buck: Buck) -> None:
    await run_rename_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


# File replace is not supported on Windows
@buck_test(setup_eden=False, skip_for_os=["windows"])
async def test_watchman_replace_file_no_eden(buck: Buck) -> None:
    await run_replace_file_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


# File replace is not supported on Windows
@buck_test(setup_eden=True, skip_for_os=["windows"])
async def test_watchman_replace_file_eden(buck: Buck) -> None:
    await run_replace_file_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_create_directory_no_eden(buck: Buck) -> None:
    await run_create_directory_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_create_directory_eden(buck: Buck) -> None:
    await run_create_directory_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_remove_directory_no_eden(buck: Buck) -> None:
    await run_remove_directory_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_remove_directory_eden(buck: Buck) -> None:
    await run_remove_directory_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_rename_directory_no_eden(buck: Buck) -> None:
    await run_rename_directory_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_rename_directory_eden(buck: Buck) -> None:
    await run_rename_directory_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_checkout_mergebase_changes_no_eden(buck: Buck) -> None:
    await run_checkout_mergebase_changes_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_checkout_mergebase_changes_eden(buck: Buck) -> None:
    await run_checkout_mergebase_changes_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_checkout_with_mergebase_no_eden(buck: Buck) -> None:
    await run_checkout_wtih_mergebase_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_checkout_with_mergebase_eden(buck: Buck) -> None:
    await run_checkout_wtih_mergebase_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_rebase_with_mergebase_no_eden(buck: Buck) -> None:
    await run_rebase_wtih_mergebase_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_rebase_with_mergebase_eden(buck: Buck) -> None:
    await run_rebase_wtih_mergebase_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=False)
async def test_watchman_restack_with_mergebase_no_eden(buck: Buck) -> None:
    await run_restack_wtih_mergebase_test(
        buck, FileSystemType.NATIVE, FileWatcherProvider.WATCHMAN
    )


@buck_test(setup_eden=True)
async def test_watchman_restack_with_mergebase_eden(buck: Buck) -> None:
    await run_restack_wtih_mergebase_test(
        buck, FileSystemType.EDEN_FS, FileWatcherProvider.WATCHMAN
    )