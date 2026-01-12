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

from buck2.tests.core.common.io.file_watcher import (
    FileWatcherEvent,
    FileWatcherEventType,
    FileWatcherKind,
    FileWatcherProvider,
    get_file_watcher_events,
)
from buck2.tests.core.common.io.file_watcher_tests import (
    FileSystemType,
    setup_file_watcher_test,
    verify_results,
)
from buck2.tests.e2e_util.api.buck import Buck


async def run_create_directory_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = os.path.join(buck.cwd, "files", "def")
    os.mkdir(path)

    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE,
            FileWatcherKind.DIRECTORY,
            "root//files/def",
        )
    ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_remove_directory_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = os.path.join(buck.cwd, "files", "d")
    shutil.rmtree(path)

    if (
        file_watcher_provider is FileWatcherProvider.WATCHMAN
        and file_system_type is FileSystemType.EDEN_FS
    ):
        # Watchman on EdenFS reports a delete *file* event for removing a directory
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/d"
            ),
        ]
    else:
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE,
                FileWatcherKind.DIRECTORY,
                "root//files/d",
            ),
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_rename_directory_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    fromPath = os.path.join(buck.cwd, "files", "d")
    toPath = os.path.join(buck.cwd, "files", "def")
    os.rename(fromPath, toPath)

    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE,
            FileWatcherKind.DIRECTORY,
            "root//files/def",
        ),
        # Watchman on EdenFS reports a delete file event for the fromPath directory
        FileWatcherEvent(
            FileWatcherEventType.DELETE,
            FileWatcherKind.FILE
            if file_watcher_provider is FileWatcherProvider.WATCHMAN
            and file_system_type is FileSystemType.EDEN_FS
            else FileWatcherKind.DIRECTORY,
            "root//files/d",
        ),
    ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)
