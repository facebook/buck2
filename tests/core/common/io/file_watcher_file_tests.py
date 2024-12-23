# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os

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

from buck2.tests.core.common.io.utils import get_files

from buck2.tests.e2e_util.api.buck import Buck


async def run_create_file_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = os.path.join(buck.cwd, "files", "def")
    with open(path, "a"):
        pass

    if file_watcher_provider != FileWatcherProvider.RUST_NOTIFY:
        required = [
            FileWatcherEvent(
                FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/def"
            )
        ]
    else:
        # notify returns modify file events for all changes
        required = [
            FileWatcherEvent(
                FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/def"
            )
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_modify_file_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = os.path.join(buck.cwd, "files", "abc")
    with open(path, "a") as f:
        f.write("modify")

    required = [
        FileWatcherEvent(
            FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/abc"
        )
    ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_remove_file_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = os.path.join(buck.cwd, "files", "abc")
    os.remove(path)

    if file_watcher_provider != FileWatcherProvider.RUST_NOTIFY:
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            )
        ]
    else:
        # notify returns modify file events for all changes
        required = [
            FileWatcherEvent(
                FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/abc"
            )
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_rename_file_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)

    fromPath = os.path.join(buck.cwd, "files", "abc")
    toPath = os.path.join(buck.cwd, "files", "def")
    os.rename(fromPath, toPath)

    if file_watcher_provider != FileWatcherProvider.RUST_NOTIFY:
        required = [
            FileWatcherEvent(
                FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/def"
            ),
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]
    else:
        # notify returns modify file events for all changes
        required = [
            FileWatcherEvent(
                FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/def"
            ),
            FileWatcherEvent(
                FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_replace_file_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)

    path = os.path.join(buck.cwd, "files", "def")
    with open(path, "a"):
        pass

    # By getting files here, we clear the log of the previous file watcher events
    # including the create event for 'def'; this also removes the directory modify event
    # that we would normally expect to see when creating a file in a directory on native file systems
    await get_files(buck)

    fromPath = os.path.join(buck.cwd, "files", "abc")
    toPath = os.path.join(buck.cwd, "files", "def")
    os.rename(fromPath, toPath)

    if file_watcher_provider in [
        FileWatcherProvider.EDEN_FS,
        FileWatcherProvider.WATCHMAN,
    ]:
        required = [
            # Watchman reports a modify event for replacing a file if we have already cleared the
            # previous file watcher events from the log - this is the case when we get files as above.
            FileWatcherEvent(
                FileWatcherEventType.MODIFY
                if file_watcher_provider is not FileWatcherProvider.EDEN_FS
                else FileWatcherEventType.CREATE,
                FileWatcherKind.FILE,
                "root//files/def",
            ),
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]
    elif file_watcher_provider is FileWatcherProvider.FS_HASH_CRAWLER:
        # fs hash crawler returns a delete for replacing a file
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]

    else:
        # notify returns modify file events for all changes
        required = [
            FileWatcherEvent(
                FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/def"
            ),
            FileWatcherEvent(
                FileWatcherEventType.MODIFY, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)
