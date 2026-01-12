# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import sys

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


async def run_create_file_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = os.path.join(buck.cwd, "files", "def")
    with open(path, "a"):
        pass

    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/def"
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

    if (
        file_watcher_provider is FileWatcherProvider.RUST_NOTIFY
        and sys.platform == "win32"
    ):
        # The Windows support of notify does not "uniquely" support file modification events.
        # Rather, it generates create/delete events for a file and directory of the same name.
        required = [
            FileWatcherEvent(
                FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/abc"
            ),
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]

    else:
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

    required = [
        FileWatcherEvent(
            FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
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

    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE, FileWatcherKind.FILE, "root//files/def"
        ),
        FileWatcherEvent(
            FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
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

    # clear log - run build twice
    await buck.targets("root//:")
    await buck.targets("root//:")

    fromPath = os.path.join(buck.cwd, "files", "abc")
    toPath = os.path.join(buck.cwd, "files", "def")
    os.rename(fromPath, toPath)

    if file_watcher_provider in [
        FileWatcherProvider.EDEN_FS,
        FileWatcherProvider.WATCHMAN,
        FileWatcherProvider.RUST_NOTIFY,
    ]:
        required = [
            # Watchman reports a modify event for replacing a file if we have already cleared the
            # previous file watcher events from the log - this is the case when we get files as above.
            FileWatcherEvent(
                FileWatcherEventType.MODIFY
                if file_watcher_provider is FileWatcherProvider.WATCHMAN
                else FileWatcherEventType.CREATE,
                FileWatcherKind.FILE,
                "root//files/def",
            ),
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]
    else:
        # fs hash crawler returns a delete for replacing a file
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/abc"
            ),
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)
