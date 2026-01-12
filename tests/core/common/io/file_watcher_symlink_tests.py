# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from pathlib import Path

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


def symlink_file_type(file_watcher_provider: FileWatcherProvider) -> FileWatcherKind:
    # FIXME(JakobDegen): Bug. Notify consistently reports this incorrectly. It might be fine in
    # practice
    if file_watcher_provider == FileWatcherProvider.RUST_NOTIFY:
        return FileWatcherKind.FILE
    else:
        return FileWatcherKind.SYMLINK


async def run_create_symlink_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    (buck.cwd / "target").write_text("")

    await get_files(buck)

    path = buck.cwd / "files" / "def"
    path.symlink_to(Path("..") / "target")

    required = [
        FileWatcherEvent(
            FileWatcherEventType.CREATE,
            symlink_file_type(file_watcher_provider),
            "root//files/def",
        ),
    ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_replace_file_with_symlink_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    path = buck.cwd / "files" / "def"
    path.write_text("")
    (buck.cwd / "target").write_text("")
    await get_files(buck)

    path.unlink()
    path.symlink_to(Path("..") / "target")

    if file_watcher_provider == FileWatcherProvider.WATCHMAN:
        # FIXME(JakobDegen): Watchman non-deterministically produces either a create or modify
        # event
        # required = [
        #     FileWatcherEvent(
        #         FileWatcherEventType.MODIFY | FileWatcherEventType.CREATE,
        #         FileWatcherKind.SYMLINK,
        #         "root//files/def",
        #     ),
        # ]
        required = []
    else:
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE, FileWatcherKind.FILE, "root//files/def"
            ),
            FileWatcherEvent(
                FileWatcherEventType.CREATE,
                symlink_file_type(file_watcher_provider),
                "root//files/def",
            ),
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)


async def run_change_symlink_target_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    (buck.cwd / "target1").write_text("")
    (buck.cwd / "target2").write_text("")
    path = buck.cwd / "files" / "def"
    path.symlink_to(Path("..") / "target1")
    await get_files(buck)

    path.unlink()
    path.symlink_to(Path("..") / "target2")

    if file_watcher_provider == FileWatcherProvider.FS_HASH_CRAWLER:
        # FIXME(JakobDegen): Bug
        required = []
    elif file_watcher_provider == FileWatcherProvider.WATCHMAN:
        required = [
            # FIXME(JakobDegen): Bug. Watchman non-deterministically flip-flops between reporting
            # create and modify events
            # FileWatcherEvent(
            #     FileWatcherEventType.CREATE, FileWatcherKind.SYMLINK, "root//files/def"
            # ),
        ]
    else:
        required = [
            FileWatcherEvent(
                FileWatcherEventType.DELETE,
                symlink_file_type(file_watcher_provider),
                "root//files/def",
            ),
            FileWatcherEvent(
                FileWatcherEventType.CREATE,
                symlink_file_type(file_watcher_provider),
                "root//files/def",
            ),
        ]

    is_fresh_instance, results = await get_file_watcher_events(buck)
    assert not is_fresh_instance
    verify_results(results, required)
