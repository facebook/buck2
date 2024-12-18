# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import subprocess
from enum import Enum

from buck2.tests.core.common.io.file_watcher import (
    FileWatcherEvent,
    FileWatcherEventType,
    FileWatcherKind,
    FileWatcherProvider,
    get_file_watcher_events,
)

from buck2.tests.core.common.io.utils import get_files

from buck2.tests.e2e_util.api.buck import Buck


class FileSystemType(Enum):
    NATIVE = 0
    EDEN_FS = 1


async def setup_file_watcher_test(buck: Buck) -> None:
    # Fails on eden because the repo exists, that's ok
    subprocess.run(["sl", "init"], cwd=buck.cwd)
    subprocess.run(["sl", "commit", "--addremove", "-m", "temp"], cwd=buck.cwd)
    subprocess.run(["sl", "bookmark", "main"], cwd=buck.cwd, check=True)

    assert subprocess.check_output(["sl", "status"], cwd=buck.cwd) == b""
    assert (await get_files(buck)) == ["files/abc", "files/d/empty"]


def verify_results(
    results: list[FileWatcherEvent],
    required: list[FileWatcherEvent],
) -> None:
    for req in required:
        if req not in results:
            print(f"results={results}")
            print(f"required={required}")
            assert req in results, "required not in results"


async def run_aba_test(buck: Buck) -> None:
    # Fails on eden because the repo exists, that's ok
    subprocess.run(["sl", "init"], cwd=buck.cwd)
    subprocess.run(["sl", "commit", "--addremove", "-m", "temp"], cwd=buck.cwd)
    subprocess.run(["sl", "bookmark", "main"], cwd=buck.cwd, check=True)

    assert subprocess.check_output(["sl", "status"], cwd=buck.cwd) == b""

    assert (await get_files(buck)) == ["files/abc", "files/d/empty"]

    subprocess.run(["sl", "mv", "files/abc", "files/d/"], cwd=buck.cwd, check=True)
    assert (await get_files(buck)) == ["files/d/abc", "files/d/empty"]

    subprocess.run(["sl", "shelve"], cwd=buck.cwd, check=True)
    assert (await get_files(buck)) == ["files/abc", "files/d/empty"]


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

    verify_results((await get_file_watcher_events(buck)), required)


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

    verify_results((await get_file_watcher_events(buck)), required)


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

    verify_results((await get_file_watcher_events(buck)), required)


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

    verify_results((await get_file_watcher_events(buck)), required)


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

    verify_results((await get_file_watcher_events(buck)), required)
