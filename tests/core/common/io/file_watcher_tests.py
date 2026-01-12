# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import subprocess
from enum import Enum

from buck2.tests.core.common.io.file_watcher import FileWatcherEvent
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
    await setup_file_watcher_test(buck)

    subprocess.run(["sl", "mv", "files/abc", "files/d/"], cwd=buck.cwd, check=True)
    assert (await get_files(buck)) == ["files/d/abc", "files/d/empty"]

    subprocess.run(["sl", "shelve"], cwd=buck.cwd, check=True)
    assert (await get_files(buck)) == ["files/abc", "files/d/empty"]
