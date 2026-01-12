# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import subprocess

from buck2.tests.core.common.io.file_watcher import (
    FileWatcherProvider,
    get_file_watcher_events,
)
from buck2.tests.core.common.io.file_watcher_tests import (
    FileSystemType,
    setup_file_watcher_test,
)
from buck2.tests.e2e_util.api.buck import Buck


# Setup repo structure to test these conditions: https://www.internalfb.com/excalidraw/EX346258
async def setup_file_watcher_scm_test(buck: Buck) -> tuple[str, str, str, str]:
    # Run after setup_file_watcher_test to create a simple stack of commits
    commit_a = subprocess.check_output(["sl", "whereami"], cwd=buck.cwd).decode()

    # Create a file
    path = os.path.join(buck.cwd, "files", "def")
    with open(path, "a"):
        pass

    # Commit it
    subprocess.run(["sl", "commit", "--addremove", "-m", "commit_b"], cwd=buck.cwd)
    commit_b = subprocess.check_output(["sl", "whereami"], cwd=buck.cwd).decode()

    # Go back to commit_a
    subprocess.run(["sl", "co", commit_a], cwd=buck.cwd)

    # Create a file
    path = os.path.join(buck.cwd, "files", "ghi")
    with open(path, "a"):
        pass

    # Commit it
    subprocess.run(["sl", "commit", "--addremove", "-m", "commit_c"], cwd=buck.cwd)
    commit_c = subprocess.check_output(["sl", "whereami"], cwd=buck.cwd).decode()

    # Create a file
    path = os.path.join(buck.cwd, "files", "jkl")
    with open(path, "a"):
        pass

    # Commit it
    subprocess.run(["sl", "commit", "--addremove", "-m", "commit_d"], cwd=buck.cwd)
    commit_d = subprocess.check_output(["sl", "whereami"], cwd=buck.cwd).decode()

    # clear log - run build twice
    await buck.targets("root//:")
    await buck.targets("root//:")

    return commit_a, commit_b, commit_c, commit_d


async def run_checkout_mergebase_changes_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)

    # Create a file
    path = os.path.join(buck.cwd, "files", "def")
    with open(path, "a"):
        pass

    # Commit it
    subprocess.run(["sl", "commit", "--addremove", "-m", "next"], cwd=buck.cwd)
    commit_a = subprocess.check_output(["sl", "whereami"], cwd=buck.cwd).decode()

    # Create a file
    path = os.path.join(buck.cwd, "files", "ghi")
    with open(path, "a"):
        pass

    # Commit it
    subprocess.run(["sl", "commit", "--addremove", "-m", "next"], cwd=buck.cwd)
    commit_b = subprocess.check_output(["sl", "whereami"], cwd=buck.cwd).decode()

    # Go back to the previous commit
    subprocess.run(["sl", "co", commit_a], cwd=buck.cwd)

    is_fresh_instance, _ = await get_file_watcher_events(buck)
    if file_watcher_provider in [
        FileWatcherProvider.FS_HASH_CRAWLER,
        FileWatcherProvider.RUST_NOTIFY,
    ]:
        # Stats only records the first 100 events (https://fburl.com/code/x9esqun4)
        # so we can't verify the results when making commit transitions
        assert not is_fresh_instance
    else:
        # We might have some events even for a fresh instance, so we ignore
        assert is_fresh_instance

    # Go back to the next commit
    subprocess.run(["sl", "co", commit_b], cwd=buck.cwd)

    is_fresh_instance, results = await get_file_watcher_events(buck)
    print(results)

    if file_watcher_provider in [
        FileWatcherProvider.FS_HASH_CRAWLER,
        FileWatcherProvider.RUST_NOTIFY,
    ]:
        assert not is_fresh_instance
    else:
        assert is_fresh_instance


async def run_checkout_with_mergebase_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    [_, _, commit_c, _] = await setup_file_watcher_scm_test(buck)

    # Go back to commit_c
    subprocess.run(["sl", "co", commit_c], cwd=buck.cwd)

    is_fresh_instance, results = await get_file_watcher_events(buck)
    print(results)

    assert not is_fresh_instance


async def run_rebase_with_mergebase_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    [_, commit_b, commit_c, _] = await setup_file_watcher_scm_test(buck)

    # Rebase C->D from A to B
    subprocess.run(["sl", "rebase", "-s", commit_c, "-d", commit_b], cwd=buck.cwd)

    is_fresh_instance, results = await get_file_watcher_events(buck)
    print(results)

    if file_system_type == FileSystemType.NATIVE:
        # Watchman is flaky on native file systems
        if file_watcher_provider != FileWatcherProvider.WATCHMAN:
            assert not is_fresh_instance
    else:
        assert is_fresh_instance


async def run_restack_with_mergebase_test(
    buck: Buck,
    file_system_type: FileSystemType,
    file_watcher_provider: FileWatcherProvider,
) -> None:
    await setup_file_watcher_test(buck)
    [commit_a, _, _, commit_d] = await setup_file_watcher_scm_test(buck)

    # Rebase D from C to A
    subprocess.run(["sl", "rebase", "-s", commit_d, "-d", commit_a], cwd=buck.cwd)

    is_fresh_instance, results = await get_file_watcher_events(buck)
    print(results)

    assert not is_fresh_instance
