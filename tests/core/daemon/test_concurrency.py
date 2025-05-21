# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
from pathlib import Path
from typing import Optional, Tuple

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, BuildResult
from buck2.tests.e2e_util.api.process import Process
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import read_invocation_record


@buck_test()
async def test_exit_when_different_state(buck: Buck) -> None:
    a = buck.build(
        "-c",
        "foo.bar=1",
        "--exit-when-different-state",
        ":long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    b = buck.build(
        "-c",
        "foo.bar=2",
        "--exit-when-different-state",
        ":long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    # create a coroutine that can return a result
    async def process(
        p: Process[BuildResult, BuckException],
    ) -> Tuple[Optional[int], str]:
        result = await expect_failure(p)
        return (result.process.returncode, result.stderr)

    done, pending = await asyncio.wait(
        [asyncio.create_task(process(a)), asyncio.create_task(process(b))],
        timeout=10,
        return_when=asyncio.FIRST_COMPLETED,
    )

    assert len(done) == 1
    assert len(pending) == 1

    # these are sets, so can't index them.
    for task in done:
        exit_code, stderr = task.result()
        assert "daemon is busy" in stderr
        assert exit_code == 4


@buck_test()
@pytest.mark.parametrize("same_state", [True, False])
async def test_exit_when_preemptible_always(buck: Buck, same_state: bool) -> None:
    a = buck.build(
        "-c",
        "foo.bar=1",
        "--preemptible=always",
        ":long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    b = buck.build(
        "-c",
        # We expect to ALWAYS preempt commands, to prevent blocking new callees
        "foo.bar=1" if same_state else "foo.bar=2",
        "--preemptible=always",
        ":long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    # create a coroutine that can return a result
    async def process(
        p: Process[BuildResult, BuckException],
    ) -> Tuple[Optional[int], str]:
        result = await expect_failure(p)
        return (result.process.returncode, result.stderr)

    done, pending = await asyncio.wait(
        [asyncio.create_task(process(a)), asyncio.create_task(process(b))],
        timeout=10,
        return_when=asyncio.FIRST_COMPLETED,
    )

    assert len(done) == 1
    assert len(pending) == 1

    # these are sets, so can't index them.
    for task in done:
        exit_code, stderr = task.result()
        assert "daemon preempted" in stderr
        assert exit_code == 5


@buck_test()
async def test_preemptible_logged(buck: Buck, tmp_path: Path) -> None:
    record_path = tmp_path / "record.json"
    await buck.targets(
        "--preemptible=always",
        ":",
        "--unstable-write-invocation-record",
        str(record_path),
    )
    record = read_invocation_record(record_path)
    assert record["preemptible"] == "ALWAYS"


@buck_test()
@pytest.mark.parametrize("same_state", [True, False])
async def test_exit_when_preemptible_on_different_state(
    buck: Buck, same_state: bool
) -> None:
    a = buck.build(
        "-c",
        "foo.bar=1",
        "--preemptible=ondifferentstate",
        ":long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    b = buck.build(
        "-c",
        # We expect to ALWAYS preempt commands, to prevent blocking new callees
        "foo.bar=1" if same_state else "foo.bar=2",
        "--preemptible=ondifferentstate",
        ":long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    # create a coroutine that can return a result
    async def process(
        p: Process[BuildResult, BuckException],
    ) -> Tuple[Optional[int], str]:
        result = await expect_failure(p)
        return (result.process.returncode, result.stderr)

    done, pending = await asyncio.wait(
        [asyncio.create_task(process(a)), asyncio.create_task(process(b))],
        timeout=10,
        return_when=asyncio.FIRST_COMPLETED,
    )

    if same_state:
        # No preempt when state is the same
        assert len(done) == 0
        assert len(pending) == 2
    else:
        assert len(done) == 1
        assert len(pending) == 1

    # These are sets, so can't index them. Expect all done tasks to be "done" because they're preempted
    for task in done:
        exit_code, stderr = task.result()
        assert "daemon preempted" in stderr
        assert exit_code == 5
