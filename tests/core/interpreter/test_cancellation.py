# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import time

from buck2.tests.e2e_util.api.buck import Buck

from buck2.tests.e2e_util.api.buck_result import BuckException, BuildResult
from buck2.tests.e2e_util.api.process import Process
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_cancellation_package(buck: Buck) -> None:
    a = buck.build(
        "--preemptible=always",
        "--config",
        "should.loop=package",
        ":target",
    )

    b = buck.build(
        ":target",
    )

    await cancellation_result(a, b)


@buck_test()
async def test_cancellation_analysis(buck: Buck) -> None:
    return
    await cancellation_result(
        buck.build(
            "--preemptible=always",
            "--config",
            "should.loop=analysis",
            ":target",
        ),
        buck.build(
            ":target",
        ),
    )


async def cancellation_result(
    aproc: Process[BuildResult, BuckException],
    bproc: Process[BuildResult, BuckException],
) -> None:
    # We need to give time to the above to start executing. On a heavily loaded
    # system we could still fail this race condition.
    aproc2 = await aproc.start()
    time.sleep(2)

    b = await bproc
    try:
        a = await aproc._get_result_or_raise_exception(aproc2)
        raise RuntimeError(
            "Expected to be preempted, but command completed successfully"
        )
    except BuckException as e:
        a = e

    # Note: one of the important features being tested here is not that a is
    # preempted (that already happens), but that b is run quickly afterwards
    # (IE: that the work a was doing was cancelled). That requires running both
    # commands with separate configurations, so that we exercise the wait for
    # the other config work to finish before dropping the existing dice path. If
    # the first command preempts but fails to short circuit the starlark
    # evaluation, the second will get stuck waiting for the first to finish, and
    # this test will time out.
    try:
        assert a.process.returncode != 0
        assert "buck daemon preempted this command" in a.stderr
        b.check_returncode()
    except Exception:
        print("A STDERR:\n", a.stdout, "\n\nA STDOUT:\n", a.stderr)
        print("B STDERR:\n", b.stdout, "\n\nB STDOUT:\n", b.stderr)
        raise
