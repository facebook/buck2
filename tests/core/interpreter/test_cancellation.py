# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import time

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException
from buck2.tests.e2e_util.buck_workspace import buck_test


ALL_STAGES = ["load", "package", "analysis", "bxl"]


@buck_test()
@pytest.mark.parametrize("stage", ALL_STAGES)
async def test_cancellation(buck: Buck, stage: str) -> None:
    if stage == "bxl":
        preempted_target = "//:root.bxl:loop_test"
        a_method = buck.bxl
    else:
        preempted_target = ":target"
        a_method = buck.build

    aproc = a_method(
        "--preemptible=always",
        "--config",
        f"should.loop={stage}",
        preempted_target,
    )
    bproc = buck.build(
        ":target",
    )

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
