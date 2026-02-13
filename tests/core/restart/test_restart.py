# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import json
import os
import signal

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test

TEST_DIGEST = "76f7aea8c1fc400287312b9608ceb24848ba02ac:14"


@buck_test()
async def test_restart_requires_no_stdout(buck: Buck) -> None:
    res = await buck.targets("//:stage0", env={"FORCE_WANT_RESTART": "true"})
    assert res.stdout.count("//:stage0") == 1


@buck_test()
async def test_restart(buck: Buck) -> None:
    # Normally shows once.
    res = await expect_failure(buck.targets("//:invalid"))
    assert res.stderr.count("Unknown target `invalid`") == 1

    # But if we force a restart...
    res = await expect_failure(
        buck.targets("//:invalid", env={"FORCE_WANT_RESTART": "true"})
    )
    assert res.stderr.count("Unknown target `invalid`") == 2


@buck_test(allow_soft_errors=True)
async def test_restart_materializer_corruption(buck: Buck) -> None:
    stage1 = "//:stage1"
    res = await buck.build(stage1)
    out = res.get_build_report().output_for_target(stage1)

    # Now we remove this file (which comes to us via RE)
    # Only way to get it back is by killing the materializer state.
    os.unlink(out)

    res = await buck.build("//:stage2")
    assert "Your command will now restart" in res.stderr


@buck_test(allow_soft_errors=True)
async def test_restart_cas_missing(buck: Buck) -> None:
    # Make sure Buck is not running.
    await buck.kill()

    # Start a daemon with the `src` file tombstoned. This means we cannot download it from RE.
    # This is just the hash of `src`.
    await buck.build(env={"BUCK2_TEST_TOMBSTONED_DIGESTS": TEST_DIGEST})

    # Now build //:stage2. Buck2 must try to download the file, fail, then
    # restart the daemon.
    res = await buck.build("//:stage2")
    assert "Your command will now restart" in res.stderr

    # TODO: We should also handle the case where the top-level artifact is what
    # fails to download (i.e. build stage1 here instead).


@buck_test(
    allow_soft_errors=True,
    skip_for_os=["windows"],
)
async def test_restart_forkserver_crash(buck: Buck) -> None:
    # Start the daemon
    await buck.build()

    # Kill its forkserver.
    forkserver_pid = json.loads((await buck.status()).stdout)["forkserver_pid"]
    assert forkserver_pid is not None
    os.kill(forkserver_pid, signal.SIGKILL)

    # Wait for its forkserver to exit.
    for _ in range(10):
        try:
            os.kill(forkserver_pid, 0)
        except OSError:
            break
        else:
            await asyncio.sleep(1)

    # Now build a thing and check we restart
    res = await buck.build("//:stage2")
    assert "Your command will now restart" in res.stderr


@buck_test()
async def test_restart_disabled(buck: Buck) -> None:
    # Ensure no daemon
    await buck.kill()

    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write("[buck2]\nrestarter = false")

    result = await expect_failure(
        buck.build(
            "//:stage2",
            env={"BUCK2_TEST_TOMBSTONED_DIGESTS": TEST_DIGEST},
        ),
    )
    assert "Your command will now restart" not in result.stderr


@buck_test(write_invocation_record=True)
async def test_trace_id(buck: Buck) -> None:
    trace_id = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"

    # But if we force a restart...
    res = await expect_failure(
        buck.targets(
            "//:invalid",
            env={"FORCE_WANT_RESTART": "true", "BUCK_WRAPPER_UUID": trace_id},
        )
    )
    record = res.invocation_record()
    assert record["trace_id"] != trace_id
    assert record["restarted_trace_id"] == trace_id
    assert record["should_restart"] is False
