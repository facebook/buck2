# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_health_check_with_request_hang(buck: Buck) -> None:
    (
        health_check_state_file,
        server_output,
        server_process,
    ) = await start_health_check_server(buck, "--with-request-hang")

    env = {
        # Set the CLI_PATH to `echo` making it effectively a no-op since we want buck to use the test server and not spawn a new one.
        "BUCK2_HEALTH_CHECK_CLI_PATH": "echo",
        "BUCK2_HEALTH_CHECK_STATE_INFO_PATH": health_check_state_file,
    }
    await buck.build(
        "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        env=env,
    )
    with open(server_output, "r") as f:
        lines = f.readlines()
        # Only the first call to update is logged. The server is stuck processing that request.
        assert lines.count("UpdateContext Requested\n") == 1
        assert lines.count("RunChecks Requested\n") == 0
        assert lines.count("UpdateContext Completed\n") == 0
        assert lines.count("RunChecks Completed\n") == 0

    server_process.kill()


async def start_health_check_server(
    buck: Buck, request_setup: str
) -> tuple[str, str, asyncio.subprocess.Process]:
    server_output = tempfile.NamedTemporaryFile("w", delete=False).name
    health_check_state_file = tempfile.NamedTemporaryFile("w", delete=False).name

    # Start the server before the build begins to ensure that the server is ready to accept requests.
    # This is necessary since the run_request_hang_server.sh script may need to build the server target.
    cmd = buck.run(
        "fbcode//buck2/tests/e2e/health_check:health_check_server_bin",
        "--",
        "--isolation-dir",  # Avoids interference with the build of test target
        "health_check_server",
        "--state-info-file",
        health_check_state_file,
        request_setup,
        env={"HEALTH_CHECK_SERVER_STATS_OUTPUT": server_output},
    )
    process = await cmd.start()

    # Loop until the tcp_port is written which implies that the health check server has started
    while True:
        with open(health_check_state_file, "r") as f:
            if f.read().strip():
                break
        await asyncio.sleep(0.1)

    return (health_check_state_file, server_output, process)
