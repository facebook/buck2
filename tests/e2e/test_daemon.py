import json
import time

import pytest

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env


@buck_test(inplace=True)
@env("BUCK2_TESTING_INACTIVITY_TIMEOUT", "true")
async def test_inactivity_timeout(buck: Buck) -> None:
    #######################################################
    # Recommend running this test in opt mode
    # Otherwise the comand that is run here
    # could take longer than 1 second to finish
    # causing this test to be flaky
    #######################################################

    # this will start the daemon
    await buck.targets("fbcode//buck2/tests/targets/daemon:rule")

    time.sleep(1)  # 1 sec timeout

    # check it's dead
    for _ in range(20):
        time.sleep(1)
        result = await buck.status()
        if result.stderr.splitlines()[-1] == "no buckd running":
            return

    raise AssertionError("Server did not die in 20 seconds")


@buck_test(inplace=True)
@pytest.mark.parametrize(
    "corrupt",
    ["not-json", '{"valid-json", "but-not-valid-data"}'],
)
async def test_corrupted_buckd_info(buck: Buck, corrupt: str) -> None:
    await buck.targets("fbcode//buck2/tests/targets/daemon:rule")

    daemon_dir_result = await buck.debug("daemon-dir")
    daemon_dir = daemon_dir_result.stdout.strip()
    with open(f"{daemon_dir}/buckd.info") as f:
        # Check file exists and valid.
        json.load(f)
    with open(f"{daemon_dir}/buckd.info", "w") as f:
        f.write(corrupt)

    await buck.targets("fbcode//buck2/tests/targets/daemon:rule")
