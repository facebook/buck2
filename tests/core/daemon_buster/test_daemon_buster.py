# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_daemon_buster(buck: Buck) -> None:
    async def pid() -> int:
        return json.loads((await buck.status()).stdout)["process_info"]["pid"]

    await buck.build(":")
    pid0 = await pid()

    await buck.build(":")
    pid1 = await pid()
    assert pid1 == pid0

    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write("[buck2]\n")
        f.write("daemon_buster = 1\n")

    await buck.build(":")
    pid2 = await pid()
    assert pid2 != pid1

    await buck.build(":")
    pid3 = await pid()
    assert pid3 == pid2

    with open(buck.cwd / ".buckconfig", "a") as f:
        f.write("[buck2]\n")
        f.write("daemon_buster = 2\n")

    await buck.build(":")
    pid4 = await pid()
    assert pid4 != pid3

    with open(buck.cwd / ".buckconfig", "r") as f:
        config = f.read()

    with open(buck.cwd / ".buckconfig", "w") as f:
        f.write(
            "\n".join(
                line for line in config.splitlines() if "daemon_buster" not in line
            )
        )

    await buck.build(":")
    pid5 = await pid()
    assert pid5 != pid4
