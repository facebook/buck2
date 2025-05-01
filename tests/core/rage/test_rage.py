# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import tempfile

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def opener(path: str, flags: int) -> int:
    # Make it executable by user
    return os.open(path, flags, 0o777)


def mock_cmd_unix(path: str) -> None:
    with open(path, "w", opener=opener) as fl:
        fl.write(
            """\
#! /bin/sh
echo "$@"
        """
        )


# No windows since mocking pastry command didn't work D41623200
@buck_test(skip_for_os=["windows"])
async def test_rage(buck: Buck) -> None:
    # Build a trivial action
    await buck.build("//:simple")

    with tempfile.TemporaryDirectory() as tmpdirname:
        pastry_path = f"{tmpdirname}/pastry"
        hg_path = f"{tmpdirname}/hg"
        mock_cmd_unix(pastry_path)
        mock_cmd_unix(hg_path)

        # We want to find our executable first
        cmd_path = tmpdirname + os.pathsep + os.environ["PATH"]
        # Run rage aginst the most recent invocation.
        await buck.rage(input=b"0", env={"PATH": cmd_path})


@buck_test()
async def test_rage_no_paste(buck: Buck) -> None:
    # Build a trivial action
    await buck.build("//:simple")
    # Run rage aginst the most recent invocation.
    await buck.rage("--no-paste", "--invocation-offset", "0")


@buck_test()
async def test_rage_no_logs(buck: Buck) -> None:
    # Rage doesn't crash even with no invocation logs
    await buck.rage("--no-paste")
