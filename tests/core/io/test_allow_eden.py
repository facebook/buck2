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

# This file acts as both a test of `buck2.allow_eden_io` as well as a self-test
# of the `setup_eden` logic in the test runner


async def _check_io_provider(buck: Buck, name: str) -> None:
    await buck.server()
    out = await buck.status()
    status = json.loads(out.stdout.strip())
    io_provider = status["io_provider"]
    assert io_provider == name


@buck_test(
    setup_eden=False,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "false",
        }
    },
)
async def test_no_eden(buck: Buck) -> None:
    await _check_io_provider(buck, "fs")


@buck_test(
    setup_eden=False,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "true",
        }
    },
)
async def test_allow_eden_io_ignored_on_fs_io(buck: Buck) -> None:
    await _check_io_provider(buck, "fs")


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "false",
        }
    },
)
async def test_allow_eden_io_respected(buck: Buck) -> None:
    await _check_io_provider(buck, "fs")


@buck_test(
    setup_eden=True,
    extra_buck_config={
        "buck2": {
            "allow_eden_io": "true",
        }
    },
)
async def test_eden_io(buck: Buck) -> None:
    await _check_io_provider(buck, "eden")
