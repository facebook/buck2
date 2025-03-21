# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import os
import re
import subprocess

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import json_get, random_string, read_what_ran


@buck_test()
async def test_local_action(buck: Buck) -> None:
    await buck.build(
        "//:foo",
        "--no-remote-cache",
        "--local-only",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    log = (await buck.log("show")).stdout.strip().splitlines()

    for line in log:
        outputs = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "outputs",
        )
        if outputs is None:
            continue
        # da39a3ee is a digest for empty directory.
        # We have 2 directories "a" and "z", where
        # "a" is empty and "z" is not.
        # "z" is a first output for action.
        digests = [o["tiny_digest"] for o in outputs]
        assert len(digests) == 2
        # Checking that "a" is first in action outputs
        assert digests[0] == "da39a3ee"
        return

    raise AssertionError("Didn't find ActionExecution data")


@buck_test()
async def test_remote_action(buck: Buck) -> None:
    await buck.build(
        "//:foo",
        "--no-remote-cache",
        "--remote-only",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    log = (await buck.log("show")).stdout.strip().splitlines()

    for line in log:
        outputs = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "outputs",
        )
        if outputs is None:
            continue
        # da39a3ee is a digest for empty directory.
        # We have 2 directories "a" and "z", where
        # "a" is empty and "z" is not.
        # "z" is a first output for action.
        digests = [o["tiny_digest"] for o in outputs]
        assert len(digests) == 2
        # Checking that "a" is first in action outputs
        assert digests[0] == "da39a3ee"
        break

    what_ran = await read_what_ran(buck)
    assert len(what_ran) == 1
    digest = what_ran[0]["reproducer"]["details"]["digest"]
    action_definition = subprocess.check_output(
        ["dotslash", os.environ["RECLI"], "cas", "download-action", digest],
        text=True,
    )
    # Though RE action has "a" first and then "z"
    assert (
        re.search(
            'Output\\WDirectories.+\n\\["buck-out/.+/__foo__/a",\\W"buck-out/.+/__foo__/z"',
            action_definition,
        )
        is not None
    )
