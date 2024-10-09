# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_discovery_cached(buck: Buck) -> None:
    args = [
        "-c",
        "buck2.cache_test_listings=true",
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, False, args)
    await run_test_and_check_discovery_presence(
        buck, False, args
    )  # will be true once the implementation is done


@buck_test(inplace=False)
async def test_discovery_cache_turned_off(buck: Buck) -> None:
    args = [
        "//:ok",
    ]
    await run_test_and_check_discovery_presence(buck, False, args)
    await run_test_and_check_discovery_presence(buck, False, args)


async def run_test_and_check_discovery_presence(
    buck: Buck,
    is_present: bool,
    args: List[str],
) -> None:
    await buck.test(*args)
    stdout = (await buck.log("what-ran")).stdout

    assert "test.run" in stdout
    if is_present:
        assert "test.discovery" not in stdout
    else:
        assert "test.discovery" in stdout
