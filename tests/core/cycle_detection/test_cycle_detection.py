# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
from typing import Awaitable

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, BuckResult
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def check_load_cycle_stderr(stderr: str) -> None:
    # We're not sure which order these will appear.
    assert r"root//load_cycle/3.bzl ->" in stderr
    assert r"root//load_cycle/2.bzl ->" in stderr
    assert r"root//load_cycle/1.bzl ->" in stderr


def check_cfg_graph_cycle_stderr(stderr: str) -> None:
    # We're not sure which order these will appear.
    assert r"root//:cycle_bot (<unspecified>) ->" in stderr
    assert r"root//:cycle_mid (<unspecified>) ->" in stderr
    assert r"root//:cycle_top (<unspecified>) ->" in stderr


def check_cfg_toolchain_graph_cycle_stderr(stderr: str) -> None:
    # We're not sure which order these will appear.
    assert r"root//:toolchain_cycle_top" in stderr
    # toolchain_cycle_mid is the toolchain rule and it doesn't appear in the error. ideally we'd fix that, but
    # for performance/memory reasons we aggregate the exec_deps out of toolchain rules.
    # assert r"root//:toolchain_cycle_mid" in stderr
    assert r"root//:toolchain_cycle_bot" in stderr
    assert r"Resolving execution platform" in stderr


# It's better to fail a test than to hit our test timeout. When cycle detection is not working, buck will just hang. So wrap these in a timeout.
async def expect_cycle(
    process: Awaitable[BuckResult],
) -> BuckException:
    return await asyncio.wait_for(expect_failure(process), timeout=200)


@buck_test()
async def test_detect_load_cycle(buck: Buck) -> None:
    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.load=yes",
        ),
    )
    check_load_cycle_stderr(failure.stderr)


@buck_test()
async def test_detect_configured_graph_cycles(buck: Buck) -> None:
    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )
    check_cfg_graph_cycle_stderr(failure.stderr)


@buck_test()
async def test_detect_configured_graph_cycles_on_recompute(buck: Buck) -> None:
    await buck.cquery("//:top")

    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )

    check_cfg_graph_cycle_stderr(failure.stderr)


@buck_test()
async def test_detect_configured_graph_cycles_2(buck: Buck) -> None:
    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_toolchain=yes",
        ),
    )
    check_cfg_toolchain_graph_cycle_stderr(failure.stderr)


@buck_test()
async def test_more_recompute_cases(buck: Buck) -> None:
    await buck.cquery("//:top")

    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.load=yes",
        ),
    )
    check_load_cycle_stderr(failure.stderr)

    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )
    check_cfg_graph_cycle_stderr(failure.stderr)
