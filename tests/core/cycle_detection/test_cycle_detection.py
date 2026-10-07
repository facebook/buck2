# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import re
from typing import Awaitable

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, BuckResult
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


# It's better to fail a test than to hit our test timeout. When cycle detection is not working, buck will just hang. So wrap these in a timeout.
async def expect_cycle(
    process: Awaitable[BuckResult],
) -> BuckException:
    return await asyncio.wait_for(expect_failure(process), timeout=200)


# A cycle is only reported once the command has stalled on it, at times for long enough that the
# client starts reporting on the wait. Those lines come and go with the timing, and carry resource
# usage figures besides.
_WAIT_REPORT_LINE = re.compile(
    r"^\[<TIMESTAMP>\] (Waiting on buck2 daemon|Resource usage:|IO:).*\n", re.MULTILINE
)


def check_cycle_error(failure: BuckException, name: str) -> None:
    golden(
        output=_WAIT_REPORT_LINE.sub("", sanitize_stderr(failure.stderr)),
        rel_path=f"golden/{name}.golden.txt",
    )


@buck_test()
async def test_detect_load_cycle(buck: Buck) -> None:
    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.load=yes",
        ),
    )
    check_cycle_error(failure, "load_cycle")


@buck_test()
async def test_detect_configured_graph_cycles(buck: Buck) -> None:
    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )
    check_cycle_error(failure, "configured_graph_cycle")


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

    check_cycle_error(failure, "configured_graph_cycle_on_recompute")


@buck_test()
async def test_detect_configured_graph_cycles_2(buck: Buck) -> None:
    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_toolchain=yes",
        ),
    )
    check_cycle_error(failure, "configured_toolchain_cycle")


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
    check_cycle_error(failure, "more_recompute_cases_load")

    failure = await expect_cycle(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )
    check_cycle_error(failure, "more_recompute_cases_cfg_graph")
