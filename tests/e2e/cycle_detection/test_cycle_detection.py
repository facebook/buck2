# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
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


@buck_test(inplace=False)
async def test_detect_load_cycle(buck: Buck) -> None:
    failure = await expect_failure(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.load=yes",
        ),
    )
    check_load_cycle_stderr(failure.stderr)


@buck_test(inplace=False)
async def test_detect_configured_graph_cycles(buck: Buck) -> None:
    failure = await expect_failure(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )
    check_cfg_graph_cycle_stderr(failure.stderr)


@buck_test(inplace=False)
async def test_detect_configured_graph_cycles_on_recompute(buck: Buck) -> None:
    await buck.cquery("//:top")

    failure = await expect_failure(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )

    check_cfg_graph_cycle_stderr(failure.stderr)


@buck_test(inplace=False)
async def test_more_recompute_cases(buck: Buck) -> None:
    await buck.cquery("//:top")

    failure = await expect_failure(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.load=yes",
        ),
    )
    check_load_cycle_stderr(failure.stderr)

    failure = await expect_failure(
        buck.cquery(
            "//:top",
            "-c",
            "cycles.cfg_graph=yes",
        ),
    )
    check_cfg_graph_cycle_stderr(failure.stderr)
