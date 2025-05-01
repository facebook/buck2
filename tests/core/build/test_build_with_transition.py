# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_build_transition_without_target_universe(buck: Buck) -> None:
    result = await buck.build_without_report(
        "root//:buck",
        "--target-platforms=root//:p",
        "--show-output",
    )

    lines = result.stdout.splitlines()
    # Just a single target is built and output
    assert 1 == len(lines)
    assert "root//:buck buck-out" in lines[0]


@buck_test()
async def test_build_transition_with_target_universe(buck: Buck) -> None:
    result = await buck.build_without_report(
        "root//:buck",
        "--target-platforms=root//:p",
        "--target-universe",
        "root//:buck",
        "--show-output",
    )

    lines = result.stdout.splitlines()
    # Just a single target is built and output
    assert 1 == len(lines)
    assert "root//:buck buck-out" in lines[0]
