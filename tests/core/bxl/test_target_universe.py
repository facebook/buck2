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


@buck_test()
async def test_bxl_target_universe_keep_going_no_errors(buck: Buck) -> None:
    await buck.bxl(
        "//target_universe.bxl:target_universe_keep_going_no_errors",
    )


@buck_test()
async def test_bxl_target_universe_universe_target_set(buck: Buck) -> None:
    await buck.bxl(
        "//target_universe.bxl:target_universe_universe_target_set",
    )


@buck_test()
async def test_bxl_target_universe_keep_going_with_errors(buck: Buck) -> None:
    await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_with_errors",
    )


@buck_test()
async def test_bxl_target_universe_keep_going_invalid_input(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(
            "//keep_going_invalid_input.bxl:invalid_input",
        ),
        stderr_regex="`keep_going` is currently only implemented for a single target pattern as a string literal",
    )
