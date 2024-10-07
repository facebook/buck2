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
async def test_debug_eval_good(buck: Buck) -> None:
    await buck.debug(
        "eval",
        "./good.bzl",
        "./good.bxl",
    )


@buck_test()
async def test_debug_eval_bad_bzl(buck: Buck) -> None:
    await expect_failure(
        buck.debug(
            "eval",
            "./bad.bzl",
        ),
        stderr_regex="fail: bad bzl",
    )


@buck_test()
async def test_debug_eval_bad_bxl(buck: Buck) -> None:
    await expect_failure(
        buck.debug(
            "eval",
            "./bad.bxl",
        ),
        stderr_regex="fail: bad bxl",
    )
