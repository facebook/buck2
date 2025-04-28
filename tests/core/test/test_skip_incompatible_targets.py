#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


@buck_test()
@env(
    "BUCK2_ALLOW_INTERNAL_TEST_RUNNER_DO_NOT_USE", "1"
)  # needed to avoid failure on missing buck2-tpx in buck-out
async def test_test_skip_incompatible_targets(buck: Buck) -> None:
    targetA = "root//:compatible-with-A"
    targetB = "root//:compatible-with-B"
    platformA = "root//:platA"

    await expect_failure(
        buck.test(
            targetA,
            targetB,
            f"--target-platforms={platformA}",
            test_executor="",
        ),
        stderr_regex=rf"{targetB}\s*is incompatible with {platformA}#.*$",
    )

    result = await buck.test(
        targetA,
        targetB,
        f"--target-platforms={platformA}",
        "--skip-incompatible-targets",
        test_executor="",
    )
    assert f"Skipping target incompatible node `{targetB}" in result.stderr

    result.check_returncode()
