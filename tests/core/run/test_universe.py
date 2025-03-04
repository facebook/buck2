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
async def test_run_executable(buck: Buck) -> None:
    result = await buck.run("root//:print_animal_hello")
    assert result.stdout.strip() == "hello dog"

    result = await buck.run(
        "root//:print_animal_hello", "--target-universe", "root//:cat_universe"
    )
    assert result.stdout.strip() == "hello cat"


@buck_test()
async def test_run_with_transition_without_target_universe(buck: Buck) -> None:
    result = await buck.run(
        "root//:buck",
        "--target-platforms=root//:p_cat",
    )

    # The transition (deliberately) loses the configuration so that we get the
    # DEFAULT 'hello buck' from the select in the target definition.
    assert result.stdout.strip() == "hello buck"


@buck_test()
async def test_run_with_transition_with_target_universe(buck: Buck) -> None:
    result = await buck.run(
        "root//:buck",
        "--target-platforms=root//:p_cat",
        "--target-universe",
        "root//:buck",
    )

    # The transition (deliberately) loses the configuration so that we get the
    # DEFAULT 'hello buck' from the select in the target definition.
    assert result.stdout.strip() == "hello buck"
