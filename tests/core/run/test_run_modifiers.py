# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_run_single_modifier(buck: Buck) -> None:
    result = await buck.run("root//:run?root//:macos")

    [os, cpu] = result.stdout.strip().split()

    assert os == "macos"
    assert cpu == "DEFAULT"


@buck_test()
async def test_run_multiple_modifiers(buck: Buck) -> None:
    result = await buck.run("root//:run?root//:macos+root//:arm")

    [os, cpu] = result.stdout.strip().split()

    assert os == "macos"
    assert cpu == "arm"


@buck_test()
async def test_run_order_of_modifiers(buck: Buck) -> None:
    result = await buck.run("root//:run?root//:macos+root//:linux")

    [os, cpu] = result.stdout.strip().split()

    assert os == "linux"
    assert cpu == "DEFAULT"


@buck_test()
async def test_run_target_universe_single_modifier(buck: Buck) -> None:
    result = await buck.run(
        "root//:run",
        "--target-universe",
        "root//:run?root//:macos",
    )

    [os, cpu] = result.stdout.strip().split()

    assert os == "macos"
    assert cpu == "DEFAULT"


@buck_test()
async def test_run_target_universe_multiple_modifiers(buck: Buck) -> None:
    result = await buck.run(
        "root//:run",
        "--target-universe",
        "root//:run?root//:macos+root//:arm",
    )

    [os, cpu] = result.stdout.strip().split()

    assert os == "macos"
    assert cpu == "arm"


@buck_test()
async def test_run_fails_with_global_modifiers(buck: Buck) -> None:
    await expect_failure(
        buck.run(
            "--modifier",
            "root//:macos",
            "root//:run?root//:linux",
        ),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )

    await expect_failure(
        buck.run(
            "--modifier",
            "root//:macos",
            "root//:run",
            "--target-universe",
            "root//:run?root//:linux",
        ),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )


@buck_test()
async def test_run_fails_with_pattern_modifier_and_target_universe_modifier(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.run(
            "root//:run?root//:macos",
            "--target-universe",
            "root//:run?root//:arm",
        ),
        stderr_regex=r"Cannot use \?modifier syntax in target pattern expression with --target-universe flag",
    )
