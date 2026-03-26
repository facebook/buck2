# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
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
async def test_bxl_target_universe_keep_going_list_input(buck: Buck) -> None:
    await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_list_input",
    )


@buck_test()
async def test_bxl_target_universe_keep_going_target_set_input(buck: Buck) -> None:
    await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_target_set_input",
    )


@buck_test()
async def test_bxl_target_universe_keep_going_mixed_list(buck: Buck) -> None:
    await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_mixed_list",
    )


@buck_test()
async def test_bxl_target_universe_keep_going_all_fail(buck: Buck) -> None:
    await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_all_fail",
    )


@buck_test()
async def test_bxl_target_universe_keep_going_incompatible_target_set(
    buck: Buck,
) -> None:
    result = await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_incompatible_target_set",
    )
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible_target" in result.stderr


@buck_test()
async def test_bxl_target_universe_keep_going_incompatible_string_pattern(
    buck: Buck,
) -> None:
    result = await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_incompatible_string_pattern",
    )
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible_target" in result.stderr


@buck_test()
async def test_bxl_target_universe_keep_going_incompatible_list(
    buck: Buck,
) -> None:
    result = await buck.bxl(
        "//keep_going.bxl:target_universe_keep_going_incompatible_list",
    )
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible_target" in result.stderr
