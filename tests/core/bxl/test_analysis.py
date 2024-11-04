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
async def test_bxl_analysis(buck: Buck) -> None:
    result = await buck.bxl(
        "//analysis.bxl:providers_test",
    )

    lines = result.stdout.splitlines()
    assert "provides_foo_foo" in lines[0]
    assert "provides_foo_foo" in lines[1]

    result = await buck.bxl(
        "//analysis.bxl:dependency_test",
    )

    assert result.stdout.splitlines() == [
        "dependency",
        "root//:stub (<unspecified>)",
    ]


@buck_test(allow_soft_errors=True)
async def test_bxl_analysis_incompatible_targets_list(buck: Buck) -> None:
    # multiple incompatible targets should be skipped and the analysis should return empty dict
    result = await buck.bxl("//analysis.bxl:incompatible_targets")
    assert "Skipping target incompatible node" in result.stderr
    assert "root//:incompatible_target" in result.stderr
    assert "{}" == result.stdout.strip()


@buck_test(allow_soft_errors=True)
async def test_bxl_analysis_incompatible_targets_single(buck: Buck) -> None:
    # single incompatible target should be skipped and the analysis should return None
    result = await buck.bxl("//analysis.bxl:incompatible_targets_single")
    assert "Skipping target incompatible node" in result.stderr
    assert "root//:incompatible_target" in result.stderr
    assert "None" == result.stdout.strip()


@buck_test()
async def test_bxl_analysis_missing_subtarget(buck: Buck) -> None:
    await expect_failure(
        buck.bxl("//analysis.bxl:missing_subtarget_test"),
        stderr_regex="requested sub target named `missing_subtarget` .* is not available",
    )
