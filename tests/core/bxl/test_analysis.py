# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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
        "Dependency",
        "root//:stub (<unspecified>)",
    ]


@buck_test(write_invocation_record=True)
async def test_bxl_analysis_missing_subtarget(buck: Buck) -> None:
    res = await expect_failure(
        buck.bxl(
            "//analysis.bxl:missing_subtarget_test",
        ),
        stderr_regex="requested sub target named `missing_subtarget` .* is not available",
    )

    record = res.invocation_record()
    errors = record["errors"]

    assert len(errors) == 1
    assert errors[0]["category"] == "USER"


@buck_test()
async def test_bxl_analysis_unconfigured_target_error(buck: Buck) -> None:
    await expect_failure(
        buck.bxl("//analysis.bxl:unconfigured_target_error_test"),
        stderr_regex="Type of parameter `labels` doesn't match",
    )


@buck_test()
async def test_bxl_analysis_subtarget_order(buck: Buck) -> None:
    # More labels than one prepare batch, so results are reassembled across batches.
    result = await buck.bxl("//analysis.bxl:ordered_subtargets_test")
    assert result.stdout.splitlines() == [f"child_{i}" for i in range(129, -1, -1)]


@buck_test()
async def test_bxl_analysis_skip_incompatible(buck: Buck) -> None:
    result = await buck.bxl(
        "//analysis.bxl:incompatible_skip_test",
        "--",
        "--target",
        "root//:incompatible_target",
    )
    assert result.stdout.splitlines() == ["None", "0", "provides_foo_foo"]
    assert "incompatible_target" in result.stderr


@buck_test()
async def test_bxl_analysis_reject_incompatible(buck: Buck) -> None:
    for entry in ["incompatible_error_test", "incompatible_list_error_test"]:
        await expect_failure(
            buck.bxl(
                f"//analysis.bxl:{entry}",
                "--",
                "--target",
                "root//:incompatible_target",
            ),
            stderr_regex="incompatible_target.*is incompatible",
        )
