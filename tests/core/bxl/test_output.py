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
async def test_bxl_caching(buck: Buck) -> None:
    result = await buck.bxl(
        "//caching.bxl:print_caching",
    )

    assert "ran me" in result.stderr
    assert "result print" in result.stdout

    result = await buck.bxl(
        "//caching.bxl:print_caching",
    )

    assert "ran me" not in result.stderr
    assert "result print" in result.stdout


@buck_test()
async def test_bxl_caching_with_target_platforms_specified(buck: Buck) -> None:
    # run with platform1, result should be cached afterwards
    result = await buck.bxl(
        "//caching.bxl:caching_with_target_platforms",
        "--target-platforms",
        "root//:platform1",
    )

    assert "ran me" in result.stderr
    assert "root//:platform1" in result.stdout

    # run with platform2, DICE should be invalidated and updated results should be
    # cached afterwards
    result = await buck.bxl(
        "//caching.bxl:caching_with_target_platforms",
        "--target-platforms",
        "root//:platform2",
    )

    assert "ran me" in result.stderr
    assert "root//:platform2" in result.stdout

    # run with platform1 again, we should already have cached results
    result = await buck.bxl(
        "//caching.bxl:caching_with_target_platforms",
        "--target-platforms",
        "root//:platform1",
    )

    assert "ran me" not in result.stderr
    assert "root//:platform1" in result.stdout


@buck_test()
async def test_bxl_error_caching(buck: Buck) -> None:
    result = await buck.bxl("//caching.bxl:print_error_caching")
    assert "ran me" in result.stderr
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//:incompatible" in result.stderr

    # output stream that writes to stderr should be cached, but regular stdlib print
    # statements (which also write to stderr) will not be cached.
    result = await buck.bxl("//caching.bxl:print_error_caching")
    assert "ran me" not in result.stderr
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//:incompatible" in result.stderr


@buck_test()
async def test_bxl_print_with_no_buckd(buck: Buck) -> None:
    result = await buck.bxl(
        "//caching.bxl:print_caching",
        "--no-buckd",
    )

    assert "ran me" in result.stderr
    assert "result print" in result.stdout
