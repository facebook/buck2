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
async def test_build_artifact(buck: Buck) -> None:
    res = await buck.bxl(
        "//:lazy_build_artifact.bxl:build_artifact",
    )
    assert "foo.txt" in res.stdout
    assert "bar.txt" in res.stdout


@buck_test()
async def test_build_artifact_catch_error(buck: Buck) -> None:
    res = await buck.bxl(
        "//:lazy_build_artifact.bxl:build_artifact_fail",
    )
    assert "foo.txt" in res.stdout


@buck_test()
async def test_cannot_build_dynmiac_action_output(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(
            "//:lazy_build_artifact.bxl:dynamic",
        ),
        stderr_regex="does not accept declared artifact",
    )


@buck_test()
async def test_cannot_bxl_action_output(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(
            "//:lazy_build_artifact.bxl:bxl_action_output",
        ),
        stderr_regex="does not accept declared artifact",
    )
