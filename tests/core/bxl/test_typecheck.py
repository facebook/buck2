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
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class TypecheckTest(BuckTestCase):
    @buck_test()
    async def test_not_bxl(self, buck: Buck) -> None:
        result = await expect_failure(
            buck.bxl(
                "//:test_typecheck.bxl:main",
            )
        )
        assert "Expected type `int` but got `str`" in result.stderr

    @buck_test()
    async def test_target_set_element_type(self, buck: Buck) -> None:
        result = await expect_failure(
            buck.bxl(
                "//:target_set_element.bxl:main",
            )
        )
        assert (
            "Expected type `bxl.UnconfiguredTargetNode` but got `bxl.ConfiguredTargetNode`"
            in result.stderr
        )

    @buck_test()
    async def test_target_set_query_args_type(self, buck: Buck) -> None:
        result = await expect_failure(
            buck.bxl(
                "//:target_set_query_args.bxl:main",
            )
        )
        assert "bxl.TargetSet[bxl.UnconfiguredTargetNode]" in result.stderr
        assert "bxl.TargetSet[bxl.ConfiguredTargetNode]" in result.stderr

    @buck_test()
    async def test_target_set_mixed_kinds(self, buck: Buck) -> None:
        result = await expect_failure(
            buck.bxl(
                "//:target_set_mixed_kinds.bxl:main",
            )
        )
        assert (
            "Binary operator `+` is not available on the types "
            "`bxl.TargetSet[bxl.ConfiguredTargetNode]` and `bxl.TargetSet[bxl.UnconfiguredTargetNode]`"
            in result.stderr
        )
