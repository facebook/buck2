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


class TypeNamesAndSymbolsTest(BuckTestCase):
    @buck_test()
    async def test_cquery_ctx(self, buck: Buck) -> None:
        await buck.bxl("//bxl/check_type_names_and_symbols.bxl:cquery_ctx")

    @buck_test()
    async def test_target_set(self, buck: Buck) -> None:
        await buck.bxl("//bxl/check_type_names_and_symbols.bxl:target_set")

    @buck_test()
    async def test_target_set_syntax(self, buck: Buck) -> None:
        await buck.bxl("//bxl/check_type_names_and_symbols.bxl:target_set_syntax")

    @buck_test()
    async def test_target_set_unsupported_node_type(self, buck: Buck) -> None:
        result = await expect_failure(
            buck.bxl(
                "//bxl/check_type_names_and_symbols.bxl:target_set_unsupported_node_type"
            )
        )
        assert "`bxl.TargetSet` expects one of" in result.stderr
        assert "got `int`" in result.stderr
