# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class LazyUtilsTest(BuckTestCase):
    @buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
    async def test_catch_resolve_lazy_dict(self, buck: Buck) -> None:
        await buck.bxl(
            "//bxl/utils:test_lazy_utils.bxl:test_catch_resolve_lazy_dict",
        )

    @buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
    async def test_batch_apply_lazy(self, buck: Buck) -> None:
        await buck.bxl(
            "//bxl/utils:test_lazy_utils.bxl:test_batch_apply_lazy",
        )

    @buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
    async def test_batch_apply_lazy_catch_each(self, buck: Buck) -> None:
        await buck.bxl(
            "//bxl/utils:test_lazy_utils.bxl:test_batch_apply_lazy_catch_each",
        )

    @buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
    async def test_batch_apply_lazy_catch_all(self, buck: Buck) -> None:
        await buck.bxl(
            "//bxl/utils:test_lazy_utils.bxl:test_batch_apply_lazy_catch_all"
        )

    @buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
    async def test_partition_results(self, buck: Buck) -> None:
        await buck.bxl("//bxl/utils:test_lazy_utils.bxl:test_partition_results")

    @buck_test(inplace=False, data_dir="bxl/simple", skip_for_os=["windows"])
    async def test_partition_results_dict(self, buck: Buck) -> None:
        await buck.bxl(
            "//bxl/utils:test_lazy_utils.bxl:test_partition_results_dict",
        )

    # dummy test to avoid test listing failure on windows
    @buck_test(inplace=True)
    async def test_dummy(self, buck: Buck) -> None:
        pass
