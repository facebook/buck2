# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_catch_resolve_lazy_dict(buck: Buck) -> None:
    await buck.bxl(
        "fbcode//buck2/tests/e2e/bxl/utils/test_lazy_utils.bxl:test_catch_resolve_lazy_dict",
    )


@buck_test(inplace=True)
async def test_batch_apply_lazy(buck: Buck) -> None:
    await buck.bxl(
        "fbcode//buck2/tests/e2e/bxl/utils/test_lazy_utils.bxl:test_batch_apply_lazy",
    )


@buck_test(inplace=True)
async def test_batch_apply_lazy_catch_each(buck: Buck) -> None:
    await buck.bxl(
        "fbcode//buck2/tests/e2e/bxl/utils/test_lazy_utils.bxl:test_batch_apply_lazy_catch_each",
    )
