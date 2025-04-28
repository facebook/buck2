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
async def test_testsof(buck: Buck) -> None:
    await buck.bxl(
        "//:lazy_uquery.bxl:tests_of",
    )


@buck_test()
async def test_testsof_fail(buck: Buck) -> None:
    await buck.bxl(
        "//:lazy_uquery.bxl:tests_of_fail",
    )
