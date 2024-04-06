# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


# This is just a template test case for `bxl_test` to use buck2's e2e test framework.
# It does not need to be edited for new `bxl_test`.


@buck_test(inplace=True)
async def test_bxl(buck: Buck) -> None:
    await buck.bxl(os.environ["BXL_MAIN"])
