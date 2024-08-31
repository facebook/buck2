# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_target_platforms_arg(buck: Buck) -> None:
    out = await buck.cquery(
        # Specifying platform without cell to make sure it is resolved against current cell
        "--target-platforms=//buck2/tests/targets/configurations/target_platforms_arg:p-clouds",
        "deps(fbcode//buck2/tests/targets/configurations/target_platforms_arg:the-test, 1)",
    )
    out.check_returncode()
    stdout = re.sub(":p-clouds#[a-f0-9]+\\)", ":p-clouds#HASH)", out.stdout)
    assert (
        stdout
        == """\
fbcode//buck2/tests/targets/configurations/target_platforms_arg:the-test (fbcode//buck2/tests/targets/configurations/target_platforms_arg:p-clouds#HASH)
fbcode//buck2/tests/targets/configurations/target_platforms_arg:t-clouds (fbcode//buck2/tests/targets/configurations/target_platforms_arg:p-clouds#HASH)
"""
    )
