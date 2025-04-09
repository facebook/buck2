# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_target_platforms_arg(buck: Buck) -> None:
    out = await buck.cquery(
        # Specifying platform without cell to make sure it is resolved against current cell
        "--target-platforms=//:p-clouds",
        "deps(//:the-test, 1)",
        rel_cwd=Path("subcell"),
    )
    stdout = re.sub(":p-clouds#[a-f0-9]+\\)", ":p-clouds#HASH)", out.stdout)
    assert (
        stdout
        == """\
subcell//:the-test (subcell//:p-clouds#HASH)
subcell//:t-clouds (subcell//:p-clouds#HASH)
"""
    )
