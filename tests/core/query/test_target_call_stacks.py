# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_target_call_stacks_default(buck: Buck) -> None:
    result = await buck.uquery(
        "--stack",
        "root//:test",
    )
    golden(
        output=result.stdout,
        rel_path="golden/uquery.stdout",
    )
    result = await buck.cquery(
        "--stack",
        "root//:test",
    )
    golden(
        output=result.stdout,
        rel_path="golden/cquery.stdout",
    )
