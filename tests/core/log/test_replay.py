# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_replay(buck: Buck) -> None:
    await buck.build("//:EEE")
    replay = await buck.log("replay", "-v2")
    assert "//:EEE" in replay.stderr


@buck_test()
async def test_partial_result_replay(buck: Buck) -> None:
    # `audit cell` is an easy way to produce partial results
    res = await buck.audit("cell")
    res2 = await buck.log("replay")

    assert res.stdout != res2.stdout
    assert res2.stdout == ""
