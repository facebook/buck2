# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


# Test select works with buckconfig.
@buck_test(inplace=True)
async def test_select_buckconfig(buck: Buck) -> None:
    out = await buck.cquery(
        "fbcode//buck2/tests/targets/configurations/select_buckconfig:the-test",
        "--output-attribute=cmd",
    )
    q = json.loads(out.stdout)
    assert len(q) == 1
    assert list(q.values())[0]["cmd"] == "NO"

    out = await buck.cquery(
        "fbcode//buck2/tests/targets/configurations/select_buckconfig:the-test",
        "--output-attribute=cmd",
        "-c",
        "aaa.bbb=ccc",
    )
    q = json.loads(out.stdout)
    assert len(q) == 1
    assert list(q.values())[0]["cmd"] == "YES"
