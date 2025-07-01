# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_select_refine(buck: Buck) -> None:
    # Smoke test for select refinement:
    # the most specific option is picked even if it is not listed first.
    out = await buck.cquery(
        "--target-platforms=//:p-good-domestic",
        "-a=labels",
        "//:the-test",
    )
    q = json.loads(out.stdout)
    assert len(q) == 1
    assert list(q.values())[0]["labels"] == ["good-domestic"]
