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


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test()
async def test_ctargets_transition(buck: Buck) -> None:
    # This target does self-transition, and `ctargets` outputs both
    # forward node and forward target node.

    result = await buck.ctargets(
        "root//:candy",
        "--target-platforms=root//:p",
    )
    [line1, line2] = result.stdout.splitlines()
    line1 = _replace_hash(line1)
    line2 = _replace_hash(line2)
    assert [line1, line2] == [
        "root//:candy (root//:p#<HASH>)",
        "root//:candy (<clay>#<HASH>)",
    ]
