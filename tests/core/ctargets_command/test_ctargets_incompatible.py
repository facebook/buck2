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
async def test_ctargets_incompatible(buck: Buck) -> None:
    result = await buck.ctargets(
        # This one will be omitted from the output because it is not compatible.
        "root//:triangle",
        # This one will be output.
        "root//:square",
        "--target-platforms=root//:rectangular",
    )
    stdout = _replace_hash(result.stdout)
    [line] = stdout.splitlines()
    assert line == "root//:square (root//:rectangular#<HASH>)"

    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//:triangle (root//:rectangular#" in result.stderr
