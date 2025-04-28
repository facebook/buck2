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


@buck_test(data_dir="simple")
async def test_query_owner(buck: Buck) -> None:
    result = await buck.cquery(
        "--target-universe=root//bin:the_binary", """owner(bin/TARGETS.fixture)"""
    )
    assert (
        _replace_hash(result.stdout)
        == "root//bin:the_binary (root//platforms:platform1#<HASH>)\n"
    )
