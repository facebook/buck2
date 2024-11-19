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
async def test_cquery_filter_should_not_include_configuration(buck: Buck) -> None:
    # First, self-check.
    result = await buck.cquery("//...")
    assert [
        "root//:aaaaa (<unbound>)",
        "root//:bbbbb (root//:aaaaa#<HASH>)",
    ] == _replace_hash(result.stdout).splitlines()

    # Now check the behavior of `filter()`.
    # `filter()` function checks unconfigured target label, as Buck1 does.
    result = await buck.cquery(r"filter('^root//:bbbbb$', //...)")
    assert [
        "root//:bbbbb (root//:aaaaa#<HASH>)",
    ] == _replace_hash(result.stdout).splitlines()
