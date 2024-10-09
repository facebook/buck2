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


@buck_test(data_dir="deprecated_correct")
async def test_owner_without_universe_correct(buck: Buck) -> None:
    # TODO(nga): there should be a warning.
    result = await buck.cquery(
        "owner(bin.sh)",
    )
    assert "" == result.stdout
    assert (
        "Query has no target literals and `--target-universe` is not specified"
        in result.stderr
    )


@buck_test(data_dir="deprecated_correct")
async def test_owner_with_auto_universe_correct(buck: Buck) -> None:
    result = await buck.cquery(
        "deps(//:test) intersect owner(bin.sh)",
    )
    lines = result.stdout.splitlines()
    # Drop configuration.
    targets = [t.split()[0] for t in lines]
    assert ["root//:bin"] == targets
