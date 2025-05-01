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


@buck_test(data_dir="sorted")
async def test_listed_providers_are_sorted(buck: Buck) -> None:
    result = await buck.audit("providers", "//:target", "--list")

    # "  - DefaultInfo" -> "DefaultInfo"
    providers = [
        line.split("-")[1].strip()
        for line in result.stdout.split("\n")
        if line.strip().startswith("-")
    ]
    assert providers == [
        "AlphaInfo",
        "DefaultInfo",
        "ZetaInfo",
    ]


@buck_test(data_dir="universe")
async def test_audit_providers_universe(buck: Buck) -> None:
    result = await buck.audit("providers", "//:aaa", "--quiet")
    assert "root//:aaa (root//:p-aaa#<HASH>)" == _replace_hash(result.stdout.strip())

    result = await buck.audit(
        "providers", "//:aaa", "--target-universe=//:bbb", "--quiet"
    )
    assert "root//:aaa (root//:p-bbb#<HASH>)" == _replace_hash(result.stdout.strip())
