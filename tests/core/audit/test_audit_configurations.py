# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


def _parse_audit_configurations(output: str) -> List[str]:
    return [x.rstrip(":") for x in output.splitlines() if not x.startswith(" ")]


@buck_test()
async def test_audit_configurations_all(buck: Buck) -> None:
    # Evaluate a target to make sure configuration is loaded.
    await buck.cquery("//:genrule")

    result = await buck.audit("configurations")
    configurations = _parse_audit_configurations(result.stdout)
    configurations = [_replace_hash(x) for x in configurations]
    assert "root//:p#<HASH>" in configurations


@buck_test()
async def test_audit_configurations_specific(buck: Buck) -> None:
    # Evaluate a target to make sure configuration is loaded.
    await buck.cquery("//:genrule")

    # Load configurations so we can learn the hash.
    result = await buck.audit("configurations")
    configurations = _parse_audit_configurations(result.stdout)
    [configuration] = [c for c in configurations if c.startswith("root//:p#")]

    # Now audit the specific configuration.
    result = await buck.audit("configurations", configuration)
    assert [configuration] == _parse_audit_configurations(result.stdout)
