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


@buck_test()
async def test_cell_ordering(buck: Buck) -> None:
    res = await buck.audit("cell")
    # The repository should be in the list, not the alias
    assert "source:" in res.stdout
    assert "a:" not in res.stdout
    assert "z:" not in res.stdout

    res = await buck.audit("cell", "--aliases")
    assert "source:" in res.stdout
    assert "a:" in res.stdout
    assert "z:" in res.stdout


@buck_test()
async def test_bxl_audit_cell(buck: Buck) -> None:
    result = await buck.bxl("//test_audit.bxl:audit_cell")

    # specify single cell
    outputs = result.stdout.splitlines()
    single_result = json.loads(outputs[0])
    assert single_result["source"] == str(buck.cwd / "fbs")

    # don't specify cell - should return all cell aliases
    all_result = json.loads(outputs[1])
    assert all_result["a"] == str(buck.cwd / "fbs")
    assert all_result["z"] == str(buck.cwd / "fbc")
    assert all_result["code"] == str(buck.cwd / "fbc")
    assert all_result["source"] == str(buck.cwd / "fbs")
