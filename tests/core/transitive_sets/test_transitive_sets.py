# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_transitive_sets(buck: Buck) -> None:
    rule = "//:bar"
    report = await buck.build(rule)
    out = report.get_build_report().output_for_target(rule)
    out = out.read_text()
    out = [line.strip() for line in out.strip().split("\n")]
    assert out == ["bar", "foo", "foo2", "foo1"]


@buck_test()
async def test_transitive_set_deduplication(buck: Buck) -> None:
    await buck.build("//:test_duplication")
