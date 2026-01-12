# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import csv
import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_what_materialized_csv(buck: Buck) -> None:
    await buck.build("//:my_rule")
    out = await buck.log("what-materialized", "--format", "csv")
    header = ["path", "method", "file_count", "total_bytes"]
    out = [
        dict(zip(header, record))
        for record in csv.reader(out.stdout.splitlines())
        if record
    ]
    assert len(out) > 0, "out should have some materializations"
    assert out[0] == dict(zip(header, header)), (
        "ensure that first entry in csv is the header"
    )
    assert any(line["path"].endswith("__my_rule__/out") for line in out), (
        "should have materialized main test file"
    )


@buck_test()
async def test_what_materialized_sorted(buck: Buck) -> None:
    await buck.build("//:my_rule")
    out = await buck.log("what-materialized", "--format", "json", "--sort-by-size")
    out = [json.loads(line) for line in out.stdout.splitlines() if line]
    assert len(out) > 0, "out should have some materializations"
    assert all(
        out[i]["total_bytes"] <= out[i + 1]["total_bytes"] for i in range(len(out) - 1)
    ), "should be sorted by size"


@buck_test()
async def test_what_materialized_aggregated(buck: Buck) -> None:
    await buck.build("//:my_rule")
    # buck2 log what-materialized --aggregate-by-ext has the following output:
    # <empty>	cas	1	1
    out = await buck.log("what-materialized", "--aggregate-by-ext")
    out = [line.split() for line in out.stdout.splitlines() if line]
    assert len(out) > 0, "out should have some materializations"
    assert out[0][0] == "<empty>"
