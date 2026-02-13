# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import subprocess

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(setup_eden=True, write_invocation_record=True)
async def test_build_count_since_rebase(buck: Buck) -> None:
    # needed for mergebase to exist
    subprocess.run(["sl", "bookmark", "main"], cwd=buck.cwd, check=True)
    res = await buck.build(
        "//:test",
    )
    record = res.invocation_record()
    print(record["hg_revision"])
    assert record["min_attempted_build_count_since_rebase"] == 1
    assert record["min_build_count_since_rebase"] == 1

    res2 = await expect_failure(
        buck.build(
            "//:test",
            "-c test.fail=1",
        )
    )
    record = res2.invocation_record()
    print(record["hg_revision"])
    assert record["min_attempted_build_count_since_rebase"] == 2
    assert record["min_build_count_since_rebase"] == 1
