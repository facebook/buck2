# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test()
async def test_ctargets_skip_missing_targets(buck: Buck) -> None:
    await expect_failure(
        buck.ctargets(
            "root//:existing",
            "root//:nonexistent",
            "--target-platforms=root//:p",
        ),
        stderr_regex="Unknown target `nonexistent` from package",
    )

    result = await buck.ctargets(
        "root//:existing",
        "root//:nonexistent",
        "--target-platforms=root//:p",
        "--skip-missing-targets",
    )
    [line] = result.stdout.splitlines()
    line = _replace_hash(line)
    assert line == "root//:existing (root//:p#<HASH>)"

    assert "Skipped 1 missing targets:" in result.stderr
    assert "root//:nonexistent" in result.stderr
