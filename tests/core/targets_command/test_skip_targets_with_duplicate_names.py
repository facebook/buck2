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


@buck_test()
async def test_skip_targets_with_duplicate_names_without_flag(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//..."),
        stderr_regex="Attempted to register target prelude//:aa twice",
    )


@buck_test()
async def test_skip_targets_with_duplicate_names_with_flag(buck: Buck) -> None:
    result = await buck.targets("//...", "--skip-targets-with-duplicate-names")
    assert [
        "prelude//:aa",
        "prelude//:bb",
    ] == result.stdout.splitlines()
    assert re.search("Attempted to register target prelude//:aa twice", result.stderr)
    assert re.search("Attempted to register target prelude//:bb twice", result.stderr)
