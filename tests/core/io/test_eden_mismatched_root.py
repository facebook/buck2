# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(
    setup_eden=True,
    # buck-out is not redirected in the subproject
    allow_soft_errors=True,
    # We don't run buck outside the subproject
    skip_final_kill=True,
)
async def test_mismatched_root(buck: Buck) -> None:
    await buck.build("//...", rel_cwd=Path("subdir"))
    await buck.kill(rel_cwd=Path("subdir"))
