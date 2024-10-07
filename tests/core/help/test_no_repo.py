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


@buck_test(skip_final_kill=True)
async def test_no_repo(buck: Buck, tmp_path: Path) -> None:
    await buck.help()
    # And make sure this also works with absolute argfiles
    arg_path = tmp_path / "argsfile.txt"
    arg_path.write_text("--help", encoding="utf-8")
    await buck.run_buck_command(f"@{arg_path}")
