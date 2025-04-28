# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os.path
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_chrome_trace(buck: Buck, tmp_path: Path) -> None:
    # Just check it at least runs.
    await buck.build("//...")
    await buck.debug("chrome-trace", "--trace-path", str(tmp_path / "trace.json"))


@buck_test()
async def test_chrome_trace_no_repo(buck: Buck, tmp_path: Path) -> None:
    # Check that it runs from a path that is not in the repo.
    await buck.build("//...")
    log_path = (await buck.log("last")).stdout.strip()
    await buck.debug(
        "chrome-trace",
        "--trace-path",
        str(tmp_path / "trace.json"),
        "--path",
        log_path,
        rel_cwd=Path(os.path.relpath("/", buck.cwd)),
    )
