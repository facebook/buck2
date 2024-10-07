# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
import re
from pathlib import Path
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test


def _includes(output: BuckResult) -> List[str]:
    return sorted(
        [
            re.sub(".*[/\\\\]", "", line)
            for line in output.stdout.splitlines()
            if line.endswith(".bzl")
        ]
    )


@buck_test()
async def test_audit_includes(buck: Buck, tmp_path: Path) -> None:
    # Using project relative path.
    output = await buck.audit("includes", "TARGETS.fixture")
    assert _includes(output) == ["incl.bzl", "prelude.bzl"]

    # Using project relative path when in a subdirectory.
    await buck.audit("includes", "TARGETS.fixture", rel_cwd=Path("dir"))
    assert _includes(output) == ["incl.bzl", "prelude.bzl"]

    # Using absolute path.
    output = await buck.audit("includes", f"{buck.cwd}/TARGETS.fixture")
    assert _includes(output) == ["incl.bzl", "prelude.bzl"]

    if os.name != "nt":
        # Create symlink to the project root in a temporary directory.
        (tmp_path / "symlink").symlink_to(buck.cwd)

        output = await buck.audit("includes", f"{tmp_path}/symlink/TARGETS.fixture")
        assert _includes(output) == ["incl.bzl", "prelude.bzl"]
