# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import os
import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.buck_workspace import buck_test


def _includes(output: BuckResult) -> list[str]:
    return sorted(
        [
            re.sub(".*[/\\\\]", "", line)
            for line in output.stdout.splitlines()
            if line.endswith(".bzl") or line.endswith(".json")
        ]
    )


@buck_test()
async def test_audit_includes(buck: Buck, tmp_path: Path) -> None:
    expected_includes = ["example.json", "incl.bzl", "prelude.bzl"]
    # Using project relative path.
    output = await buck.audit("includes", "TARGETS.fixture")
    assert _includes(output) == expected_includes

    # Using project relative path when in a subdirectory.
    await buck.audit("includes", "TARGETS.fixture", rel_cwd=Path("dir"))
    assert _includes(output) == expected_includes

    # Using absolute path.
    output = await buck.audit("includes", f"{buck.cwd}/TARGETS.fixture")
    assert _includes(output) == expected_includes

    if os.name != "nt":
        # Create symlink to the project root in a temporary directory.
        (tmp_path / "symlink").symlink_to(buck.cwd)

        output = await buck.audit("includes", f"{tmp_path}/symlink/TARGETS.fixture")
        assert _includes(output) == expected_includes
