# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import os
from pathlib import Path
from typing import Dict

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

output_cleanup_targets = [
    "local_action",
    "remote_action",
    "symlinked_dir",
    "write",
    "copy",
]


@buck_test(skip_for_os=["windows"])
@pytest.mark.parametrize(
    "materializations",
    [
        "deferred",
        "all",
    ],
)
@pytest.mark.parametrize(
    "first",
    output_cleanup_targets,
)
@pytest.mark.parametrize(
    "second",
    output_cleanup_targets,
)
async def test_output_cleanup(
    buck: Buck, tmp_path: Path, materializations: str, first: str, second: str
) -> None:
    def read_dir(d: Path) -> Dict[str, str]:
        steps = 0
        out = {}

        for root, dirs, files in os.walk(d, topdown=False):
            for name in files:
                path = os.path.join(root, name)
                out[os.path.relpath(path, d)] = open(path).read()
                steps += 1
            for name in dirs:
                path = os.path.join(root, name)
                out.setdefault(os.path.relpath(path, d), {})
                steps += 1

        return out

    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write(f"\n[buck2]\nmaterializations = {materializations}")
    await buck.kill()  # Ensure the config gets picked up

    rebuild = tmp_path / "rebuild"
    clean = tmp_path / "clean"

    first = f"{first}-a"
    second = f"{second}-b"

    await buck.build(":main", "-c", f"test.main={first}")
    await buck.build(":main", "-c", f"test.main={second}", "--out", str(rebuild))

    await buck.clean()
    await buck.build(":main", "-c", f"test.main={second}", "--out", str(clean))

    assert read_dir(rebuild) == read_dir(clean)


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test()
async def test_noop(buck: Buck) -> None:
    return
