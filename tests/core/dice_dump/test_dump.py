# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import gzip
import os.path
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_debug_legacy_dice_dump(buck: Buck, tmp_path: Path) -> None:
    file_path = tmp_path / "dump"

    await buck.uquery("//...")
    await buck.debug("dice-dump", "--path", str(file_path))

    assert os.path.exists(f"{file_path}/nodes.gz")
    assert os.path.exists(f"{file_path}/edges.gz")
    assert os.path.exists(f"{file_path}/nodes_currently_running.gz")

    nodes = gzip.open(f"{file_path}/nodes.gz", "r").read().decode()
    assert "BuildDataKey" in nodes
    assert "FileOpsKey" in nodes

    edges = gzip.open(f"{file_path}/edges.gz", "r").read().decode()
    print(edges)
    assert edges  # check not empty

    nodes_currently_running = (
        gzip.open(f"{file_path}/nodes_currently_running.gz", "r").read().decode()
    )
    print(nodes_currently_running)
    assert nodes_currently_running == ""
