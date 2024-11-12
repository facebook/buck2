# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_unconfigured_target_hashing(
    buck: Buck,
) -> None:
    await assert_hashes(buck, ":foo", "foo.txt", False)
    await assert_hashes(buck, ":foo", "bar.txt", True)
    await assert_hashes(buck, ":foo_dep", "foo.txt", False)
    await assert_hashes(buck, ":foo_dep", "bar.txt", True)
    await assert_hashes(buck, ":none", "bar.txt", True)


async def assert_hashes(
    buck: Buck, target: str, modified_path: str, same_hash: bool
) -> None:
    result = await buck.targets(
        target,
        "--show-unconfigured-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
    )

    modified_result = await buck.targets(
        target,
        "--show-unconfigured-target-hash",
        "--json",
        "--target-hash-file-mode",
        "PATHS_ONLY",
        "--target-hash-modified-paths",
        modified_path,
    )
    output = json.loads(result.stdout)
    modified_output = json.loads(modified_result.stdout)

    # Hash should change if modified path belongs to target or to any of its dependencies
    if same_hash:
        assert output[0]["buck.target_hash"] == modified_output[0]["buck.target_hash"]
    else:
        assert output[0]["buck.target_hash"] != modified_output[0]["buck.target_hash"]
