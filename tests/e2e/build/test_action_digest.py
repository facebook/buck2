# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform
from buck2.tests.e2e_util.helper.utils import read_what_ran


# If this test fails, it means that a change that modifies action digest was made.
# Background in this post:
# https://fb.workplace.com/groups/buck2eng/permalink/3452581371706005/
# Changes should instead be deployed by:
#   1: Create a new buck2 flag and hide the changes behind it (Ex. D59503359)
#   2: Wait for bvb that contains #1 to land
#   3: Activate the flag via .buckconfig (Ex. D59648609)
#       3.1: Fix/followup on any CI failures caused by cache invalidation
#   4: Observe for a couple of days to ensure that there are no issues
#   5. Remove the code associated with the config flag but NOT the config itself,
#      this way this test wouldn't need to be changed at all (Ex. D59864942)
#   6: Wait for bvb that contains #5 to land
#   7: Remove the config flag (Ex. D59988979)
@buck_test(inplace=True)
async def test_action_digest(buck: Buck) -> None:
    await buck.build(
        get_mode_from_platform(),
        "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        "--remote-only",
    )
    compiled_out = await read_what_ran(buck)
    compiled_digests = [
        entry["reproducer"]["details"]["digest"] for entry in compiled_out
    ]
    compiled_digests.sort()

    # TODO(nga): this should also test reverted buck2.
    buck.path_to_executable = Path("buck2")
    await buck.build(
        get_mode_from_platform(),
        "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
        "--remote-only",
    )
    deployed_out = await read_what_ran(buck)
    deployed_digests = [
        entry["reproducer"]["details"]["digest"] for entry in deployed_out
    ]
    deployed_digests.sort()

    assert (
        compiled_digests == deployed_digests
    ), "Action Digest was modified, refer to comment on this test for next steps"
