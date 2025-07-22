# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-unsafe

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_hash_changes_after_buckconfig_rollout(buck: Buck) -> None:
    result1 = await buck.cquery("//:target")
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        print("[buck2]", file=f)
        print("new_platform_hash_rollout = 1", file=f)
    result2 = await buck.cquery("//:target")
    assert result1.stdout != result2.stdout

    await buck.kill()
    result3 = await buck.cquery("//:target")
    assert result2.stdout == result3.stdout
