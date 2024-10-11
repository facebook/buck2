# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


# Test compatible_with is evaluated before select,
# and if target is incompatible, select should not be evaluated at all.

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_compatible_with_before_select(buck: Buck) -> None:
    result = await buck.cquery(
        "--target-platforms=root//:p-linux",
        "root//:windows-only",
    )
    # Check it does not fail.
    result.check_returncode()

    result = await buck.cquery(
        "--target-platforms=root//:p-linux",
        "root//:windows-only-deps",
    )
    result.check_returncode()

    result = await buck.cquery(
        "--target-platforms=root//:p-linux",
        "deps(root//:windows-only-exec-deps)",
    )
    assert "root//:windows-only (root//:p-exec-windows" in result.stdout
