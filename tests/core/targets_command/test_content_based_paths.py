# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_targets_show_outputs_with_content_based_path(buck: Buck) -> None:
    await expect_failure(
        buck.targets(
            "root//:write_with_content_based_path",
            "--show-output",
        ),
        stderr_regex="Using `targets --show-output` on output .*out.txt.* which has a content-based path is not allowed! Use `build --show-output` instead.",
    )
