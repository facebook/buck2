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


@buck_test(setup_eden=True)
async def test_in_subdir(buck: Buck) -> None:
    err = "No such file or directory"
    await expect_failure(
        buck.targets("test_bundled_cell//dir:"),
        stderr_regex=err,
    )
    await expect_failure(
        buck.cquery("root//:"),
        stderr_regex=err,
    )
    # FIXME(JakobDegen): Decide if this is a bug or not
    (buck.cwd / "somedir").mkdir()
    await buck.targets("test_bundled_cell//dir:")
    await buck.cquery("root//:")
