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


@buck_test(inplace=False)
async def test_no_package_call_does_not_reset_visibility(buck: Buck) -> None:
    # Test that PACKAGE file without package() call does not reset visibility inherited from parent PACKAGE file.

    # TODO(rajneesh): This test is currently broken. The nested_package:bottom target
    # should not reset visibility if the PACKAGE file does not call the package() function.
    await expect_failure(
        buck.build("root//b:top"),
        stderr_regex=".*`root//a/nested_package:bottom` is not visible to `root//b:top`.*",
    )
