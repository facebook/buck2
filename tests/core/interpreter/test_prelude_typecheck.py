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


# Test prelude is typechecked unconditionally.
@buck_test()
async def test_prelude_typecheck(buck: Buck) -> None:
    await expect_failure(
        buck.uquery("//:"),
        stderr_regex="Expected type `str` but got `int`",
    )
