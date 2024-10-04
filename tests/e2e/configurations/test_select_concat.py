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
async def test_oneof_concat(buck: Buck) -> None:
    # TODO(T199609145): Fix the issue
    await expect_failure(
        buck.cquery("//:foo"),
        stderr_regex="while concat, LHS is oneof, expecting RHS to also be oneof",
    )
