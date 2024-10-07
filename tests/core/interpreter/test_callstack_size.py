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
async def test_stack_overflow(buck: Buck) -> None:
    await expect_failure(
        buck.uquery("bad//:"), stderr_regex="Starlark call stack overflow"
    )


@buck_test()
async def test_callstack_size(buck: Buck) -> None:
    output = await buck.uquery("good//:")
    assert "TEST PASSED" in output.stderr
