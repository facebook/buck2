# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

"""
Test that when we render paths relative to the repo root, we prefix them with a
`./` to ensure the OS executes the cwd-relative path and doesn't do a $PATH
lookup for them.
"""


@buck_test()
async def test_build_root_executable_local(buck: Buck) -> None:
    await buck.build(":top", "--local-only")


@buck_test()
async def test_build_root_executable_remote(buck: Buck) -> None:
    await buck.build(":top", "--remote-only")
