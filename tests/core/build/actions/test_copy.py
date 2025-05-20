# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(skip_for_os=["windows"])  # Exec bit and all
async def test_exec_bit_of_copied_file(buck: Buck) -> None:
    res = await buck.build_without_report(
        ":perms_of_copied_file", "--out=-", "--local-only", "--no-remote-cache"
    )
    assert "r-x" in res.stdout.strip()


@buck_test()  # Make sure there's at least one test defined
async def test_dummy(buck: Buck) -> None:
    pass
