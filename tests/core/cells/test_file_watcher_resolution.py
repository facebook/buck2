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
async def test_changing_cell_location_bug(buck: Buck) -> None:
    await buck.targets("foo//:", "bar//:")

    # Switch the location of the 2 cells
    (buck.cwd / ".buckconfig").write_text(
        "[cells]\nfoo=bar\nbar=foo\nroot=.\nprelude=.\n"
    )

    # Make sure buck picks up the `CellResolver` updates
    await buck.targets("foo//:", "bar//:")

    (buck.cwd / "foo" / "TARGETS.fixture").write_text("fail('error')")

    await expect_failure(
        buck.targets("foo//:", "bar//:"),
        stderr_regex="fail: error",
    )
