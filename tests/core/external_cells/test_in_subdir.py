# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class InSubdirTest(BuckTestCase):
    # A bundled cell is served from memory; whether its directory exists on disk
    # makes no difference.
    @buck_test(setup_eden=True)
    async def test_in_subdir(self, buck: Buck) -> None:
        await buck.targets("test_bundled_cell//dir:")
        await buck.cquery("root//:")

        (buck.cwd / "somedir").mkdir()
        await buck.targets("test_bundled_cell//dir:")
        await buck.cquery("root//:")
