# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


# `project.ignore` is read when the daemon starts. A command that asks for a
# different one is refused rather than run with the daemon's. The data sets it
# behind a `<file:...>` include, as fbsource does, which not every config parse
# follows.
class ProjectIgnoreTest(BuckTestCase):
    @buck_test()
    async def test_override_is_refused(self, buck: Buck) -> None:
        await buck.targets(":")
        await expect_failure(
            buck.targets(":", "-c", "project.ignore=other"),
            stderr_regex="`project.ignore` cannot be set with `--config`",
        )

    @buck_test()
    async def test_change_needs_a_restart(self, buck: Buck) -> None:
        await buck.targets(":")

        ignores = buck.cwd / "ignores.bcfg"
        ignores.write_text(
            ignores.read_text().replace("ignore = ignored", "ignore = other")
        )
        await expect_failure(
            buck.targets(":"),
            stderr_regex="`project.ignore` of cell `root` differs from what the daemon read when it started",
        )

        await buck.kill()
        await buck.targets(":")
