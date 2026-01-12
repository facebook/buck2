# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_help(buck: Buck) -> None:
    result = await buck.help_env()
    golden(
        output=result.stdout,
        rel_path="buck2-help-env.golden.txt",
    )
    result = await buck.help_env("--self-testing")
    golden(
        output=result.stdout,
        rel_path="buck2-help-env-testing.golden.txt",
    )
