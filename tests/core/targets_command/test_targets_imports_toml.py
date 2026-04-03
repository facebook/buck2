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
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_imports_toml(buck: Buck) -> None:
    """Test that targets --streaming --imports handles TOML file imports."""

    # TODO(jtbraun): D99294710 will fix this, right now all improts are being treated as starlark.
    await expect_failure(
        buck.targets("//...", "--json", "--streaming", "--imports"),
        stderr_regex=r"Error evaluating module: `root//data.toml`",
    )
