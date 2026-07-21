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
async def test_load_as_extension_not_allowed(buck: Buck) -> None:
    """A `?as=` load is rejected when the file matches no glob in
    `buck2.load_as_allowlist`. The fixture allows `*.lock`; overriding the
    allowlist to empty makes `data.lock?as=toml` fail."""
    await expect_failure(
        buck.targets("//...", "-c", "buck2.load_as_allowlist="),
        stderr_regex=r"data\.lock\?as=toml.*load_as_allowlist",
    )
