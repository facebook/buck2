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


@buck_test(data_dir="cross_cell")
async def test_load_as_cross_cell_rejected(buck: Buck) -> None:
    """An `?as=` load may not reference a file owned by another cell's package,
    even when that package's intra-cell path matches the loader's. This checks
    the isolation comparison is on the full cell path, not the raw package path:
    both sides share the intra-cell path `pkg`, but the cells differ."""
    await expect_failure(
        buck.targets("//..."),
        stderr_regex=r"other//pkg/lib\.lock\?as=toml.*only visible within its own package",
    )


@buck_test(data_dir="cross_package")
async def test_load_as_cross_package_rejected(buck: Buck) -> None:
    """An `?as=` load may not reference another package: the loader is in
    `root//`, but `root//sub/data.lock` is owned by `root//sub`."""
    await expect_failure(
        buck.targets("//..."),
        stderr_regex=r"data\.lock\?as=toml.*only visible within its own package",
    )


@buck_test(data_dir="cross_package_json")
async def test_load_json_cross_package_rejected(buck: Buck) -> None:
    """A native `.json` load (no `?as=`) is package-isolated exactly like an
    `?as=` load: the loader is in `root//`, but `root//sub/data.json` is owned
    by `root//sub`."""
    await expect_failure(
        buck.targets("//..."),
        stderr_regex=r"data\.json.*only visible within its own package",
    )


@buck_test(data_dir="cross_package_toml")
async def test_load_toml_cross_package_rejected(buck: Buck) -> None:
    """A native `.toml` load (no `?as=`) is package-isolated exactly like an
    `?as=` load: the loader is in `root//`, but `root//sub/data.toml` is owned
    by `root//sub`."""
    await expect_failure(
        buck.targets("//..."),
        stderr_regex=r"data\.toml.*only visible within its own package",
    )
