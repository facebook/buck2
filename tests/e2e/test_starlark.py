# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_lint_buck2(buck: Buck) -> None:
    # FIXME(JakobDegen): Reusing `project.ignore` for this is bad, `starlark
    # lint` should have `-I` and `-X` flags like sapling
    await buck.starlark(
        "lint",
        "buck2",
        "-c",
        "project.ignore=buck2/tests/e2e,buck2/tests/core",
    )


@buck_test(inplace=True)
async def test_typecheck_prelude_lightweight(buck: Buck) -> None:
    await buck.starlark("typecheck", "buck2/prelude/prelude.bzl")


@buck_test(inplace=True)
async def test_typecheck_prelude_compiler(buck: Buck) -> None:
    await buck.uquery("fbcode//buck2:buck2", "--unstable-typecheck")
