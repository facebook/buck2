# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.golden import golden


"""
Tests to ensure that the `buck docs` command works as expected
"""


class DocsTest(BuckTestCase):
    @buck_test()
    async def test_docs_returns(self, buck: Buck) -> None:
        result = await buck.docs("starlark")
        result.check_returncode()
        decoded = json.loads(result.stdout)
        assert decoded == []

    @buck_test()
    async def test_prelude_docs(self, buck: Buck) -> None:
        result = await buck.docs("starlark", "prelude//:prelude.bzl")
        result.check_returncode()
        decoded = json.loads(result.stdout)
        golden(
            output=json.dumps(decoded, indent=2),
            rel_path="prelude_docs.golden.json",
        )

    @buck_test()
    async def test_docs_fail_with_invalid_patterns(self, buck: Buck) -> None:
        await expect_failure(
            buck.docs("starlark", "not_an_import_path"),
            stderr_regex="Import path must have suffix .*: `root//not_an_import_path`",
        )
        await expect_failure(
            buck.docs("starlark", "//cell"),
            stderr_regex="Import path must have suffix .*: `root//cell`",
        )
