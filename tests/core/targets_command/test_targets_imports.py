# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_imports(buck: Buck) -> None:
    result = await buck.targets("//...", "--json", "--streaming", "--imports")
    xs = json.loads(result.stdout)
    found = 0
    for x in xs:
        if "buck.imports" in x:
            if x["buck.file"] == "root//TARGETS.fixture":
                assert x["buck.package"] == "root//"
                assert x["buck.imports"] == ["prelude//prelude.bzl", "root//a.bzl"]
                found += 1
            elif x["buck.file"] == "root//a.bzl":
                assert x["buck.imports"] == [
                    "prelude//prelude.bzl",
                    "root//b.bzl",
                ]
                assert "buck.package" not in x
                found += 1
            elif x["buck.file"] == "root//PACKAGE":
                assert x["buck.imports"] == [
                    "prelude//prelude.bzl",
                    "root//b.bzl",
                ]
                assert "buck.package" not in x
                found += 1
    assert found == 3
