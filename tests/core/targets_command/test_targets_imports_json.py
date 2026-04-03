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
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_imports_json(buck: Buck) -> None:
    """Test that targets --streaming --imports handles JSON file imports."""
    result = await buck.targets("//...", "--json", "--streaming", "--imports")
    xs = json.loads(result.stdout)

    found_targets = False
    found_bzl = False
    found_json = False

    for x in xs:
        if "buck.imports" not in x:
            continue
        file = x["buck.file"]
        imports = x["buck.imports"]

        if file == "root//TARGETS.fixture":
            assert "root//uses_json.bzl" in imports
            found_targets = True
        elif file == "root//uses_json.bzl":
            assert "root//data.json" in imports
            found_bzl = True
        elif file == "root//data.json":
            assert imports == []
            found_json = True

    assert found_targets, "TARGETS.fixture imports should be reported"
    assert found_bzl, "uses_json.bzl imports (including data.json) should be reported"
    assert found_json, "data.json should appear as an import with empty sub-imports"
