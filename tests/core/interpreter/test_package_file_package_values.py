# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_package_file_package_values(buck: Buck) -> None:
    # Build file does all the assertions.
    output = await buck.build("//:")
    assert "TEST PASSED" in output.stderr


@buck_test()
async def test_audit_package_values(buck: Buck) -> None:
    stdout = (await buck.audit("package-values", "//")).stdout
    golden(
        output=stdout,
        rel_path="audit-package-values.golden.json",
    )


@buck_test()
async def test_targets_package_values(buck: Buck) -> None:
    stdout = (await buck.targets("--package-values", "//...")).stdout
    golden(
        output=stdout,
        rel_path="targets-package-values.golden.json",
    )


@buck_test()
async def test_targets_package_values_regex(buck: Buck) -> None:
    # Empty string as regex.
    out = (await buck.targets("--package-values-regex", "", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {"aaa.bbb": "ccc", "xxx.yyy": "zzz"}
    assert json_result["buck.package_values"] == expected

    out = (await buck.targets("--package-values-regex", "aaa.bbb", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {"aaa.bbb": "ccc"}
    assert json_result["buck.package_values"] == expected

    out = (await buck.targets("--package-values-regex", "xxx", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {"xxx.yyy": "zzz"}
    assert json_result["buck.package_values"] == expected

    out = (
        await buck.targets(
            "--package-values-regex",
            "aaa.bbb",
            "--package-values-regex",
            "xxx.yyy",
            "//...",
        )
    ).stdout
    json_result = json.loads(out)[0]
    expected = {"aaa.bbb": "ccc", "xxx.yyy": "zzz"}
    assert json_result["buck.package_values"] == expected

    out = (await buck.targets("--package-values-regex", "non_existent", "//...")).stdout
    json_result = json.loads(out)[0]
    expected = {}
    assert json_result["buck.package_values"] == expected

    args = ["allow", "only", "one", "arg", "per", "flag", "occurrence"]
    await expect_failure(
        buck.targets("--package-values-regex", *args, "//..."),
        stderr_regex="Error parsing root//arg",
    )


@buck_test()
async def test_targets_streaming_package_values(buck: Buck) -> None:
    stdout = (await buck.targets("--streaming", "--package-values", "//...")).stdout
    golden(
        output=stdout,
        rel_path="targets-streaming-package-values.golden.json",
    )
