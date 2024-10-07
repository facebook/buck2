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


@buck_test()
async def test_deps_in_cquery_not_uquery(buck: Buck) -> None:
    # Check that plugin deps appear as deps in uquery but not in cquery
    result = await buck.uquery("deps(//tests:reg_a)")
    assert "//tests:reg_a_REAL" in result.stdout
    result = await buck.cquery("deps(//tests:reg_a)")
    assert "//tests:reg_a_REAL" not in result.stdout
    # And make sure that the attribute itself is serialized correctly in cquery and uquery
    result = await buck.uquery("-a", "actual", "//tests:reg_a")
    assert json.loads(result.stdout) == {
        "root//tests:reg_a": {"actual": "root//tests:reg_a_REAL"}
    }
    result = await buck.cquery("-a", "actual", "//tests:reg_a")
    assert json.loads(result.stdout) == {
        "root//tests:reg_a (<unspecified>)": {"actual": "root//tests:reg_a_REAL"}
    }


@buck_test()
async def test_cquery(buck: Buck) -> None:
    ###### Check that everything is correctly configured as reported by cquery
    result = await buck.cquery(
        "--json",
        "-a",
        "buck.deps",
        "-a",
        "buck.execution_platform",
        "-a",
        "buck.plugins",
        "deps(//tests:b)",
    )
    result = json.loads(result.stdout)

    b = next(v for k, v in result.items() if k.startswith("root//tests:b"))
    l = next(  # noqa: E741 `l` as a variable name is fine
        v for k, v in result.items() if k.startswith("root//tests:l")
    )

    assert set(b["buck.plugins"]["RustProcMacro"]) == {
        "root//tests:reg_a_REAL",
        "root//tests:reg_b_REAL",
        "root//tests:doc_a_REAL",
    }
    assert set(l["buck.plugins"]["RustProcMacro"]) == {
        "root//tests:reg_a_REAL",
        "root//tests:doc_b_REAL",
    }

    assert b["buck.execution_platform"].startswith("root//config:platform_linux")
    assert any(
        dep.startswith("root//tests:reg_a_REAL (root//config:platform_linux")
        for dep in b["buck.deps"]
    )
    assert l["buck.execution_platform"].startswith("root//config:platform_windows")
    assert any(
        dep.startswith("root//tests:reg_a_REAL (root//config:platform_windows")
        for dep in l["buck.deps"]
    )

    assert any(
        k.startswith("root//tests:reg_a_REAL (root//config:platform_linux")
        for k in result.keys()
    )
    assert any(
        k.startswith("root//tests:reg_a_REAL (root//config:platform_windows")
        for k in result.keys()
    )


@buck_test()
async def test_analysis(buck: Buck) -> None:
    # Check that we can properly identify all the different plugin deps in analysis
    result = await buck.build("root//tests:b", "root//tests:l")

    b = json.loads(
        result.get_build_report().output_for_target("root//tests:b").read_text()
    )
    assert b == {
        "indirect": ["Reg A (linux)"],
        "direct": ["Reg B (linux)"],
        "indirect_doc": ["Doc A (linux)"],
        "direct_doc": [],
    }

    l = json.loads(  # noqa: E741 `l` as a variable name is fine
        result.get_build_report().output_for_target("root//tests:l").read_text()
    )
    assert l == {  # noqa: E741 `l` as a variable name is fine
        "indirect": [],
        "direct": ["Reg A (windows)"],
        "indirect_doc": [],
        "direct_doc": ["Doc B (windows)"],
    }


@buck_test()
async def test_plugin_dep_errors(buck: Buck) -> None:
    # Tests are explained in the file
    await buck.build("//test_errors:recursive_target_1")

    await buck.build("//test_errors:regular_a")

    result = await buck.uquery("deps(//test_errors:regular_b)")
    assert "//test_errors:toolchain" in result.stdout
    result = await expect_failure(buck.build("//test_errors:regular_b"))
    assert (
        "Plugin dep `root//test_errors:toolchain` is a toolchain rule" in result.stderr
    )

    result = await expect_failure(buck.build("//test_errors:wrong_plugin_kind"))
    assert "The rule did not declare that it uses plugins of kind A" in result.stderr


@buck_test()
async def test_repeated_insertion(buck: Buck) -> None:
    result = await buck.cquery(
        "-a", "buck.plugins", "//repeated_insertion:different_deps_alias"
    )
    assert {"Plugin": ["root//repeated_insertion:plugin"]} == list(
        json.loads(result.stdout).values()
    )[0]["buck.plugins"]


@buck_test()
async def test_visibility(buck: Buck) -> None:
    result = await expect_failure(buck.build("//visibility:missing_access"))
    assert (
        "`root//visibility/package:hidden` is not visible to `root//visibility:missing_access`"
        in result.stderr
    )

    await buck.build("//visibility:has_access")
