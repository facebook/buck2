# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
from pathlib import Path
from typing import Iterable, Set

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="visibility")
@pytest.mark.parametrize(
    "rule, passes",
    [
        ("self//:pass1", True),
        ("self//:pass2", True),
        ("self//:pass3", True),
        ("self//:pass4", True),
        ("self//:fail1", False),
        ("self//:fail2", False),
        ("self//:fail3", False),
        ("self//:fail4", False),
        ("self//:fail5", False),
        ("self//:fail6", False),
    ],
)
async def test_audit_visibility(buck: Buck, rule: str, passes: bool) -> None:
    if passes:
        out = await buck.audit_visibility(rule)
        assert out.stdout == ""
    else:
        await expect_failure(
            buck.audit_visibility(rule),
            stderr_regex=f"not visible to `{rule}`",
        )


@buck_test(inplace=True, skip_for_os=["windows", "darwin"])
async def test_audit_parse(buck: Buck) -> None:
    # random config hash
    config_hash = "3f794b0267173c8e"

    # rule
    # json
    result = await buck.audit(
        "parse",
        f"buck-out/v2/gen/fbsource/{config_hash}/path/to/target/__target_name__/output",
        "--json",
    )

    result = json.loads(result.stdout)
    assert result["cell_path"] == "fbsource//path/to/target"
    assert result["target_label"] == "fbsource//path/to/target:target_name"
    assert result["short_artifact_path"] == "output"
    assert result["config_hash"] == config_hash
    assert (
        result["full_artifact_path_no_hash"]
        == "fbsource/path/to/target/__target_name__/output"
    )

    # not json
    result = await buck.audit(
        "parse",
        f"buck-out/v2/gen/fbsource/{config_hash}/path/to/target/__target_name__/output",
    )

    result = result.stdout.splitlines()
    assert result[0] == "fbsource//path/to/target"
    assert result[1] == "fbsource//path/to/target:target_name"
    assert result[2] == "output"
    assert result[3] == config_hash
    assert result[4] == "fbsource/path/to/target/__target_name__/output"

    # output attribute
    result = await buck.audit(
        "parse",
        f"buck-out/v2/gen/fbsource/{config_hash}/path/to/target/__target_name__/output",
        "--output-attribute",
        "config_hash",
        "--output-attribute",
        "full_artifact_path_no_hash",
    )

    result = result.stdout.splitlines()
    assert result[0] == config_hash
    assert result[1] == "fbsource/path/to/target/__target_name__/output"

    # output attribute with json
    result = await buck.audit(
        "parse",
        f"buck-out/v2/gen/fbsource/{config_hash}/path/to/target/__target_name__/output",
        "--json",
        "--output-attribute",
        "config_hash",
        "--output-attribute",
        "full_artifact_path_no_hash",
    )

    result = json.loads(result.stdout)
    assert result["config_hash"] == config_hash
    assert (
        result["full_artifact_path_no_hash"]
        == "fbsource/path/to/target/__target_name__/output"
    )

    # tmp
    result = await buck.audit(
        "parse",
        f"buck-out/v2/tmp/fbsource/{config_hash}/path/to/target/__target_name__/output",
        "--json",
    )

    result = json.loads(result.stdout)
    assert result["cell_path"] == "fbsource//path/to/target"
    assert result["target_label"] == "fbsource//path/to/target:target_name"
    assert result["config_hash"] == config_hash
    assert (
        result["full_artifact_path_no_hash"]
        == "fbsource/path/to/target/__target_name__/output"
    )

    # bxl
    result = await buck.audit(
        "parse",
        f"buck-out/v2/gen-bxl/fbsource/{config_hash}/path/to/function.bxl/__function_name__/output",
        "--json",
    )

    result = json.loads(result.stdout)
    assert (
        result["bxl_function_label"] == "fbsource//path/to/function.bxl:function_name"
    )
    assert result["config_hash"] == config_hash
    assert (
        result["full_artifact_path_no_hash"]
        == "fbsource/path/to/function.bxl/__function_name__/output"
    )

    # anon
    result = await buck.audit(
        "parse",
        f"buck-out/v2/gen-anon/fbsource/{config_hash}/path/to/target/rule_hash/__target_name__/output",
        "--json",
    )

    result = json.loads(result.stdout)
    assert result["cell_path"] == "fbsource//path/to/target"
    assert result["target_label"] == "fbsource//path/to/target:target_name"
    assert result["config_hash"] == config_hash
    assert result["attr_hash"] == "rule_hash"
    assert (
        result["full_artifact_path_no_hash"]
        == "fbsource/path/to/target/rule_hash/__target_name__/output"
    )

    # test
    result = await buck.audit(
        "parse",
        f"buck-out/v2/test/fbsource/{config_hash}/path/to/target/__target_name__/output",
        "--json",
    )

    result = json.loads(result.stdout)
    assert result["cell_path"] == "fbsource//path/to/target"
    assert result["config_hash"] == config_hash
    assert (
        result["full_artifact_path_no_hash"]
        == "fbsource/path/to/target/__target_name__/output"
    )


@buck_test(inplace=False, data_dir="deferred_materializer")
async def test_audit_deferred_materializer_list(buck: Buck) -> None:
    res = await buck.audit("deferred-materializer", "list")
    assert res.stdout.strip() == ""

    await buck.build("//:simple")

    res = await buck.audit("deferred-materializer", "list")
    assert "__simple__" in res.stdout.strip()


@buck_test(inplace=False, data_dir="cells")
async def test_cell_ordering(buck: Buck) -> None:
    res = await buck.audit("cell")
    # The repository should be in the list, not the alias
    assert "b:" in res.stdout
    assert "a:" not in res.stdout
    assert "z:" not in res.stdout

    res = await buck.audit("cell", "--aliases")
    assert "b:" in res.stdout
    assert "a:" in res.stdout
    assert "z:" in res.stdout


def _classpath_jars(classpaths: Iterable[str]) -> Set[str]:
    return {Path(p).name for p in classpaths}


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath(buck: Buck) -> None:
    result = await buck.audit(
        "classpath", "fbsource//fbandroid/buck2/tests/good/classpath:top"
    )
    classpath_jars = _classpath_jars(result.stdout.splitlines())
    assert classpath_jars == {
        "top.jar",
        "direct_dep.jar",
        "mid_test.jar",
        "transitive_lib.jar",
    }


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath_binary(buck: Buck) -> None:
    result = await buck.audit(
        "classpath", "fbsource//fbandroid/buck2/tests/good/classpath:apk"
    )
    classpath_jars = _classpath_jars(result.stdout.splitlines())
    assert classpath_jars == {
        "dep_of_android_resource.jar",
        "ids_r_dot_java.jar",
        "top.jar",
        "direct_dep.jar",
        "transitive_lib.jar",
        "mid_test.jar",
        "lib_with_resource_only.jar",
    }


@buck_test(inplace=True, skip_for_os=["windows"])
async def test_audit_classpath_json(buck: Buck) -> None:
    top = "fbsource//fbandroid/buck2/tests/good/classpath:top"
    direct_dep = "fbsource//fbandroid/buck2/tests/good/classpath:direct_dep"

    result = await buck.audit("classpath", top, direct_dep, "--json")
    out = json.loads(result.stdout.strip())

    assert len(out.keys()) == 2, f"Found more than 2 targets in {out}"
    assert _classpath_jars(out.get(direct_dep)) == {"direct_dep.jar"}
    assert _classpath_jars(out.get(top)) == {
        "top.jar",
        "direct_dep.jar",
        "mid_test.jar",
        "transitive_lib.jar",
    }
