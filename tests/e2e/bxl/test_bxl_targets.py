# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_new_target_set(buck: Buck) -> None:
    await buck.bxl(
        "//bxl/new_target_set.bxl:new_ctarget_set",
    )

    await buck.bxl(
        "//bxl/new_target_set.bxl:new_utarget_set",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_target_set_ops(buck: Buck) -> None:
    await buck.bxl("//bxl/target_set_ops.bxl:test_operations")


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_target_platform_from_value_as_starlark_target_label(
    buck: Buck,
) -> None:
    # Pass in explicit target platform from client. Result should be configured with this target platform.
    result = await buck.bxl(
        "--target-platforms",
        "root//platforms:platform2",
        "//bxl/cquery.bxl:owner_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform2#<HASH>)]\n"
    )

    # No target platform specified from client context. Result should be configured with root//platforms:platform1
    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>)]\n"
    )

    # Target platform from client context should be overridden by what's declared in cquery.
    result = await buck.bxl(
        "--target-platforms",
        "root//platforms:platform2",
        "//bxl/cquery.bxl:owner_test_with_target_platform",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_unconfigured_sub_targets(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/providers.bxl:unconfigured_sub_targets",
    )

    output = json.loads(result.stdout.strip())
    assert output["lib1"] == "root//lib:lib1"
    assert output["lib1_FooInfo"] == "root//lib:lib1[FooInfo]"
    assert output["lib2"] == "root//lib:lib2"
    assert output["lib3_FooInfo"] == "root//lib:lib3[FooInfo]"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_target_exists(buck: Buck) -> None:
    await buck.bxl(
        "//bxl/target_exists.bxl:target_exists",
    )

    await expect_failure(
        buck.bxl("//bxl/target_exists.bxl:target_exists_no_target_patterns"),
        stderr_regex="Expected a single target as a string literal, not a target pattern",
    )

    await expect_failure(
        buck.bxl("//bxl/target_exists.bxl:target_exists_no_subtargets"),
        stderr_regex="Expecting target pattern, without providers",
    )
