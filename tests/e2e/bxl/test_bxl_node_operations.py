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
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_label_functions(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/label_functions.bxl:label_func_test",
    )

    assert _replace_hash(result.stdout).splitlines() == [
        "root//bin:the_binary (root//platforms:platform1#<HASH>)",
        "root//bin:the_binary[sub] (root//platforms:platform1#<HASH>)",
        # configured_target() called for below, should only return configured target
        "root//bin:the_binary (root//platforms:platform1#<HASH>)",
        "root//bin:the_binary[sub1][sub2] (root//platforms:platform1#<HASH>)",
        # configured_target() called for below, should only return configured target
        "root//bin:the_binary (root//platforms:platform1#<HASH>)",
        "root//bin:the_binary",
        "root//bin:the_binary[sub]",
        "root//bin:the_binary[sub1][sub2]",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_coerced_attrs(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/coerced_attributes.bxl:coerced_attrs_test",
    )

    result = await buck.bxl(
        "//bxl/coerced_attributes.bxl:coerced_attributes_display_json_test",
    )
    cmd_select = json.loads(result.stdout)
    assert cmd_select["__type"] == "selector"
    assert cmd_select["entries"] == {
        "DEFAULT": "foo",
        "ovr_config//os:macos": "bar",
        "ovr_config//os:windows": "foobar",
    }

    result = await buck.bxl(
        "//bxl/coerced_attributes.bxl:coerced_attributes_display_test",
    )

    output = result.stdout

    assert "root//platforms:platform1" in output
    assert "genrule_with_selects" in output
    assert (
        'select({"ovr_config//os:macos": "bar", "ovr_config//os:windows": "foobar", "DEFAULT": "foo"})'
        in output
    )
    assert "PUBLIC" in output
    assert "magic" in output

    await buck.bxl(
        "//bxl/coerced_attributes.bxl:selector_attrs_test",
    )
    await buck.bxl(
        "//bxl/coerced_attributes.bxl:concat_attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_configured_node(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/node.bxl:configured_node_test",
    )

    assert _replace_hash(result.stdout).splitlines() == [
        "root//bin:the_binary (root//platforms:platform1#<HASH>)",
        "root//rules/rules.bzl:_foo_binary",
        "normal",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_unconfigured_node(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/node.bxl:unconfigured_node_test",
    )

    assert result.stdout.splitlines() == [
        "root//bin:the_binary",
        "root//rules/rules.bzl:_foo_binary",
        "normal",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_node_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:node_attributes.bxl:attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_node_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:node_attributes.bxl:lazy_attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_node_attrs_with_special_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:node_attributes.bxl:lazy_attrs_with_special_attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_resolved_node_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:resolved_node_attributes.bxl:resolved_attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_resolved_node_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:resolved_node_attributes.bxl:lazy_resolved_attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_resolved_node_with_special_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:resolved_node_attributes.bxl:lazy_resolved_attrs_with_special_attrs_test",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_unconfigured_target_node_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:unconfigure_target_node_attrs.bxl:node_attrs",
    )

    result = await buck.bxl(
        "//bxl/unconfigure_target_node_attrs.bxl:node_attrs_display",
    )

    output = result.stdout

    assert "root//platforms:platform1" in output
    assert "genrule_with_selects" in output
    assert (
        'select({"ovr_config//os:macos": "bar", "ovr_config//os:windows": "foobar", "DEFAULT": "foo"})'
        in output
    )
    assert "PUBLIC" in output
    assert "magic" in output

    await buck.bxl(
        "//bxl/unconfigure_target_node_attrs.bxl:selector_attrs_test",
    )
    await buck.bxl(
        "//bxl/unconfigure_target_node_attrs.bxl:concat_attrs_test",
    )

    await buck.bxl(
        "//bxl/unconfigure_target_node_attrs.bxl:attr_metadata",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_configured_target_node_attrs(buck: Buck) -> None:
    await buck.bxl(
        "//bxl:configured_target_node_attrs.bxl:attrs_test",
    )
