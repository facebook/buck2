# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import random
import re
import string

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_owner(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>)]\n"
    )

    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_with_cell_path_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_owner_list(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_list_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>), root//bin:the_binary_with_dir_srcs (root//platforms:platform1#<HASH>)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_kind(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:kind_test",
    )

    assert "foo" in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_inputs(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:inputs_test",
    )

    assert "TARGETS.fixture" in result.stdout
    assert "file_set" in result.stdout
    assert "1" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:filter_test",
    )

    assert "root//bin:the_binary" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_attrregex_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:attrregexfilter_test",
    )

    assert "foo" in result.stdout
    assert "bzzt" in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_attrfilter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:attrfilter_test",
    )

    assert "foo" in result.stdout
    assert "bzzt" not in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_nattrfilter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:nattrfilter_test",
    )

    assert "foo" not in result.stdout
    assert "bzzt" in result.stdout
    assert "bar" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_rdeps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:rdeps_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>), root//lib:lib1 (root//platforms:platform1#<HASH>), root//lib:file1 (root//platforms:platform1#<HASH>)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_deps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:deps_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary (root//platforms:platform1#<HASH>), root//:data (root//platforms:platform1#<HASH>), root//lib:lib1 (root//platforms:platform1#<HASH>), root//lib:lib2 (root//platforms:platform1#<HASH>), root//lib:lib3 (root//platforms:platform1#<HASH>), root//:foo_toolchain (root//platforms:platform1#<HASH>), root//:bin (root//platforms:platform1#<HASH>)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_buildfile(buck: Buck) -> None:
    await buck.bxl("//bxl/cquery.bxl:buildfile_test")


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_incompatible_configured_targets(buck: Buck) -> None:
    # incompatible target should be skipped and the cquery should return compatible targets
    result = await buck.bxl("//bxl/cquery.bxl:incompatible_configured_targets_test")
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible" in result.stderr
    assert "root//incompatible_targets:foo" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_incompatible_configured_targets_single_label(buck: Buck) -> None:
    # incompatible target should be skipped and the cquery should not fail
    result = await buck.bxl(
        "//bxl/cquery.bxl:incompatible_configured_targets_single_label_test"
    )
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible" in result.stderr


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_incompatible_targets(buck: Buck) -> None:
    # incompatible target should be skipped and the cquery should not fail
    result = await buck.bxl("//bxl/cquery.bxl:incompatible_targets_test")
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible" in result.stderr
    assert "root//incompatible_targets:foo" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_incompatible_targets_recursive(buck: Buck) -> None:
    # incompatible target should be skipped and the cquery should return compatible targets
    result = await buck.bxl("//bxl/cquery.bxl:incompatible_targets_test_recursive")
    assert "Skipped 2 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible" in result.stderr
    assert "root//incompatible_targets/inner_folder:incompatible_inner" in result.stderr
    assert "root//incompatible_targets/inner_folder:foo_inner" in result.stdout
    assert "root//incompatible_targets:foo" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_configured_label(buck: Buck) -> None:
    await buck.bxl("//bxl/cquery.bxl:cquery_configured_label")


@buck_test(inplace=False, data_dir="testsof")
async def test_cquery_testsof(buck: Buck) -> None:
    result = await buck.bxl(
        "//cquery.bxl:testsof_test",
    )
    assert "root//:foo_test (root//:platform_default_tests" in result.stdout

    result = await buck.bxl(
        "//cquery.bxl:testsof_with_default_target_platform_test",
    )
    assert (
        "root//:foo_test_with_default_platform (root//:foo_test_default_platform"
        in result.stdout
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_allpaths(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:allpaths_test",
    )

    assert (
        "[root//graph:one, root//graph:ten, root//graph:eleven, root//graph:two, root//graph:three]\n"
        == result.stdout
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_allpaths_filtered(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:allpaths_filtered_test",
    )

    assert "[root//graph:one, root//graph:two, root//graph:three]\n" == result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_somepath(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:somepath_test",
    )

    assert "[root//graph:one, root//graph:two, root//graph:three]\n" == result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_somepath_filtered(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:somepath_filtered_test",
    )

    assert (
        "[root//graph:one, root//graph:ten, root//graph:twenty]\n"
        + "[root//graph:one, root//graph:five, root//graph:six, root//graph:twenty]\n"
        == result.stdout
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_kind(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:kind_test",
    )

    assert "foo" in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_inputs(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:inputs_test",
    )

    assert "TARGETS.fixture" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:uquery.bxl:filter_test",
    )

    assert "root//bin:the_binary" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_attrregex_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:attrregexfilter_test",
    )

    assert "foo" in result.stdout
    assert "bzzt" in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_attrfilter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:attrfilter_test",
    )

    assert "foo" in result.stdout
    assert "bzzt" not in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_owner(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:owner_test",
    )
    assert result.stdout == "[root//bin:the_binary]\n"

    result = await buck.bxl(
        "//bxl/uquery.bxl:owner_with_cell_path_test",
    )
    assert _replace_hash(result.stdout) == "[root//bin:the_binary]\n"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_owner_list(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:owner_list_test",
    )
    assert (
        _replace_hash(result.stdout)
        == "[root//bin:the_binary, root//bin:the_binary_with_dir_srcs]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_targets_in_buildfile(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:targets_in_buildfile_test",
    )
    assert (
        result.stdout
        == "[root//bin:the_binary, root//bin:the_binary_with_dir_srcs, root//bin:platform]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_buildfile(buck: Buck) -> None:
    await buck.bxl("//bxl/uquery.bxl:buildfile_test")


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_rdeps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:rdeps_test",
    )
    assert result.stdout == "[root//bin:the_binary, root//lib:lib1, root//lib:file1]\n"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_query_deps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:deps_test",
    )
    assert (
        result.stdout
        == "[root//bin:the_binary, root//:data, root//lib:lib1, root//lib:lib2, root//lib:lib3, root//:foo_toolchain, root//:bin]\n"
    )


@buck_test(inplace=False, data_dir="testsof")
async def test_uquery_testsof(buck: Buck) -> None:
    result = await buck.bxl(
        "//uquery.bxl:testsof_test",
    )
    assert "root//:foo_test" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_uquery_eval(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/uquery.bxl:eval_query_test",
    )
    assert result.stdout == "[root//bin/TARGETS.fixture]\n"

    result = await buck.bxl(
        "//bxl/uquery.bxl:eval_query_with_query_args",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_aquery_incompatible_targets(buck: Buck) -> None:
    # incompatible target should be skipped and the aquery should not fail
    result = await buck.bxl("//bxl/aquery.bxl:incompatible_targets")
    assert "Skipped 1 incompatible targets" in result.stderr
    assert "root//incompatible_targets:incompatible" in result.stderr


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_eval(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:eval_query_test",
    )

    assert "TARGETS.fixture" in result.stdout

    result = await buck.bxl(
        "//bxl/cquery.bxl:eval_query_with_query_args",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_allpaths(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:allpaths_test",
    )

    assert (
        "[root//graph:one (<unspecified>), root//graph:ten (<unspecified>), root//graph:eleven (<unspecified>), root//graph:two (<unspecified>), root//graph:three (<unspecified>)]\n"
        == result.stdout
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_somepath(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:somepath_test",
    )

    assert (
        "[root//graph:one (<unspecified>), root//graph:two (<unspecified>), root//graph:three (<unspecified>)]\n"
        == result.stdout
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_somepath_filtered(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:somepath_filtered_test",
    )

    assert (
        "[root//graph:one (<unspecified>), root//graph:ten (<unspecified>), root//graph:twenty (<unspecified>)]\n"
        + "[root//graph:one (<unspecified>), root//graph:five (<unspecified>), root//graph:six (<unspecified>), root//graph:twenty (<unspecified>)]\n"
        == result.stdout
    )


def random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))
