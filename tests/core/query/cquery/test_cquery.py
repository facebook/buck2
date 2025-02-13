# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash

"""
Generally we test for basic functionality of things working here and do
more extensive testing in the uquery tests.
"""


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test(data_dir="unsorted")
async def test_query_inputs(buck: Buck) -> None:
    result = await buck.cquery("""inputs(set(root//bin:the_binary //lib:file1))""")
    assert result.stdout == "bin/TARGETS.fixture\n"


@buck_test(data_dir="unsorted")
async def test_query_cell(buck: Buck) -> None:
    result = await buck.cquery("""//stuff:magic""", rel_cwd=Path("special"))
    assert (
        _replace_hash(result.stdout)
        == "special//stuff:magic (root//platforms:platform1#<HASH>)\n"
    )


@buck_test(data_dir="unsorted")
async def test_query_relative(buck: Buck) -> None:
    result = await buck.cquery("""...""", rel_cwd=Path("special"))
    assert (
        _replace_hash(result.stdout)
        == "special//stuff:magic (root//platforms:platform1#<HASH>)\n"
    )


@buck_test(data_dir="unsorted")
async def test_query_provider_names(buck: Buck) -> None:
    await expect_failure(
        buck.cquery("'root//bin:the_binary[provider_name]'"),
        stderr_regex="Expected a target pattern without providers",
    )

    await expect_failure(
        buck.cquery("'root//bin:the_binary#some_flavor'"),
        stderr_regex="Expected a target pattern without providers",
    )


@buck_test(data_dir="unsorted")
async def test_query_print_provider_text(buck: Buck) -> None:
    out = await buck.cquery("%s", "root//bin:the_binary", "--show-providers")
    golden_replace_cfg_hash(
        output=_replace_hash(out.stdout),
        rel_path="unsorted/query_print_provider_text.golden.txt",
    )


@buck_test(data_dir="unsorted")
async def test_query_print_provider_json(buck: Buck) -> None:
    out = await buck.cquery("%s", "root//bin:the_binary", "--show-providers", "--json")
    golden_replace_cfg_hash(
        output=_replace_hash(out.stdout),
        rel_path="unsorted/query_print_provider_json.golden.json",
    )


@buck_test(data_dir="unsorted")
async def test_query_chunked_stream(buck: Buck) -> None:
    q = "deps(root//bin:the_binary)"
    result1 = await buck.cquery(q)
    await buck.kill()
    result2 = await buck.cquery(q, env={"BUCK2_DEBUG_RAWOUTPUT_CHUNK_SIZE": "5"})
    assert result1.stdout == result2.stdout


@buck_test(data_dir="unsorted")
async def test_attributes(buck: Buck) -> None:
    attrs_out = await buck.cquery(
        "--output-attribute",
        "buck\\..*",
        "--output-attribute",
        "srcs",
        "set(root//bin:the_binary //lib:file1)",
    )
    attrs_json_out = await buck.cquery(
        "--output-attribute",
        "buck\\..*",
        "--output-attribute",
        "srcs",
        "--json",
        "set(root//bin:the_binary //lib:file1)",
    )
    # specifying any attrs enables json output
    assert attrs_json_out.stdout == attrs_out.stdout
    attrs_json_out = json.loads(_replace_hash(attrs_json_out.stdout))
    assert {
        "root//bin:the_binary (root//platforms:platform1#<HASH>)": {
            "buck.deps": [
                "root//:data (root//platforms:platform1#<HASH>)",
                "root//lib:lib1 (root//platforms:platform1#<HASH>)",
                "root//lib:lib2 (root//platforms:platform1#<HASH>)",
                "root//lib:lib3 (root//platforms:platform1#<HASH>)",
                "root//:foo_toolchain (root//platforms:platform1#<HASH>)",
                "root//:bin (root//platforms:platform1#<HASH>)",
            ],
            "buck.execution_platform": "<legacy_global_exec_platform>",
            "buck.package": "root//bin:TARGETS.fixture",
            "buck.plugins": {},
            "buck.target_configuration": "root//platforms:platform1#<HASH>",
            "buck.type": "_foo_binary",
            "buck.oncall": None,
            "srcs": ["root//bin/TARGETS.fixture"],
        },
        "root//lib:file1 (root//platforms:platform1#<HASH>)": {
            "buck.deps": [],
            "buck.execution_platform": "<legacy_global_exec_platform>",
            "buck.package": "root//lib:TARGETS.fixture",
            "buck.plugins": {},
            "buck.target_configuration": "root//platforms:platform1#<HASH>",
            "buck.type": "_foo_genrule",
            "buck.oncall": None,
        },
    } == attrs_json_out


# Tests for "%Ss" uses
@buck_test(data_dir="unsorted")
async def test_args_as_set(buck: Buck) -> None:
    out = await buck.cquery("%Ss", "root//bin:the_binary", "//lib:file1")
    assert (
        _replace_hash(out.stdout)
        == "root//bin:the_binary (root//platforms:platform1#<HASH>)\nroot//lib:file1 (root//platforms:platform1#<HASH>)\n"
    )


@buck_test(data_dir="unsorted")
async def test_multi_query(buck: Buck) -> None:
    out = await buck.cquery("%s", "root//bin:the_binary", "//lib:file1")
    assert (
        _replace_hash(out.stdout)
        == "root//bin:the_binary (root//platforms:platform1#<HASH>)\nroot//lib:file1 (root//platforms:platform1#<HASH>)\n"
    )


@buck_test(data_dir="unsorted")
async def test_query_attrfilter(buck: Buck) -> None:
    out = await buck.uquery(
        "attrfilter(buck.package, 'root//bin:TARGETS.fixture',root//bin:the_binary)"
    )
    assert out.stdout.strip() == "root//bin:the_binary"


@buck_test(data_dir="multi_query_universe")
async def test_multi_query_universe(buck: Buck) -> None:
    out = await buck.cquery(
        "deps(%s)", "root//:macos-bin", "//:common-dep", "--output-format=json"
    )
    # `common-dep` is configured for linux, so it must not include `only-on-macos` target.
    #   Which would be the case if we constructed universe from all the queries together
    #   instead of separate universes for each query.
    golden_replace_cfg_hash(
        output=_replace_hash(out.stdout),
        rel_path="multi_query_universe/multi_query_universe.golden.json",
    )


@buck_test(data_dir="unsorted")
async def test_multi_query_print_provider_text(buck: Buck) -> None:
    out = await buck.cquery(
        "%s", "root//bin:the_binary", "//lib:lib1", "--show-providers"
    )
    golden_replace_cfg_hash(
        output=_replace_hash(out.stdout),
        rel_path="unsorted/multi_query_print_provider_text.golden.txt",
    )


@buck_test(data_dir="unsorted")
async def test_multi_query_print_provider_json(buck: Buck) -> None:
    out = await buck.cquery(
        "%s", "root//bin:the_binary", "//lib:lib1", "--show-providers", "--json"
    )

    golden_replace_cfg_hash(
        output=_replace_hash(out.stdout),
        rel_path="unsorted/multi_query_print_provider_json.golden.json",
    )


@buck_test(data_dir="visibility")
async def test_visibility(buck: Buck) -> None:
    for good in [
        "self//:pass1",
        "self//:pass2",
        "self//:pass3",
        "self//:pass4",
    ]:
        out = await buck.cquery(good)
        assert good in out.stdout

    for bad in [
        "self//:fail1",
        "self//:fail2",
        "self//:fail3",
        "self//:fail4",
    ]:
        print(bad)
        failure = await expect_failure(buck.cquery(bad))
        assert "not visible to `%s`" % bad in failure.stderr


@buck_test(data_dir="testsof")
async def test_testsof(buck: Buck) -> None:
    out = await buck.cquery(
        "testsof(//:foo_lib)",
        "--target-platforms",
        "//:platform_default_tests",
    )

    assert "root//:foo_test" in out.stdout
    assert "root//:foo_extra_test" not in out.stdout
    assert "root//:foo_lib" not in out.stdout

    out = await buck.cquery(
        "testsof(//:foo_lib)",
        "--target-platforms",
        "//:platform_more_tests",
    )

    assert "root//:foo_test" in out.stdout
    assert "root//:foo_extra_test" in out.stdout
    assert "root//:foo_lib" not in out.stdout


# DICE currently may re-evaluate dead nodes ignoring errors, but it cannot ignore panics.
# The disabling of execution platforms through a buckconfig ended up causing a panic
# that was the root cause of non-deterministic buck2 failures on 10% of fbcode TD in S303188.
#
# TODO(scottcao): Disabling execution platforms is a hack that we need to get rid of
# because it's not how buck2 should be used. Get rid of this test case once fbcode TD
# stops disabling execution platforms
@buck_test(data_dir="toolchain_deps")
async def test_disabling_of_execution_platforms(buck: Buck) -> None:
    # Run these commands 10x such that a stress run of 10 on continuous CI would run these commands 100x.
    # If there is a regression then the stress run would for sure detect it.
    for _ in range(10):
        query = "deps(set(tests/...))"
        await buck.cquery(query)
        await buck.cquery(query, "-c", "build.execution_platforms=")
