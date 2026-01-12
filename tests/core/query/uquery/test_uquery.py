# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import re
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden
from manifold.clients.python.manifold_client_deprecated import Client as ManifoldClient

"""
If you need to add a directory that's isolated in buck2/test/targets
(ex. some test of form @buck_test( data_dir=some_new_directory)),
then you will need to update isolated_targets in buck2/test/targets/TARGETS.
Otherwise the test will fail because it cannot recognize the new directory.
"""


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


@buck_test(data_dir="bxl_simple")
async def test_uquery_inputs(buck: Buck) -> None:
    result = await buck.uquery("""inputs(set(root//bin:the_binary //lib:file1))""")
    assert result.stdout == "bin/TARGETS.fixture\n"

    result = await buck.uquery("""inputs(set())""")
    assert result.stdout == ""


@buck_test(data_dir="bxl_simple")
async def test_uquery_union(buck: Buck) -> None:
    result = await buck.uquery("""deps(root//lib:lib1) + set(root//data:data)""")
    assert result.stdout == "root//lib:file1\nroot//lib:lib1\nroot//data:data\n"

    result = await buck.uquery(
        """buildfile(root//bin:the_binary) + inputs(deps(root//lib:lib1))"""
    )
    assert result.stdout == "bin/TARGETS.fixture\nlib/TARGETS.fixture\n"

    result = await buck.uquery("""'root//bin:the_binary' + set(root//data:data)""")
    assert result.stdout == "root//bin:the_binary\nroot//data:data\n"


@buck_test(data_dir="bxl_simple")
async def test_uquery_owner(buck: Buck) -> None:
    result = await buck.uquery("""owner(bin/TARGETS.fixture)""")
    assert result.stdout == "root//bin:the_binary\n"

    result = await buck.uquery("""owner(data/buck/build/data.file)""")
    assert result.stdout == "root//data:data\n"

    # there's no buildfile in the root of the special buck, make sure that works
    result = await buck.uquery("""owner(special/file)""")
    assert "No owner" in result.stderr
    assert result.stdout == ""

    # there's a buildfile here, but no target owns the file
    result = await buck.uquery("""owner(.buckconfig)""")
    assert "No owner" in result.stderr
    assert result.stdout == ""

    result = await buck.uquery(
        """owner(../data/buck/build/data.file)""", rel_cwd=Path("special")
    )
    assert result.stdout == "root//data:data\n"

    result = await buck.uquery("""owner(root//bin/TARGETS.fixture)""")
    assert result.stdout == "root//bin:the_binary\n"


@buck_test(data_dir="bxl_simple")
async def test_query_owner_with_explicit_package_boundary_violation(buck: Buck) -> None:
    # This needs to be changed to `expect_failure` once Buck2 is checking path validity
    # outside of `package_boundary_exceptions`
    result = await buck.uquery(
        """owner(package_boundary_violation/bin)""",
        "-c",
        "project.package_boundary_exceptions=",
    )
    assert "root//package_boundary_violation:bin" in result.stdout
    assert "root//:package_boundary_violation" not in result.stdout

    result = await buck.uquery("""owner(package_boundary_violation/bin)""")
    assert "root//package_boundary_violation:bin" in result.stdout
    assert "root//:package_boundary_violation" in result.stdout


@buck_test(data_dir="bxl_simple", allow_soft_errors=True)
async def test_uquery_buildfile(buck: Buck) -> None:
    result = await buck.uquery("""buildfile(root//bin:the_binary)""")
    assert result.stdout == "bin/TARGETS.fixture\n"

    result = await buck.uquery("""buildfile(root//bin: + root//data:)""")
    assert result.stdout == "bin/TARGETS.fixture\ndata/TARGETS.fixture\n"

    result = await buck.uquery(
        """buildfile(owner(../data/buck/build/data.file))""", rel_cwd=Path("special")
    )
    assert result.stdout == "data/TARGETS.fixture\n"


@buck_test(data_dir="bxl_simple")
async def test_uquery_targets_in_buildfile(buck: Buck) -> None:
    result = await buck.uquery("""targets_in_buildfile(bin/TARGETS.fixture)""")
    assert (
        result.stdout
        == "\n".join(
            [
                "root//bin:setting",
                "root//bin:my_config",
                "root//bin:my_platform",
                "root//bin:the_binary",
                "root//bin:the_binary_with_dir_srcs",
                "root//bin:platform",
            ]
        )
        + "\n"
    )


@buck_test(data_dir="bxl_simple")
async def test_query_configuration_deps(buck: Buck) -> None:
    result = await buck.uquery(
        """deps(root//bin:the_binary, 1, configuration_deps())"""
    )
    assert "root//bin:my_config" in result.stdout


@buck_test(data_dir="bxl_simple")
async def test_deps(buck: Buck) -> None:
    result = await buck.uquery("""deps(root//bin:the_binary)""")
    assert (
        result.stdout
        == "\n".join(
            [
                "root//:foo_toolchain",
                "root//:bin",
                "root//lib:file3",
                "root//lib:lib3",
                "root//lib:file2",
                "root//lib:lib2",
                "root//lib:file1",
                "root//lib:lib1",
                "root//:genrule_binary",
                "root//:data",
                "root//bin:the_binary",
            ]
        )
        + "\n"
    )

    target_deps_expr = """deps(root//bin:the_binary, 100, target_deps())"""

    result = await buck.uquery(target_deps_expr)
    assert (
        result.stdout
        == "\n".join(
            [
                "root//bin:the_binary",
                "root//:data",
                "root//lib:lib1",
                "root//lib:lib2",
                "root//lib:lib3",
                "root//lib:file1",
                "root//lib:file2",
                "root//lib:file3",
            ]
        )
        + "\n"
    )

    # this is a little subtle, query's deps() function always forms a graph
    # with the nodes themselves so we subtract them out. It's not quite right
    # if a node in the graph of target deps were to have an exec dep on another.
    result = await buck.uquery(
        "deps({}, 1, exec_deps()) - {}".format(target_deps_expr, target_deps_expr)
    )
    assert (
        result.stdout
        == "\n".join(
            [
                "root//:foo_toolchain",
                "root//:bin",
                "root//:genrule_binary",
            ]
        )
        + "\n"
    )


@buck_test(data_dir="bxl_simple")
async def test_uquery_cell(buck: Buck) -> None:
    result = await buck.uquery("""//stuff:magic""", rel_cwd=Path("special"))
    assert result.stdout == "special//stuff:magic\n"


@buck_test(data_dir="bxl_simple")
async def test_uquery_relative(buck: Buck) -> None:
    result = await buck.uquery("""...""", rel_cwd=Path("special"))
    assert result.stdout == "special//stuff:magic\n"
    result = await buck.uquery("""...""", rel_cwd=Path("bin"))
    assert "root//bin:the_binary\n" in result.stdout


@buck_test(data_dir="bxl_simple")
async def test_uquery_provider_names(buck: Buck) -> None:
    await expect_failure(
        buck.uquery("'root//bin:the_binary[provider_name]'"),
        stderr_regex="Expected a target pattern without providers",
    )

    await expect_failure(
        buck.uquery("'root//bin:the_binary#some_flavor'"),
        stderr_regex="Expected a target pattern without providers",
    )


@buck_test(data_dir="bxl_simple")
async def test_query_filter(buck: Buck) -> None:
    # Test uquery/cquery on target and file sets
    out = await buck.uquery("filter('the_binary$', root//...)")
    assert out.stdout == "root//bin:the_binary\n"
    out = await buck.cquery("filter('the_binary\\w', root//...)")
    assert (
        _replace_hash(out.stdout)
        == "root//bin:the_binary_with_dir_srcs (root//platforms:platform1#<HASH>)\n"
    )
    out = await buck.uquery("filter('fixture$', inputs(root//bin:the_binary))")
    assert out.stdout == "bin/TARGETS.fixture\n"
    out = await buck.cquery("filter('fixture$', inputs(root//bin:the_binary))")
    assert out.stdout == "bin/TARGETS.fixture\n"


@buck_test(setup_eden=True, data_dir="bxl_simple")
async def test_attributes(buck: Buck) -> None:
    out = await buck.uquery("set(root//bin:the_binary //lib:file1)")
    assert out.stdout == "root//bin:the_binary\nroot//lib:file1\n"

    json_out = await buck.uquery("--json", "set(root//bin:the_binary //lib:file1)")
    json_out = json.loads(json_out.stdout)
    assert ["root//bin:the_binary", "root//lib:file1"] == json_out

    attrs_out = await buck.uquery(
        "--output-attribute",
        "buck\\..*",
        "--output-attribute",
        "srcs",
        "--output-attribute",
        "deps",
        "set(root//bin:the_binary //lib:file1)",
    )
    attrs_json_out = await buck.uquery(
        "--output-attribute",
        "buck\\..*",
        "--output-attribute",
        "srcs",
        "--output-attribute",
        "deps",
        "--json",
        "set(root//bin:the_binary //lib:file1)",
    )
    # specifying any attrs enables json output
    assert attrs_json_out.stdout == attrs_out.stdout
    attrs_json_out = json.loads(attrs_json_out.stdout)
    assert {
        "root//bin:the_binary": {
            "buck.deps": [
                "root//:data",
                "root//lib:lib1",
                "root//lib:lib2",
                "root//lib:lib3",
                "root//:foo_toolchain",
                "root//:bin",
            ],
            "buck.package": "root//bin:TARGETS.fixture",
            "buck.tree_modifiers": ["cfg//os:linux"],
            "buck.type": "_foo_binary",
            "buck.configuration_deps": ["root//bin:my_config"],
            "buck.oncall": None,
            "deps": ["root//lib:lib1", "root//lib:lib2", "root//lib:lib3"],
            "srcs": ["root//bin/TARGETS.fixture"],
        },
        "root//lib:file1": {
            "buck.deps": [],
            "buck.package": "root//lib:TARGETS.fixture",
            "buck.tree_modifiers": ["cfg//os:linux"],
            "buck.type": "_foo_genrule",
            "buck.configuration_deps": [],
            "buck.oncall": None,
        },
    } == attrs_json_out


@buck_test(data_dir="bxl_simple")
async def test_dot(buck: Buck) -> None:
    out = await buck.uquery("--dot", "deps(root//bin:the_binary, 100, target_deps())")
    golden(output=out.stdout, rel_path="bxl_simple/expected/dot/deps.golden")

    out = await buck.uquery(
        "--dot",
        "--output-attribute=name",
        "--output-attribute=^deps",
        "--output-attribute=cmd",
        "deps(root//bin:the_binary, 100, target_deps()) - //platforms:",
    )
    golden(output=out.stdout, rel_path="bxl_simple/expected/dot/attrs.golden")

    out = await buck.uquery(
        "--dot",
        "deps(root//bin:the_binary, 100, target_deps()) - set(//lib: //platforms:)",
    )
    golden(output=out.stdout, rel_path="bxl_simple/expected/dot/subgraph.golden")


@buck_test(data_dir="bxl_simple")
async def test_dot_compact(buck: Buck) -> None:
    out = await buck.uquery(
        "--dot-compact", "deps(root//bin:the_binary, 100, target_deps())"
    )
    golden(
        output=out.stdout,
        rel_path="bxl_simple/expected/dot_compact/deps.golden",
    )

    out = await buck.uquery(
        "--dot-compact",
        "--output-attribute=name",
        "--output-attribute=^deps",
        "--output-attribute=cmd",
        "deps(root//bin:the_binary, 100, target_deps()) - //platforms:",
    )
    golden(
        output=out.stdout,
        rel_path="bxl_simple/expected/dot_compact/attrs.golden",
    )

    out = await buck.uquery(
        "--dot-compact",
        "deps(root//bin:the_binary, 100, target_deps()) - set(//lib: //platforms:)",
    )
    golden(
        output=out.stdout,
        rel_path="bxl_simple/expected/dot_compact/subgraph.golden",
    )


@buck_test(data_dir="bxl_simple")
async def test_html(buck: Buck) -> None:
    uuid = "aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee"
    await buck.uquery(
        "--output-format=html",
        "deps(root//bin:the_binary, 100, target_deps())",
        env={"BUCK_WRAPPER_UUID": uuid},
    )
    with ManifoldClient({"bucket": "buck2_logs", "apikey": "buck2_logs-key"}) as client:
        assert client.exists(bucket="buck2_logs", path=f"flat/{uuid}-graph.html")


# Tests for "%Ss" uses
@buck_test(data_dir="bxl_simple")
async def test_args_as_set(buck: Buck) -> None:
    out = await buck.uquery("%Ss", "root//bin:the_binary", "//lib:file1")
    assert out.stdout == "root//bin:the_binary\nroot//lib:file1\n"

    result = await buck.uquery("--json", "%Ss", "root//bin:the_binary", "//lib:file1")
    json_out = json.loads(result.stdout)
    assert json_out == ["root//bin:the_binary", "root//lib:file1"]


@buck_test(data_dir="bxl_simple")
async def test_multi_uquery(buck: Buck) -> None:
    out = await buck.uquery("%s", "root//bin:the_binary", "//lib:file1")
    assert out.stdout == "root//bin:the_binary\nroot//lib:file1\n"

    result = await buck.uquery(
        "owner(%s)", "bin/TARGETS.fixture", "data/buck/build/data.file"
    )
    assert result.stdout == "root//bin:the_binary\nroot//data:data\n"

    result = await buck.uquery(
        "--json", "owner(%s)", "bin/TARGETS.fixture", "data/buck/build/data.file"
    )
    json_out = json.loads(result.stdout)

    assert json_out == {
        "bin/TARGETS.fixture": ["root//bin:the_binary"],
        "data/buck/build/data.file": ["root//data:data"],
    }

    # match buck1's strange handling of multi-query with --output-attribute
    result = await buck.uquery(
        "--json",
        "--output-attribute=name",
        "owner(%s)",
        "bin/TARGETS.fixture",
        "data/buck/build/data.file",
    )
    json_out = json.loads(result.stdout)

    assert json_out == {
        "root//bin:the_binary": {"name": "the_binary"},
        "root//data:data": {"name": "data"},
    }

    # test a case where the query for one arg fails. The process should exit with a non-zero code, but
    # the produced output should be valid json with an appropriate error indicator.
    failure = await expect_failure(
        buck.uquery("--json", "inputs(%s)", "//data:data", "xyz")
    )
    json_out = json.loads(failure.stdout)
    assert "$error" in json_out["xyz"]
    assert json_out["//data:data"] == ["data/buck/build/data.file"]

    # Test where the parameter is not a literal, but a query fragment
    out = await buck.uquery("%s", "deps(root//lib:lib1)")
    assert out.stdout == "root//lib:file1\nroot//lib:lib1\n"

    out = await buck.uquery("owner(%s)", "inputs(root//bin:the_binary)")
    assert out.stdout == "root//bin:the_binary\n"

    out = await buck.uquery("owner(%s)", "data/buck/build/data.file")
    assert out.stdout == "root//data:data\n"

    # We'd really prefer this to be an error, but Buck1 allows it
    out = await buck.uquery("owner(%s", "data/buck/build/data.file)")
    assert out.stdout == "root//data:data\n"


@buck_test(data_dir="testsof")
async def test_testsof(buck: Buck) -> None:
    out = await buck.uquery("testsof(//:foo_lib)")

    assert "root//:foo_test" in out.stdout
    assert "root//:foo_extra_test" in out.stdout
    assert "root//:foo_lib" not in out.stdout


@buck_test(data_dir="directory_sources")
async def test_directory_source(buck: Buck) -> None:
    await buck.build(":a_file")
    await buck.build(":a_dir")

    result = await buck.query("owner(dir/file1.txt)")
    assert result.stdout == "root//:a_dir\n"
    result = await buck.query("inputs(:a_dir)")
    assert (
        result.stdout == "dir/file1.txt\ndir/subdir/file2.txt\ndir/subdir/file3.txt\n"
    )

    # Can't reference files that don't exist
    await expect_failure(
        buck.build("does_not_exist:"),
        stderr_regex="Source file `does_not_exist` does not exist as a member of package",
    )

    # Want to make sure we can't do a package boundary violation
    # Currently these are soft errors
    await expect_failure(
        buck.build("subpackage:"),
        stderr_regex="Source file `subpackage` does not exist as a member of package",
    )

    await expect_failure(
        buck.build("dir_with_subpackage"),
        stderr_regex="may not cover any subpackages, but includes subpackage `dir_with_subpackage/subpackage`.",
    )


@buck_test(data_dir="oncall")
async def test_oncall(buck: Buck) -> None:
    out = await buck.uquery("//:foo", "--output-attribute=oncall")
    assert '"magic"' in out.stdout
    out = await buck.cquery("//:bar", "--output-attribute=oncall")
    assert '"magic"' in out.stdout


@buck_test(data_dir="oncall")
async def test_output_all_attributes(buck: Buck) -> None:
    def contains(out: BuckResult, want: list[str], notwant: list[str]) -> None:
        x = json.loads(out.stdout)["root//:foo"]
        for w in want:
            assert w in x
        for w in notwant:
            assert w not in x

    out = await buck.uquery("//:foo", "--output-all-attributes", "--json")
    contains(
        out,
        [
            "buck.type",
            "name",
            "buck.oncall",
            "buck.package",
            "buck.configuration_deps",
            "buck.deps",
            "visibility",
        ],
        ["madeup"],
    )
    out = await buck.uquery("//:foo", "--output-basic-attributes", "--json")
    contains(
        out,
        ["buck.type", "name", "buck.package", "visibility"],
        ["buck.oncall", "buck.configuration_deps"],
    )


@buck_test(data_dir="bxl_simple")
async def test_output_format_starlark_golden(buck: Buck) -> None:
    result = await buck.uquery(
        "--output-format=starlark",
        "--stack",
        "//lib:",
    )

    golden(
        output=result.stdout,
        rel_path="output_starlark.golden.out",
    )


@buck_test(data_dir="bxl_simple")
async def test_uquery_rdeps(buck: Buck) -> None:
    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1)""")
    assert result.stdout == "root//bin:the_binary\nroot//lib:lib1\nroot//lib:file1\n"

    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1, 0)""")
    assert result.stdout == "root//lib:file1\n"

    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1, 1)""")
    assert result.stdout == "root//lib:lib1\nroot//lib:file1\n"

    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1, 100)""")
    assert result.stdout == "root//bin:the_binary\nroot//lib:lib1\nroot//lib:file1\n"


@buck_test(data_dir="bxl_simple")
async def test_query_attrfilter_special_attribute(buck: Buck) -> None:
    out = await buck.uquery(
        "attrfilter(buck.package, 'root//bin:TARGETS.fixture',root//bin:the_binary)"
    )
    assert out.stdout.strip() == "root//bin:the_binary"


# Tests for intersect and except operators on FileSet, TargetSet, and String types
# These tests verify the fix for https://github.com/facebook/buck2/issues/1109
@buck_test(data_dir="set_operators")
async def test_uquery_fileset_intersect(buck: Buck) -> None:
    """Test FileSet intersect FileSet using inputs()."""
    result = await buck.uquery(
        """inputs(root//:lib_a) intersect inputs(root//:lib_b)"""
    )
    assert result.stdout == "common.txt\n"


@buck_test(data_dir="set_operators")
async def test_uquery_fileset_except(buck: Buck) -> None:
    """Test FileSet except FileSet using inputs()."""
    result = await buck.uquery("""inputs(root//:lib_a) except inputs(root//:lib_b)""")
    assert result.stdout == "lib_a.txt\n"


@buck_test(data_dir="set_operators")
async def test_uquery_fileset_intersect_string(buck: Buck) -> None:
    """Test FileSet intersect String."""
    result = await buck.uquery("""inputs(root//:lib_a) intersect "common.txt" """)
    assert result.stdout == "common.txt\n"


@buck_test(data_dir="set_operators")
async def test_uquery_fileset_except_string(buck: Buck) -> None:
    """Test FileSet except String."""
    result = await buck.uquery("""inputs(root//:lib_a) except "common.txt" """)
    assert result.stdout == "lib_a.txt\n"


@buck_test(data_dir="set_operators")
async def test_uquery_string_intersect_fileset(buck: Buck) -> None:
    """Test String intersect FileSet."""
    result = await buck.uquery(""" "common.txt" intersect inputs(root//:lib_a)""")
    assert result.stdout == "common.txt\n"


@buck_test(data_dir="set_operators")
async def test_uquery_string_except_fileset(buck: Buck) -> None:
    """Test String except FileSet (string not in fileset)."""
    result = await buck.uquery(""" "lib_a.txt" except inputs(root//:lib_b)""")
    assert result.stdout == "lib_a.txt\n"


@buck_test(data_dir="set_operators")
async def test_uquery_targetset_intersect(buck: Buck) -> None:
    """Test TargetSet intersect TargetSet using set()."""
    result = await buck.uquery(
        """set(root//:lib_a root//:app) intersect set(root//:lib_b root//:app)"""
    )
    assert result.stdout == "root//:app\n"


@buck_test(data_dir="set_operators")
async def test_uquery_targetset_except(buck: Buck) -> None:
    """Test TargetSet except TargetSet using set()."""
    result = await buck.uquery(
        """set(root//:lib_a root//:app) except set(root//:app)"""
    )
    assert result.stdout == "root//:lib_a\n"


@buck_test(data_dir="set_operators")
async def test_uquery_targetset_intersect_string(buck: Buck) -> None:
    """Test TargetSet intersect String."""
    result = await buck.uquery(
        """set(root//:lib_a root//:app) intersect "root//:lib_a" """
    )
    assert result.stdout == "root//:lib_a\n"


@buck_test(data_dir="set_operators")
async def test_uquery_targetset_except_string(buck: Buck) -> None:
    """Test TargetSet except String."""
    result = await buck.uquery("""set(root//:lib_a root//:app) except "root//:app" """)
    assert result.stdout == "root//:lib_a\n"


@buck_test(data_dir="set_operators")
async def test_uquery_string_intersect_targetset(buck: Buck) -> None:
    """Test String intersect TargetSet."""
    result = await buck.uquery(
        """ "root//:lib_a" intersect set(root//:lib_a root//:app)"""
    )
    assert result.stdout == "root//:lib_a\n"


@buck_test(data_dir="set_operators")
async def test_uquery_string_except_targetset(buck: Buck) -> None:
    """Test String except TargetSet (string not in targetset)."""
    result = await buck.uquery(
        """ "root//:app" except set(root//:lib_a root//:lib_b)"""
    )
    assert result.stdout == "root//:app\n"


@buck_test(data_dir="set_operators")
async def test_uquery_string_intersect_string(buck: Buck) -> None:
    """Test String intersect String for targets."""
    result = await buck.uquery(""" "root//:lib_a" intersect "root//:lib_a" """)
    assert result.stdout == "root//:lib_a\n"


@buck_test(data_dir="set_operators")
async def test_uquery_string_except_string(buck: Buck) -> None:
    """Test String except String (different targets)."""
    result = await buck.uquery(""" "root//:app" except "root//:lib_a" """)
    assert result.stdout == "root//:app\n"
