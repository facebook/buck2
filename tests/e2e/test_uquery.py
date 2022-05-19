import json
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


"""
If you need to add a directory that's isolated in buck2/test/targets
(ex. some test of form @buck_test(inplace=False, data_dir=some_new_directory)),
then you will need to update isolated_targets in buck2/test/targets/TARGETS.
Otherwise the test will fail because it cannot recognize the new directory.
"""


@buck_test(inplace=False, data_dir="bql/simple")
async def test_uquery_inputs(buck: Buck) -> None:
    result = await buck.uquery("""inputs(set(root//bin:the_binary //lib:file1))""")
    assert result.stdout == "bin/TARGETS.fixture\n"

    result = await buck.uquery("""inputs(set())""")
    assert result.stdout == ""


@buck_test(inplace=False, data_dir="bql/simple")
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


@buck_test(inplace=False, data_dir="bql/simple")
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


@buck_test(inplace=False, data_dir="bql/simple")
async def test_uquery_buildfile(buck: Buck) -> None:
    result = await buck.uquery("""buildfile(root//bin:the_binary)""")
    assert result.stdout == "bin/TARGETS.fixture\n"

    result = await buck.uquery("""buildfile(root//bin: + root//data:)""")
    assert result.stdout == "bin/TARGETS.fixture\ndata/TARGETS.fixture\n"

    result = await buck.uquery(
        """buildfile(owner(../data/buck/build/data.file))""", rel_cwd=Path("special")
    )
    assert result.stdout == "data/TARGETS.fixture\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_deps(buck: Buck) -> None:
    result = await buck.uquery("""deps(root//bin:the_binary)""")
    assert (
        result.stdout
        == "\n".join(
            [
                "root//platforms:platform1",
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
                "root//platforms:platform1",
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
        "deps(%s, 1, exec_deps()) - %s" % (target_deps_expr, target_deps_expr)
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


@buck_test(inplace=False, data_dir="bql/simple")
async def test_uquery_cell(buck: Buck) -> None:
    result = await buck.uquery("""//stuff:magic""", rel_cwd=Path("special"))
    assert result.stdout == "special//stuff:magic\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_uquery_relative(buck: Buck) -> None:
    result = await buck.uquery("""...""", rel_cwd=Path("special"))
    assert result.stdout == "special//stuff:magic\n"
    result = await buck.uquery("""...""", rel_cwd=Path("bin"))
    assert "root//bin:the_binary\n" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_uquery_provider_names(buck: Buck) -> None:
    result = await buck.uquery("""'root//bin:the_binary[provider_name]'""")
    assert result.stdout == "root//bin:the_binary\n"

    result = await buck.uquery("""'root//bin:the_binary#some_flavor'""")
    assert result.stdout == "root//bin:the_binary\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_attributes(buck: Buck) -> None:
    out = await buck.uquery("set(root//bin:the_binary //lib:file1)")
    assert out.stdout == "root//bin:the_binary\nroot//lib:file1\n"

    json_out = await buck.uquery("--json", "set(root//bin:the_binary //lib:file1)")
    json_out = json.loads(json_out.stdout)
    assert ["root//bin:the_binary", "root//lib:file1"] == json_out

    attrs_out = await buck.uquery(
        "--output-attribute",
        "srcs",
        "--output-attribute",
        "deps",
        "set(root//bin:the_binary //lib:file1)",
    )
    attrs_json_out = await buck.uquery(
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
            "buck.configuration_deps": [],
            "deps": ["root//lib:lib1", "root//lib:lib2", "root//lib:lib3"],
            "srcs": ["root//bin/TARGETS.fixture"],
        },
        "root//lib:file1": {"buck.configuration_deps": []},
    } == attrs_json_out


@buck_test(inplace=False, data_dir="bql/simple")
async def test_dot(buck: Buck) -> None:
    out = await buck.uquery("--dot", "deps(root//bin:the_binary, 100, target_deps())")
    with open(buck.cwd / "expected/dot/deps") as f:
        assert out.stdout == f.read()

    out = await buck.uquery(
        "--dot",
        "--output-attribute=name",
        "--output-attribute=^deps",
        "deps(root//bin:the_binary, 100, target_deps()) - //platforms:",
    )
    with open(buck.cwd / "expected/dot/attrs") as f:
        assert out.stdout == f.read()

    out = await buck.uquery(
        "--dot",
        "deps(root//bin:the_binary, 100, target_deps()) - set(//lib: //platforms:)",
    )
    with open(buck.cwd / "expected/dot/subgraph") as f:
        assert out.stdout == f.read()


# Tests for "%Ss" uses
@buck_test(inplace=False, data_dir="bql/simple")
async def test_args_as_set(buck: Buck) -> None:
    out = await buck.uquery("%Ss", "root//bin:the_binary", "//lib:file1")
    assert out.stdout == "root//bin:the_binary\nroot//lib:file1\n"

    result = await buck.uquery("--json", "%Ss", "root//bin:the_binary", "//lib:file1")
    json_out = json.loads(result.stdout)
    assert json_out == ["root//bin:the_binary", "root//lib:file1"]


@buck_test(inplace=False, data_dir="bql/simple")
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
    assert out.stdout == "root//platforms:platform1\nroot//lib:file1\nroot//lib:lib1\n"

    out = await buck.uquery("owner(%s)", "inputs(root//bin:the_binary)")
    assert out.stdout == "root//bin:the_binary\n"

    out = await buck.uquery("owner(%s)", "data/buck/build/data.file")
    assert out.stdout == "root//data:data\n"


@buck_test(inplace=False, data_dir="testsof")
async def test_testsof(buck: Buck) -> None:
    out = await buck.uquery("testsof(//:foo_lib)")

    assert "root//:foo_test" in out.stdout
    assert "root//:foo_extra_test" in out.stdout
    assert "root//:foo_lib" not in out.stdout
