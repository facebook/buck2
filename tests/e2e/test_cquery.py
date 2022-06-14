import json
import re
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

"""
Generally we test for basic functionality of things working here and do
more extensive testing in the uquery tests.
"""


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_inputs(buck: Buck) -> None:
    result = await buck.cquery("""inputs(set(root//bin:the_binary //lib:file1))""")
    assert result.stdout == "bin/TARGETS.fixture\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_owner(buck: Buck) -> None:
    result = await buck.cquery("""owner(bin/TARGETS.fixture)""")
    assert result.stdout == "root//bin:the_binary (root//platforms:platform1)\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_owner_with_explicit_package_boundary_violation(buck: Buck) -> None:
    result = await expect_failure(
        buck.cquery(
            """owner(package_boundary_violation/bin)""",
            "-c",
            "project.package_boundary_exceptions=",
        ),
        stderr_regex="Couldn't coerce `package_boundary_violation/bin` as a source.",
    )

    result = await buck.cquery("""owner(package_boundary_violation/bin)""")
    assert (
        "root//package_boundary_violation:bin (root//platforms:platform1)"
        in result.stdout
    )
    assert (
        "root//:package_boundary_violation (root//platforms:platform1)" in result.stdout
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_cell(buck: Buck) -> None:
    result = await buck.cquery("""//stuff:magic""", rel_cwd=Path("special"))
    assert result.stdout == "special//stuff:magic (root//platforms:platform1)\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_relative(buck: Buck) -> None:
    result = await buck.cquery("""...""", rel_cwd=Path("special"))
    assert result.stdout == "special//stuff:magic (root//platforms:platform1)\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_provider_names(buck: Buck) -> None:
    result = await buck.cquery("""'root//bin:the_binary[provider_name]'""")
    assert result.stdout == "root//bin:the_binary (root//platforms:platform1)\n"

    result = await buck.cquery("""'root//bin:the_binary#some_flavor'""")
    assert result.stdout == "root//bin:the_binary (root//platforms:platform1)\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_print_provider(buck: Buck) -> None:
    out = await buck.cquery("%s", "root//bin:the_binary", "--show-providers")
    assert (
        out.stdout == "root//bin:the_binary (root//platforms:platform1)\n"
        "  Providers([\n"
        "    DefaultInfo(\n"
        "      sub_targets={},\n"
        "      default_outputs=[],\n"
        "      other_outputs=[]\n"
        "    ),\n"
        "    RunInfo( args=cmd_args() ),\n"
        '    FooInfo( foo="the_binary_foo" )\n'
        "  ])\n"
    )

    out = await buck.cquery("%s", "root//bin:the_binary", "--show-providers", "--json")
    out = json.loads(out.stdout)

    providers = out["root//bin:the_binary"][
        "root//bin:the_binary (root//platforms:platform1)"
    ]["buck.providers"]
    assert providers["DefaultInfo"]["sub_targets"] == {}
    assert providers["DefaultInfo"]["default_outputs"] == []
    assert providers["DefaultInfo"]["other_outputs"] == []

    assert providers["RunInfo"]["args"]["items"] == []
    assert providers["RunInfo"]["args"]["hidden"] == []
    assert providers["RunInfo"]["args"]["options"] is None

    assert providers["FooInfo"]["foo"] == "the_binary_foo"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_chunked_stream(buck: Buck) -> None:
    q = "deps(root//bin:the_binary)"
    result1 = await buck.cquery(q)
    await buck.kill()
    result2 = await buck.cquery(q, env={"BUCK2_DEBUG_RAWOUTPUT_CHUNK_SIZE": "5"})
    assert result1.stdout == result2.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_attributes(buck: Buck) -> None:
    attrs_out = await buck.cquery(
        "--output-attribute", "srcs", "set(root//bin:the_binary //lib:file1)"
    )
    attrs_json_out = await buck.cquery(
        "--output-attribute", "srcs", "--json", "set(root//bin:the_binary //lib:file1)"
    )
    # specifying any attrs enables json output
    assert attrs_json_out.stdout == attrs_out.stdout
    attrs_json_out = json.loads(attrs_json_out.stdout)
    assert {
        "root//bin:the_binary (root//platforms:platform1)": {
            "srcs": ["root//bin/TARGETS.fixture"]
        },
        "root//lib:file1 (root//platforms:platform1)": {},
    } == attrs_json_out


# Tests for "%Ss" uses
@buck_test(inplace=False, data_dir="bql/simple")
async def test_args_as_set(buck: Buck) -> None:
    out = await buck.cquery("%Ss", "root//bin:the_binary", "//lib:file1")
    assert (
        out.stdout
        == "root//bin:the_binary (root//platforms:platform1)\nroot//lib:file1 (root//platforms:platform1)\n"
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_multi_query(buck: Buck) -> None:
    out = await buck.cquery("%s", "root//bin:the_binary", "//lib:file1")
    assert (
        out.stdout
        == "root//bin:the_binary (root//platforms:platform1)\nroot//lib:file1 (root//platforms:platform1)\n"
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_multi_query_print_provider(buck: Buck) -> None:
    out = await buck.cquery(
        "%s", "root//bin:the_binary", "//lib:lib1", "--show-providers"
    )
    assert (
        out.stdout == "root//bin:the_binary (root//platforms:platform1)\n"
        "  Providers([\n"
        "    DefaultInfo(\n"
        "      sub_targets={},\n"
        "      default_outputs=[],\n"
        "      other_outputs=[]\n"
        "    ),\n"
        "    RunInfo( args=cmd_args() ),\n"
        '    FooInfo( foo="the_binary_foo" )\n'
        "  ])\n"
        "root//lib:lib1 (root//platforms:platform1)\n"
        "  Providers([\n"
        "    DefaultInfo(\n"
        "      sub_targets={},\n"
        "      default_outputs=[],\n"
        "      other_outputs=[]\n"
        "    ),\n"
        '    FooInfo( foo="lib1_foo" )\n'
        "  ])\n"
    )

    out = await buck.cquery(
        "%s", "root//bin:the_binary", "//lib:lib1", "--show-providers", "--json"
    )
    out = json.loads(out.stdout)

    providers = out["root//bin:the_binary"][
        "root//bin:the_binary (root//platforms:platform1)"
    ]["buck.providers"]
    assert providers["DefaultInfo"]["sub_targets"] == {}
    assert providers["DefaultInfo"]["default_outputs"] == []
    assert providers["DefaultInfo"]["other_outputs"] == []

    assert providers["RunInfo"]["args"]["items"] == []
    assert providers["RunInfo"]["args"]["hidden"] == []
    assert providers["RunInfo"]["args"]["options"] is None

    assert providers["FooInfo"]["foo"] == "the_binary_foo"

    providers = out["//lib:lib1"]["root//lib:lib1 (root//platforms:platform1)"][
        "buck.providers"
    ]
    assert providers["DefaultInfo"]["sub_targets"] == {}
    assert providers["DefaultInfo"]["default_outputs"] == []
    assert providers["DefaultInfo"]["other_outputs"] == []

    assert providers["FooInfo"]["foo"] == "lib1_foo"


@buck_test(inplace=True)
async def test_compatible_with(buck: Buck) -> None:
    for good in [
        "fbcode//buck2/tests/targets/configurations_uncategorized:compatible_with_pass",
        "fbcode//buck2/tests/targets/configurations_uncategorized:compatible_with_pass2",
    ]:
        out = await buck.cquery(good)
        assert re.match(
            "{} \\(.*\\)\n".format(good),
            out.stdout,
        )

    for bad in [
        "fbcode//buck2/tests/targets/configurations_uncategorized:compatible_with_fail",
        "fbcode//buck2/tests/targets/configurations_uncategorized:compatible_with_fail2",
    ]:
        out = await buck.cquery(bad)
        assert out.stdout == ""


@buck_test(inplace=True)
async def test_owner_skips_incompatible_targets(buck: Buck) -> None:
    result = await buck.cquery(
        "owner(buck2/tests/targets/configurations/cquery_owner_skip_incompatible_targets/src.txt)",
        "--target-platforms=fbcode//buck2/tests/targets/configurations/cquery_owner_skip_incompatible_targets:platform2",
    )
    assert "No owner" in result.stderr
    assert "Skipping target incompatible node" in result.stderr


@buck_test(inplace=False, data_dir="visibility")
async def test_visibility(buck: Buck) -> None:
    for good in [
        "fbcode//:pass1",
        "fbcode//:pass2",
        "fbcode//:pass3",
        "fbcode//:pass4",
    ]:
        out = await buck.cquery(good)
        assert good in out.stdout

    for bad in [
        "fbcode//:fail1",
        "fbcode//:fail2",
        "fbcode//:fail3",
        "fbcode//:fail4",
    ]:
        print(bad)
        failure = await expect_failure(buck.cquery(bad))
        assert "not visible to `%s`" % bad in failure.stderr


@buck_test(inplace=False, data_dir="visibility")
async def test_visibility_default_public(buck: Buck) -> None:
    # Check with overrideen default visibility
    await buck.cquery(
        "--config",
        "buildfile.buck2_default_visibility_to_public=true",
        "fbcode//:fail4",
    )


@buck_test(inplace=False, data_dir="testsof")
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
