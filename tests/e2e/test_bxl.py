import json
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_label_functions(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/label_functions.bxl",
        "label_func_test",
    )

    assert result.stderr.splitlines() == [
        "root//bin:the_binary (root//platforms:platform1)",
        "root//bin:the_binary[sub] (root//platforms:platform1)",
        "root//bin:the_binary[sub1][sub2] (root//platforms:platform1)",
        "root//bin:the_binary",
        "root//bin:the_binary[sub]",
        "root//bin:the_binary[sub1][sub2]",
    ]


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_root(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl:root.bxl",
        "root_test",
    )

    assert str(buck.cwd) in result.stderr


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cli_args.bxl",
        "cli_test",
        "--",
        "--int_arg",
        "1",
        "--float_arg",
        "4.3",
        "--enum_type",
        "a",
        "--list_type",
        "1",
        "2",
        "3",
        "--target",
        ":foo",
        "--providers_label",
        "cell/pkg:bar[sub]",
    )
    assert (
        result.stderr
        == 'bool_arg: False\nstring_arg: "default"\nint_arg: 1\nfloat_arg: 4.3\noptional: None\nenum_type: "a"\ntarget: root//:foo\nproviders_label: root//cell/pkg:bar[sub]\nlist: [1, 2, 3]\n'
    )

    result = await buck.bxl(
        "//bxl/cli_args.bxl",
        "cli_test",
        "--",
        "--int_arg",
        "2",
        "--float_arg",
        "3.4",
        "--optional",
        "value",
        "--enum_type",
        "b",
        "--list_type",
        "1",
        "--target",
        "bar:foo",
        "--providers_label",
        "cell/pkg:bar",
    )
    assert (
        result.stderr
        == 'bool_arg: False\nstring_arg: "default"\nint_arg: 2\nfloat_arg: 3.4\noptional: "value"\nenum_type: "b"\ntarget: root//bar:foo\nproviders_label: root//cell/pkg:bar\nlist: [1]\n'
    )

    # illegal target
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl",
            "cli_test",
            "--",
            "--int_arg",
            "2",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "b",
            "--list_type",
            "1",
            "--target",
            "illegal?target",
            "--providers_label",
            "cell/pkg:bar",
        )
    )

    # not int
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl",
            "cli_test",
            "--",
            "--int_arg",
            "2.0",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "b",
            "--list_type",
            "1",
            "--target",
            ":foo",
            "--providers_label",
            "cell/pkg:bar",
        )
    )

    # not valid enum variant
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl",
            "cli_test",
            "--",
            "--int_arg",
            "2",
            "--float_arg",
            "3.4",
            "--optional",
            "value",
            "--enum_type",
            "not_enum",
            "--list_type",
            "1",
            "--target",
            ":foo",
            "--providers_label",
            "cell/pkg:bar",
        )
    )

    # missing non-optional field
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl",
            "cli_test",
            "--",
            "--int_arg",
            "2",
            "--optional",
            "value",
            "--enum_type",
            "a",
            "--list_type",
            "1",
            "--target",
            ":foo",
            "--providers_label",
            "cell/pkg:bar",
        )
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_owner(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl",
        "owner_test",
    )
    assert result.stderr == "[root//bin:the_binary (root//platforms:platform1)]\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_kind(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl",
        "kind_test",
    )

    assert "foo" in result.stderr
    assert "bar" not in result.stderr


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_attrregex_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl",
        "attrregexfilter_test",
    )

    assert "foo" in result.stderr
    assert "bzzt" in result.stderr
    assert "bar" not in result.stderr


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_rdeps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl",
        "rdeps_test",
    )
    assert (
        result.stderr
        == "[root//lib:file1 (root//platforms:platform1), root//lib:lib1 (root//platforms:platform1), root//bin:the_binary (root//platforms:platform1)]\n"
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_build(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/build.bxl",
        "build_test",
        "--show-all-outputs",
        "--show-all-outputs-format",
        "json",
        "--",
        "--target",
        ":buildable",
    )
    outputs = json.loads(result.stdout)
    assert (
        buck.cwd / Path(outputs["root//bxl/build.bxl:build_test"][0])
    ).read_text() == "FOO\n"

    result = await buck.bxl(
        "//bxl/build.bxl",
        "cquery_build_test",
        "--show-all-outputs",
        "--show-all-outputs-format",
        "json",
    )
    outputs = json.loads(result.stdout)
    assert (
        buck.cwd / Path(outputs["root//bxl/build.bxl:cquery_build_test"][0])
    ).read_text() == "FOO\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_analysis(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl:analysis.bxl",
        "providers_test",
    )

    assert "the_binary_foo" in result.stderr
    assert "bin_foo" in result.stderr


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl:actions.bxl",
        "artifact_test",
    )

    assert "<source bin/TARGETS.fixture>" in result.stderr
    assert "[<source bin/TARGETS.fixture>]" in result.stderr


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_create_build_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl:actions.bxl",
        "build_actions_test",
        "--show-all-outputs",
        "--show-all-outputs-format",
        "json",
        "--",
        "--content",
        "my_content",
    )
    outputs = json.loads(result.stdout)
    assert (
        buck.cwd / Path(outputs["root//bxl/actions.bxl:build_actions_test"][0])
    ).read_text() == "my_content"
