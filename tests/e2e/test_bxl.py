import json
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_label_functions(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/label_functions.bxl:label_func_test",
    )

    assert result.stdout.splitlines() == [
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
        "//bxl:root.bxl:root_test",
    )

    assert str(buck.cwd) in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cli_args.bxl:cli_test",
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
        "--sub_target",
        "cell/pkg:bar[sub]",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nstring_arg: "default"\nint_arg: 1\nfloat_arg: 4.3\noptional: None\nenum_type: "a"\ntarget: root//:foo\nsub_target: root//cell/pkg:bar[sub]\nlist: [1, 2, 3]\n'
    )

    result = await buck.bxl(
        "//bxl/cli_args.bxl:cli_test",
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
        "--sub_target",
        "cell/pkg:bar",
    )
    assert (
        result.stdout
        == 'bool_arg: False\nstring_arg: "default"\nint_arg: 2\nfloat_arg: 3.4\noptional: "value"\nenum_type: "b"\ntarget: root//bar:foo\nsub_target: root//cell/pkg:bar\nlist: [1]\n'
    )

    # illegal target
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:cli_test",
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
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # not int
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:cli_test",
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
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # list inner type mismatch
    await expect_failure(
        buck.bxl(
            "//bxl:cli_args.bxl:cli_test",
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
            "wrong_inner_list_type",
            "--target",
            "bar:foo",
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # not valid enum variant
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:cli_test",
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
            "--sub_target",
            "cell/pkg:bar",
        )
    )

    # missing non-optional field
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:cli_test",
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
            "--sub_target",
            "cell/pkg:bar",
        )
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cli_target_pattern(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cli_args.bxl:target_expr_test",
        "--",
        "--targets",
        ":bin",
    )
    assert "[root//:bin]" in result.stdout

    result = await buck.bxl(
        "//bxl/cli_args.bxl:target_expr_test",
        "--",
        "--targets",
        "root//:",
    )
    assert "root//:bin" in result.stdout
    assert "root//:data" in result.stdout
    assert "root//:foo_toolchain" in result.stdout
    assert "root//:genrule_binary" in result.stdout
    assert "root//:package_boundary_violation" in result.stdout
    assert "root//:buildable" in result.stdout

    result = await buck.bxl(
        "//bxl/cli_args.bxl:target_expr_test",
        "--",
        "--targets",
        "root//:",
    )
    assert "root//:bin" in result.stdout
    assert "root//:data" in result.stdout
    assert "root//:foo_toolchain" in result.stdout
    assert "root//:genrule_binary" in result.stdout
    assert "root//:package_boundary_violation" in result.stdout
    assert "root//:buildable" in result.stdout

    result = await buck.bxl(
        "//bxl/cli_args.bxl:target_expr_test",
        "--",
        "--targets",
        "root//bin/...",
    )
    assert "root//bin/kind:foo" in result.stdout
    assert "root//bin/kind:bar" in result.stdout
    assert "root//bin/kind:bzzt" in result.stdout
    assert "root//bin:the_binary" in result.stdout

    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            ":non-existent",
        )
    )

    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            "invalid/...",
        )
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_owner(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_test",
    )
    assert result.stdout == "[root//bin:the_binary (root//platforms:platform1)]\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_kind(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:kind_test",
    )

    assert "foo" in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_inputs(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:inputs_test",
    )

    assert "TARGETS.fixture" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:filter_test",
    )

    assert "root//bin:the_binary" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_attrregex_filter(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:attrregexfilter_test",
    )

    assert "foo" in result.stdout
    assert "bzzt" in result.stdout
    assert "bar" not in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_rdeps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:rdeps_test",
    )
    assert (
        result.stdout
        == "[root//lib:file1 (root//platforms:platform1), root//lib:lib1 (root//platforms:platform1), root//bin:the_binary (root//platforms:platform1)]\n"
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_deps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:deps_test",
    )
    assert (
        result.stdout
        == "[root//bin:the_binary (root//platforms:platform1), root//platforms:platform1 (root//platforms:platform1), root//:data (root//platforms:platform1), root//lib:lib1 (root//platforms:platform1), root//lib:lib2 (root//platforms:platform1), root//lib:lib3 (root//platforms:platform1), root//:foo_toolchain (root//platforms:platform1), root//:bin (root//platforms:platform1)]\n"
    )


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_build(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/build.bxl:build_test",
        "--",
        "--target",
        ":buildable",
    )
    outputs = json.loads(result.stdout)
    assert (buck.cwd / Path(outputs["root//:buildable"][0])).read_text() == "FOO\n"

    result = await buck.bxl(
        "//bxl/build.bxl:cquery_build_test",
    )
    outputs = result.stdout.splitlines()[0]
    assert (buck.cwd / Path(outputs)).read_text() == "FOO\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_analysis(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/analysis.bxl:providers_test",
    )

    assert "the_binary_foo" in result.stdout
    assert "bin_foo" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/actions.bxl:artifact_test",
    )

    assert "<source bin/TARGETS.fixture>" in result.stdout
    assert "[<source bin/TARGETS.fixture>]" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_create_build_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/actions.bxl:build_actions_test",
        "--",
        "--content",
        "my_content",
    )
    assert (buck.cwd / Path(result.stdout.strip())).read_text() == "my_content"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_configured_node(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/node.bxl:configured_node_test",
    )

    assert result.stdout.splitlines() == [
        "root//bin:the_binary (root//platforms:platform1)",
        "root//rules/rules.bzl:_foo_binary",
    ]


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_unconfigured_node(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/node.bxl:unconfigured_node_test",
    )

    assert result.stdout.splitlines() == [
        "root//bin:the_binary",
    ]


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_caching(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/caching.bxl:print_caching",
    )

    assert "ran me" in result.stderr
    assert "result print" in result.stdout

    result = await buck.bxl(
        "//bxl/caching.bxl:print_caching",
    )

    assert "ran me" not in result.stderr
    assert "result print" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_cquery_eval(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:eval_query_test",
    )

    assert "TARGETS.fixture" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_bxl_read_config(buck: Buck) -> None:
    result = await buck.bxl(
        "-c",
        "key.section=foo",
        "//bxl/read_config.bxl:read_config_test",
    )

    assert "foo" in result.stdout
    assert "True" in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_load_file(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:load_file.bxl:load_test",
    )

    assert str(buck.cwd) in result.stdout


@buck_test(inplace=False, data_dir="bql/simple")
async def test_node_attrs(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:node_attributes.bxl:attrs_test",
    )

    assert result.stdout.splitlines() == [
        "string",
        "the_binary",
        "list",
        "[root//lib:lib1 (root//platforms:platform1), root//lib:lib2 (root//platforms:platform1), root//lib:lib3 (root//platforms:platform1)]",
        "list",
        '["$(exe root//:bin (root//platforms:platform1))", "$(location root//:data (root//platforms:platform1))"]',
    ]
