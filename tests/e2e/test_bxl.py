import json
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_label_functions(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/label_functions.bxl:label_func_test",
    )

    assert result.stdout.splitlines() == [
        "root//bin:the_binary (root//platforms:platform1)",
        "root//bin:the_binary[sub] (root//platforms:platform1)",
        # configured_target() called for below, should only return configured target
        "root//bin:the_binary (root//platforms:platform1)",
        "root//bin:the_binary[sub1][sub2] (root//platforms:platform1)",
        # configured_target() called for below, should only return configured target
        "root//bin:the_binary (root//platforms:platform1)",
        "root//bin:the_binary",
        "root//bin:the_binary[sub]",
        "root//bin:the_binary[sub1][sub2]",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_target_functions(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/target_functions.bxl:new_empty_target_set",
    )

    assert result.stdout.splitlines() == ["target_set", "0"]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_root(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl:root.bxl:root_test",
    )

    assert str(buck.cwd) in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
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

    # multiple occurrences of a list-type argument
    # i.e., --arg 1 --arg 2 --arg 3
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
        "--list_type",
        "2",
        "--list_type",
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


@buck_test(inplace=False, data_dir="bxl/simple")
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
    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            "root/bin:fake_name",
        )
    )

    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            "root/bin/fake_name:",
        )
    )

    await expect_failure(
        buck.bxl(
            "//bxl/cli_args.bxl:target_expr_test",
            "--",
            "--targets",
            "root/bin/fake_name:fake_name",
        )
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_owner(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_test",
    )
    assert result.stdout == "[root//bin:the_binary (root//platforms:platform1)]\n"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_owner_list(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:owner_list_test",
    )
    assert (
        result.stdout
        == "[root//bin:the_binary (root//platforms:platform1), root//bin:the_binary_with_dir_srcs (root//platforms:platform1)]\n"
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
async def test_query_rdeps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:rdeps_test",
    )
    assert (
        result.stdout
        == "[root//lib:file1 (root//platforms:platform1), root//lib:lib1 (root//platforms:platform1), root//bin:the_binary (root//platforms:platform1)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_query_deps(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/cquery.bxl:deps_test",
    )
    assert (
        result.stdout
        == "[root//bin:the_binary (root//platforms:platform1), root//platforms:platform1 (root//platforms:platform1), root//:data (root//platforms:platform1), root//lib:lib1 (root//platforms:platform1), root//lib:lib2 (root//platforms:platform1), root//lib:lib3 (root//platforms:platform1), root//:foo_toolchain (root//platforms:platform1), root//:bin (root//platforms:platform1)]\n"
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_build(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/build.bxl:build_test",
        "--",
        "--target",
        ":buildable",
    )
    outputs = json.loads(result.stdout)
    assert (buck.cwd / Path(outputs["root//:buildable"][0])).read_text() == "FOO"

    result = await buck.bxl(
        "//bxl/build.bxl:cquery_build_test",
    )
    outputs = result.stdout.splitlines()[0]
    assert (buck.cwd / Path(outputs)).read_text() == "FOO"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_analysis(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/analysis.bxl:providers_test",
    )

    assert "the_binary_foo" in result.stdout
    assert "bin_foo" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/actions.bxl:artifact_test",
    )

    assert "<source bin/TARGETS.fixture>" in result.stdout
    assert "[<source bin/TARGETS.fixture>]" in result.stdout


@buck_test(inplace=True)
async def test_bxl_artifact_path(buck: Buck) -> None:

    result = await buck.bxl(
        "fbcode//buck2/tests/targets/bxl/simple/bxl/artifacts.bxl:artifact_path_test",
    )

    outputs = json.loads(result.stdout)

    assert outputs["sources"] == ["<source buck2/tests/targets/rules/shell/DATA>"]

    assert outputs["source_artifact"] == "<source buck2/tests/targets/rules/shell/DATA>"
    # The project relative path of the source artifact
    assert (
        outputs["source_artifact_project_rel_path"]
        == "fbcode/buck2/tests/targets/rules/shell/DATA"
    )

    assert (
        "build artifact out/out.txt bound to fbcode//buck2/tests/targets/rules/shell:gen"
        in outputs["build_artifact"]
    )

    prefix = ""

    if buck.isolation_prefix is None:
        prefix = "buck-out/v2/gen/fbcode/"
    else:
        prefix = "buck-out/" + buck.isolation_prefix + "/gen/fbcode/"

    # The project relative path to the buck-out directory with the output
    assert prefix in outputs["build_artifact_project_rel_path"]
    assert (
        "/buck2/tests/targets/rules/shell/__gen__/out/out.txt"
        in outputs["build_artifact_project_rel_path"]
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_dynamic_action(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/dynamic.bxl:dynamic_test",
    )
    outputs = result.stdout.strip()
    assert Path(outputs).read_text() == "content"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_create_build_actions(buck: Buck) -> None:

    result = await buck.bxl(
        "//bxl/actions.bxl:build_actions_test",
        "--",
        "--content",
        "my_content",
    )
    assert (buck.cwd / Path(result.stdout.strip())).read_text() == "my_content"


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_configured_node(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/node.bxl:configured_node_test",
    )

    assert result.stdout.splitlines() == [
        "root//bin:the_binary (root//platforms:platform1)",
        "root//rules/rules.bzl:_foo_binary",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_unconfigured_node(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/node.bxl:unconfigured_node_test",
    )

    assert result.stdout.splitlines() == [
        "root//bin:the_binary",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
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


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_cquery_eval(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:cquery.bxl:eval_query_test",
    )

    assert "TARGETS.fixture" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_read_config(buck: Buck) -> None:
    result = await buck.bxl(
        "-c",
        "key.section=foo",
        "//bxl/read_config.bxl:read_config_test",
    )

    assert "foo" in result.stdout
    assert "True" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_load_file(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:load_file.bxl:load_test",
    )

    assert str(buck.cwd) in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
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


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_resolved_node_attrs(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:resolved_node_attributes.bxl:resolved_attrs_test",
    )

    outputs = json.loads(result.stdout)

    assert outputs["name"] == "the_binary"
    assert outputs["cmd"] == '["RunInfo(args=cmd_args())", "$(location ...)"]'
    assert (
        outputs["default_target_platform_label"]
        == "root//platforms:platform1 (root//platforms:platform1)"
    )
    assert (
        outputs["default_target_platform_providers"]
        == '[DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[]), PlatformInfo(label="root//platforms:platform1", configuration=ConfigurationInfo(constraints={}, values={}))]'
    )
    assert (
        outputs["foo_toolchain_label"]
        == "root//:foo_toolchain (root//platforms:platform1)"
    )
    assert (
        outputs["foo_toolchain_providers"]
        == '[DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[]), RunInfo(args=cmd_args()), FooInfo(foo="foo_toolchain_foo")]'
    )
    assert outputs["src"] == "<source bin/TARGETS.fixture>"
    assert (
        outputs["deps"]["root//lib:lib1 (root//platforms:platform1)"]
        == '[DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[]), FooInfo(foo="lib1_foo")]'
    )
    assert (
        outputs["deps"]["root//lib:lib2 (root//platforms:platform1)"]
        == '[DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[]), FooInfo(foo="lib2_foo")]'
    )
    assert (
        outputs["deps"]["root//lib:lib3 (root//platforms:platform1)"]
        == '[DefaultInfo(sub_targets={}, default_outputs=[], other_outputs=[]), FooInfo(foo="lib3_foo")]'
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_exists(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:fs.bxl:exists_relative_path",
    )

    assert "True" in result.stdout

    result = await buck.bxl(
        "//bxl:fs.bxl:not_exists",
    )

    assert "False" in result.stdout

    result = await buck.bxl(
        "//bxl:fs.bxl:exists_absolute_path", "--", "--root_path", buck.cwd
    )

    assert "True" in result.stdout

    result = await buck.bxl(
        "//bxl:fs.bxl:exists_source_artifact",
    )

    assert "True" in result.stdout

    result = await buck.bxl(
        "//bxl:fs.bxl:exists_file_node",
    )

    assert "True" in result.stdout


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_list(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl:fs.bxl:list_relative_path",
    )

    assert result.stdout.splitlines() == [
        "root//bin/TARGETS.fixture",
        "root//bin/kind",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_absolute_path", "--", "--root_path", buck.cwd
    )

    assert result.stdout.splitlines() == [
        "root//bin/TARGETS.fixture",
        "root//bin/kind",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_source_artifact",
    )

    assert result.stdout.splitlines() == [
        "root//bin/kind/TARGETS.fixture",
        "root//bin/kind/rules.bzl",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_file_node",
    )

    assert result.stdout.splitlines() == [
        "root//bin/kind/TARGETS.fixture",
        "root//bin/kind/rules.bzl",
    ]

    result = await buck.bxl(
        "//bxl:fs.bxl:list_include_ignored",
    )

    assert result.stdout.splitlines() == [
        "root//bin/TARGETS.fixture",
        "root//bin/ignored",
        "root//bin/ignored.txt",
        "root//bin/kind",
    ]


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_bxl_fs_is_file(buck: Buck) -> None:
    await buck.bxl("//bxl:fs.bxl:is_file", "--", "--root_path", buck.cwd)
