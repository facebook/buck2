# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json
import re
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash


async def _cquery_node(buck: Buck, target: str, *args: str) -> dict[str, Any]:
    result = await buck.cquery("--output-all-attributes", target, *args)
    nodes = json.loads(result.stdout)
    assert len(nodes) == 1, nodes
    return next(iter(nodes.values()))


def _platform(platform: str) -> str:
    """Strips the configuration hash from a platform reported by cquery."""
    return platform.split("#")[0]


class ExecutionGroupsTest(BuckTestCase):
    @buck_test()
    async def test_toolchains_select_only_their_group_platform(
        self, buck: Buck
    ) -> None:
        node = await _cquery_node(buck, "root//groups:bundle")
        # The macOS-only and remote-only toolchains constrain only their own groups, so the default
        # group has no constraints and resolves to the first platform.
        assert _platform(node["buck.execution_platform"]) == "root//config:linux_local"
        assert {
            group: _platform(platform)
            for group, platform in node["buck.exec_group_platforms"].items()
        } == {
            "Resources": "root//config:macos_local",
            "Package": "root//config:linux_remote",
        }

    @buck_test()
    async def test_group_toolchain_deps_use_group_platform(self, buck: Buck) -> None:
        result = await buck.cquery("deps(root//groups:bundle, 2)")
        deps = result.stdout.splitlines()
        assert any(
            dep.startswith("root//groups:macos_tool (root//config:macos_local")
            for dep in deps
        ), deps
        assert any(
            dep.startswith("root//groups:remote_tool (root//config:linux_remote")
            for dep in deps
        ), deps

    @buck_test()
    async def test_exec_deps_select_only_their_group_platform(self, buck: Buck) -> None:
        node = await _cquery_node(buck, "root//groups:binary")
        assert _platform(node["buck.execution_platform"]) == "root//config:linux_remote"
        # `Unused` has no deps, so it resolves to the first platform.
        assert {
            group: _platform(platform)
            for group, platform in node["buck.exec_group_platforms"].items()
        } == {
            "Link": "root//config:linux_local",
            "Unused": "root//config:linux_local",
        }

    @buck_test()
    async def test_exec_dep_in_two_groups_is_configured_for_each(
        self, buck: Buck
    ) -> None:
        result = await buck.cquery("deps(root//groups:binary, 1)")
        any_tool = sorted(
            _platform(dep.split(" ")[1].strip("()"))
            for dep in result.stdout.splitlines()
            if dep.startswith("root//groups:any_tool ")
        )
        assert any_tool == ["root//config:linux_local", "root//config:linux_remote"]

    @buck_test()
    async def test_dep_configured_identically_in_two_groups_is_one_dep(
        self, buck: Buck
    ) -> None:
        result = await buck.cquery(
            "root//groups:same_platform", "--output-attribute=buck.deps", "--json"
        )
        [deps] = [n["buck.deps"] for n in json.loads(result.stdout).values()]
        any_tool = [d for d in deps if d.startswith("root//groups:any_tool ")]
        assert len(any_tool) == 1, deps

    @buck_test()
    async def test_toolchain_configured_identically_in_two_groups_is_one_dep(
        self, buck: Buck
    ) -> None:
        result = await buck.cquery(
            "root//groups:same_platform_toolchain",
            "--output-attribute=buck.deps",
            "--json",
        )
        [deps] = [n["buck.deps"] for n in json.loads(result.stdout).values()]
        toolchain = [d for d in deps if d.startswith("root//groups:any_toolchain ")]
        assert len(toolchain) == 1, deps

    @buck_test()
    async def test_group_exec_dep_passed_to_anon_target(self, buck: Buck) -> None:
        await buck.aquery("root//anon_tool:t")

    @buck_test()
    async def test_audit_reports_skipped_platforms_per_group(self, buck: Buck) -> None:
        result = await buck.audit(
            "execution-platform-resolution",
            "root//groups:binary",
            "root//groups:bundle",
            "root//groups:same_platform",
            "root//groups:plugin_user",
        )
        golden_replace_cfg_hash(
            output=result.stdout, rel_path="audit_groups.txt.golden"
        )

    @buck_test()
    async def test_audit_explains_group_without_platform(self, buck: Buck) -> None:
        result = await buck.audit(
            "execution-platform-resolution", "root//errors/no_group_platform:t"
        )
        golden_replace_cfg_hash(
            output=result.stdout, rel_path="audit_no_group_platform.txt.golden"
        )

    @buck_test()
    async def test_audit_reports_group_diagnosis_error_under_group(
        self, buck: Buck
    ) -> None:
        result = await buck.audit(
            "execution-platform-resolution", "root//errors/group_diagnosis_error:t"
        )
        golden_replace_cfg_hash(
            output=result.stdout, rel_path="audit_group_diagnosis_error.txt.golden"
        )

    @buck_test()
    async def test_audit_reports_default_diagnosis_error_under_default(
        self, buck: Buck
    ) -> None:
        result = await buck.audit(
            "execution-platform-resolution", "root//errors/default_diagnosis_error:t"
        )
        golden_replace_cfg_hash(
            output=result.stdout, rel_path="audit_default_diagnosis_error.txt.golden"
        )

    @buck_test()
    async def test_audit_explains_group_without_platform_error_fallback(
        self, buck: Buck
    ) -> None:
        # With `fallback = "error"` the group's resolution fails configuration, and the error points
        # to this audit command.
        result = await buck.audit(
            "execution-platform-resolution",
            "root//errors/no_group_platform:t",
            "-c",
            "build.execution_platforms=root//config:platforms_error_fallback",
        )
        golden_replace_cfg_hash(
            output=result.stdout,
            rel_path="audit_no_group_platform_error_fallback.txt.golden",
        )

    @buck_test()
    async def test_rule_without_groups_has_no_group_platforms(self, buck: Buck) -> None:
        node = await _cquery_node(buck, "root//groups:plain")
        assert "buck.execution_platform" in node
        assert "buck.exec_group_platforms" not in node

    # Runs `python3` tool scripts locally and on a Linux RE platform.
    @buck_test(skip_for_os=["windows"])
    async def test_actions_run_on_group_platform(self, buck: Buck) -> None:
        await buck.build(
            "root//groups:bundle", "root//groups:binary", "--no-remote-cache"
        )
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        expected = {
            "compile_resources": "Local",
            "package": "Re",
            "compile": "Re",
            "explicit_default": "Re",
            "link": "Local",
            "unused_group": "Local",
        }
        # Build action identities have the form `<target> (<category> ...)`.
        executor_by_category = {
            category: [
                e["reproducer"]["executor"]
                for e in entries
                if re.search(rf"\({category}[ )]", e["identity"])
            ]
            for category in expected
        }
        assert executor_by_category == {
            category: [executor] for category, executor in expected.items()
        }, entries

    # Runs `python3` tool scripts locally and on a Linux RE platform.
    @buck_test(skip_for_os=["windows"])
    async def test_dynamic_output_actions_run_on_group_platform(
        self, buck: Buck
    ) -> None:
        await buck.build("root//groups:dynamic_binary", "--no-remote-cache")
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        executor_by_category = {
            category: [
                e["reproducer"]["executor"]
                for e in entries
                if re.search(rf"\({category}[ )]", e["identity"])
            ]
            for category in ("dynamic_compile", "dynamic_link")
        }
        assert executor_by_category == {
            "dynamic_compile": ["Re"],
            "dynamic_link": ["Local"],
        }, entries

    @buck_test()
    async def test_exec_compatible_with_attr_constrains_its_group(
        self, buck: Buck
    ) -> None:
        # Constraint labels are also matched in the target configuration, as for the target's
        # `exec_compatible_with`, so these targets need a real target platform.
        target_platform = "--target-platforms=root//config:linux_local"
        macos = await _cquery_node(buck, "root//groups:link_on_macos", target_platform)
        assert (
            _platform(macos["buck.execution_platform"]) == "root//config:linux_remote"
        )
        assert {
            group: _platform(platform)
            for group, platform in macos["buck.exec_group_platforms"].items()
        } == {"Link": "root//config:macos_local"}

        # The select keys on the target platform's `linux` constraint; without it `Link` would
        # resolve to `linux_local`, the first platform.
        via_select = await _cquery_node(
            buck, "root//groups:link_on_macos_via_select", target_platform
        )
        assert {
            group: _platform(platform)
            for group, platform in via_select["buck.exec_group_platforms"].items()
        } == {"Link": "root//config:macos_local"}

    @buck_test()
    async def test_audit_explains_group_constraints(self, buck: Buck) -> None:
        result = await buck.audit(
            "execution-platform-resolution",
            "root//groups:link_on_macos_remote",
            "--target-platforms=root//config:linux_local",
            "-c",
            "build.execution_platforms=root//config:platforms_error_fallback",
        )
        golden_replace_cfg_hash(
            output=result.stdout, rel_path="audit_group_constraints.txt.golden"
        )

    @buck_test()
    async def test_run_in_group_without_platform(self, buck: Buck) -> None:
        await expect_failure(
            buck.build(
                "root//errors/run_no_group_platform:t",
                "--target-platforms=root//config:linux_local",
            ),
            stderr_regex="Action with category `compile` and identifier `main` runs in execution group `G`, which has no compatible execution platform",
        )

    @buck_test()
    async def test_exec_compatible_with_attr_for_default_group(
        self, buck: Buck
    ) -> None:
        target_platform = "--target-platforms=root//config:linux_local"
        # Only the default group is constrained to macOS; `Link` still takes the first platform.
        node = await _cquery_node(
            buck, "root//groups:default_on_macos", target_platform
        )
        assert _platform(node["buck.execution_platform"]) == "root//config:macos_local"
        assert {
            group: _platform(platform)
            for group, platform in node["buck.exec_group_platforms"].items()
        } == {"Link": "root//config:linux_local"}

        no_groups = await _cquery_node(
            buck, "root//groups:no_groups_on_remote", target_platform
        )
        assert (
            _platform(no_groups["buck.execution_platform"])
            == "root//config:linux_remote"
        )

    @buck_test()
    async def test_two_exec_compatible_with_attrs_for_default_group(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.uquery("root//errors/exec_compatible_with_default:t"),
            stderr_regex="Execution group `DefaultExecGroup` has more than one `attrs.exec_compatible_with\\(\\)` attribute: `a` and `b`",
        )

    @buck_test()
    async def test_exec_compatible_with_attr_in_default_only(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/exec_compatible_with_default_only:t"),
            stderr_regex="`attrs.default_only\\(\\)` cannot wrap `attrs.exec_compatible_with\\(\\)`",
        )

    @buck_test()
    async def test_two_exec_compatible_with_attrs_for_one_group(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.uquery("root//errors/exec_compatible_with_twice:t"),
            stderr_regex="Execution group `G` has more than one `attrs.exec_compatible_with\\(\\)` attribute: `a` and `b`",
        )

    @buck_test()
    async def test_exec_compatible_with_attr_with_undeclared_group(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.uquery("root//errors/exec_compatible_with_undeclared:t"),
            stderr_regex="Attribute `c` uses execution group `G`, which the rule does not declare",
        )

    @buck_test()
    async def test_arg_tools_select_only_their_group_platform(self, buck: Buck) -> None:
        exe = await _cquery_node(buck, "root//groups:arg_exe")
        # The remote-only compiler is in the default group and the local-only tool in `Link`; with
        # the tool in the default group, no platform would satisfy both.
        assert _platform(exe["buck.execution_platform"]) == "root//config:linux_remote"
        assert {
            group: _platform(platform)
            for group, platform in exe["buck.exec_group_platforms"].items()
        } == {"Link": "root//config:linux_local"}

        location = await _cquery_node(buck, "root//groups:arg_location")
        assert {
            group: _platform(platform)
            for group, platform in location["buck.exec_group_platforms"].items()
        } == {"Link": "root//config:macos_local"}

    @buck_test()
    async def test_arg_tool_is_configured_for_its_group(self, buck: Buck) -> None:
        result = await buck.cquery("deps(root//groups:arg_exe, 1)")
        local_tool = [
            _platform(dep.split(" ")[1].strip("()"))
            for dep in result.stdout.splitlines()
            if dep.startswith("root//groups:local_tool ")
        ]
        assert local_tool == ["root//config:linux_local"]

    @buck_test()
    async def test_arg_tool_resolves_in_group_action(self, buck: Buck) -> None:
        result = await buck.aquery(
            "kind(run, deps(root//groups:arg_exe))", "--output-attribute=category"
        )
        assert [a["category"] for a in json.loads(result.stdout).values()] == [
            "arg_tool"
        ]

    @buck_test()
    async def test_strip_cfg_of_arg_in_group(self, buck: Buck) -> None:
        result = await buck.bxl("root//groups/strip_cfg.bxl:strip_cfg")
        assert result.stdout.splitlines() == [
            '"$(exe root//groups:local_tool)"',
            '"$(location_exec root//groups:macos_tool) $(location_toolchain root//groups:resource_toolchain)"',
        ]

    @buck_test()
    async def test_arg_with_undeclared_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/arg_undeclared:t"),
            stderr_regex="Attribute `cmd` uses execution group `G`, which the rule does not declare",
        )

    @buck_test()
    async def test_arg_in_group_passed_to_anon_target(self, buck: Buck) -> None:
        # `G` and the default group resolve to the same platform, so `G`'s tool can run there.
        await buck.aquery("root//errors/arg_in_anon:t")
        # A rule with execution groups cannot be an anon target at all.
        await expect_failure(
            buck.aquery("root//errors/arg_in_anon:grouped_anon"),
            stderr_regex="Rule `grouped_anon` declares execution groups, which anon targets do not have",
        )

    # Runs a `python3` test locally; the default group would run it on a Linux RE platform.
    @buck_test(skip_for_os=["windows"])
    async def test_test_runs_on_its_group_platform(self, buck: Buck) -> None:
        await buck.test("root//groups:group_test", "--no-remote-cache")
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        test_runs = [
            e["reproducer"]["executor"] for e in entries if e["reason"] == "test.run"
        ]
        assert test_runs == ["Local"], entries

    # Runs a `python3` test locally through a rule that re-creates the test info field by field.
    @buck_test(skip_for_os=["windows"])
    async def test_test_info_copied_field_by_field(self, buck: Buck) -> None:
        await buck.test("root//groups:copied_group_test", "--no-remote-cache")
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        test_runs = [
            e["reproducer"]["executor"] for e in entries if e["reason"] == "test.run"
        ]
        assert test_runs == ["Local"], entries

    # Runs a `python3` test locally through a forwarding rule.
    @buck_test(skip_for_os=["windows"])
    async def test_test_info_forwarded_from_other_rule(self, buck: Buck) -> None:
        await buck.test("root//groups:forwarded_group_test", "--no-remote-cache")
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        test_runs = [
            e["reproducer"]["executor"] for e in entries if e["reason"] == "test.run"
        ]
        assert test_runs == ["Local"], entries

    @buck_test()
    async def test_test_group_without_platform_fails_when_run(self, buck: Buck) -> None:
        target_platform = "--target-platforms=root//config:linux_local"
        await buck.aquery("root//errors/test_group_no_platform:t", target_platform)
        await expect_failure(
            buck.test("root//errors/test_group_no_platform:t", target_platform),
            stderr_regex="No compatible execution platform",
        )

    @buck_test()
    async def test_test_info_without_executor_copied_field_by_field(
        self, buck: Buck
    ) -> None:
        target_platform = "--target-platforms=root//config:linux_local"
        await buck.build("root//errors/test_group_no_platform:copied", target_platform)
        await expect_failure(
            buck.test("root//errors/test_group_no_platform:copied", target_platform),
            stderr_regex="names execution group `TestRun`, but no executor was recorded for it when the test info was created",
        )

    @buck_test()
    async def test_test_info_with_undeclared_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.test("root//errors/test_group_undeclared:t"),
            stderr_regex="Execution group `G` is not declared by this rule",
        )

    @buck_test()
    async def test_group_constraints_select_on_target_platform(
        self, buck: Buck
    ) -> None:
        for target_platform, link_platform in [
            ("root//config:macos_local", "root//config:macos_local"),
            ("root//config:linux_local", "root//config:linux_local"),
        ]:
            node = await _cquery_node(
                buck,
                "root//groups:link_follows_target_os",
                f"--target-platforms={target_platform}",
            )
            assert {
                group: _platform(platform)
                for group, platform in node["buck.exec_group_platforms"].items()
            } == {"Link": link_platform}, target_platform

    # Runs a `python3` tool locally in an anon target; the default group is a Linux RE platform.
    @buck_test(skip_for_os=["windows"])
    async def test_anon_target_runs_on_its_group_platform(self, buck: Buck) -> None:
        await buck.build(
            "root//groups:anon_in_group",
            "root//groups:anon_targets_in_group",
            "--no-remote-cache",
        )
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        anon_links = [
            e["reproducer"]["executor"]
            for e in entries
            if re.search(r"\(anon_link[ )]", e["identity"])
        ]
        # Both targets create the same anon target, so it runs once.
        assert anon_links == ["Local"], entries

    @buck_test()
    async def test_anon_target_shared_by_creators_with_different_resolutions(
        self, buck: Buck
    ) -> None:
        targets = ["root//anon_args:shared_a", "root//anon_args:shared_b"]
        result = await buck.build(*targets)
        report = result.get_build_report()
        outputs = {report.output_for_target(t) for t in targets}
        assert len(outputs) == 1, outputs

    @buck_test()
    async def test_groupless_creator_passes_arg_tool_of_other_platform(
        self, buck: Buck
    ) -> None:
        # Without execution groups the anon target does not check the platforms of arg tools.
        await buck.build("root//anon_args:remote_arg_on_local")

    @buck_test()
    async def test_anon_target_of_rule_with_groups(self, buck: Buck) -> None:
        await expect_failure(
            buck.aquery("root//errors/anon_rule_with_groups:t"),
            stderr_regex="Rule `_grouped` declares execution groups, which anon targets do not have",
        )

    @buck_test()
    async def test_anon_target_rejects_arg_tool_of_other_platform(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.aquery("root//groups:anon_default_arg_in_link"),
            stderr_regex=r"`\$\(exe root//groups:remote_tool\)` refers to a tool built for execution platform `root//config:linux_remote`, but the anon target runs on `root//config:linux_local`",
        )
        await expect_failure(
            buck.aquery("root//groups:anon_link_arg_in_default"),
            stderr_regex=r"`\$\(exe root//groups:local_tool\)` refers to a tool built for execution platform `root//config:linux_local`, but the anon target runs on `root//config:linux_remote`",
        )

    # Runs a `python3` tool script locally.
    @buck_test(skip_for_os=["windows"])
    async def test_anon_target_runs_arg_tool_of_its_group(self, buck: Buck) -> None:
        target = "root//groups:anon_link_arg_in_link"
        result = await buck.build(target, "--no-remote-cache")
        output = result.get_build_report().output_for_target(target)
        assert output.read_text() == "local_tool"
        result = await buck.log("what-ran", "--format", "json")
        entries = [json.loads(line) for line in result.stdout.splitlines()]
        executors = [
            e["reproducer"]["executor"]
            for e in entries
            if re.search(r"\(anon_arg[ )]", e["identity"])
        ]
        assert executors == ["Local"], entries

    # Runs a `python3` tool script locally.
    @buck_test(skip_for_os=["windows"])
    async def test_anon_target_runs_toolchain_arg_tool_of_its_group(
        self, buck: Buck
    ) -> None:
        target = "root//groups:anon_toolchain_arg_in_link"
        result = await buck.build(target, "--no-remote-cache")
        output = result.get_build_report().output_for_target(target)
        assert output.read_text() == "local_tool"

    @buck_test()
    async def test_anon_target_rejects_toolchain_arg_tool_of_other_platform(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.aquery("root//groups:anon_toolchain_arg_in_default"),
            stderr_regex=r"`\$\(exe root//groups:local_tool\)` refers to a tool built for execution platform `root//config:linux_local`, but the anon target runs on `root//config:linux_remote`",
        )

    @buck_test()
    async def test_anon_target_without_group_rejects_group_tool(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.aquery("root//groups:anon_without_group"),
            stderr_regex="Exec deps and the current anon target must have the same execution platform resolution",
        )

    @buck_test()
    async def test_unbound_node_keeps_group_deps(self, buck: Buck) -> None:
        # The `select()` key `:constraint` is configured unbound, which resolves no execution
        # platforms. Its analysis reads its grouped exec dep and toolchain dep, so they must still
        # be deps.
        result = await buck.cquery(
            "root//unbound:t", "--target-platforms", "root//config:linux_local"
        )
        assert result.stdout.startswith("root//unbound:t "), result.stdout

    @buck_test()
    async def test_unbound_node_analyzes_group_exec_dep(self, buck: Buck) -> None:
        # Analysis of the unbound `:constraint` resolves its grouped exec dep and toolchain dep
        # against the unspecified execution platform resolution.
        await buck.build(
            "root//unbound:t", "--target-platforms", "root//config:linux_local"
        )

    @buck_test()
    async def test_attr_with_undeclared_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/undeclared:t"),
            stderr_regex="Attribute `_tool` uses execution group `G`, which the rule does not declare. Declared execution groups: \\[\\]",
        )

    @buck_test()
    async def test_attr_with_group_of_declared_name_from_other_file(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.uquery("root//errors/undeclared_same_name:t"),
            stderr_regex="Attribute `_tool` uses execution group `G`, which the rule does not declare. Declared execution groups: \\[`G`\\]. The declared `G` is defined in `errors/undeclared_same_name/rules.bzl`, this one in `errors/same_name/other.bzl`",
        )

    @buck_test()
    async def test_default_group_listed(self, buck: Buck) -> None:
        node = await _cquery_node(buck, "root//errors/default_listed:t")
        assert "buck.exec_group_platforms" not in node

    @buck_test()
    async def test_default_group_listed_twice(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/default_twice:t"),
            stderr_regex="Execution groups of a rule must have distinct names; `DefaultExecGroup` is listed more than once",
        )

    @buck_test()
    async def test_default_group_undeclared_dep(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/default_undeclared_dep:t"),
            stderr_regex="Attribute `_tool` uses the default execution group, which the rule does not declare",
        )

    @buck_test()
    async def test_default_group_undeclared_constraints(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/default_undeclared_constraints:t"),
            stderr_regex="Attribute `constraints` constrains the default execution group, which the rule does not declare",
        )

    @buck_test()
    async def test_default_group_undeclared_with_plugins(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/plugins_default_undeclared:t"),
            stderr_regex="The rule uses plugins, which are dependencies in the default execution group, but does not declare it",
        )

    @buck_test()
    async def test_default_group_undeclared_run(self, buck: Buck) -> None:
        await expect_failure(
            buck.aquery("root//errors/default_undeclared_run:t"),
            stderr_regex="The action runs in the default execution group, which the rule does not declare",
        )

    @buck_test()
    async def test_default_group_undeclared_write(self, buck: Buck) -> None:
        await expect_failure(
            buck.aquery("root//errors/default_undeclared_write:t"),
            stderr_regex="The action runs in the default execution group, which the rule does not declare",
        )

    @buck_test()
    async def test_default_group_undeclared_arg(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/default_undeclared_arg:t"),
            stderr_regex="Attribute `cmd` uses the default execution group, which the rule does not declare",
        )

    @buck_test()
    async def test_arg_without_exec_group_holds_flags(self, buck: Buck) -> None:
        await buck.uquery("root//errors/unmarked_arg:t")

    @buck_test()
    async def test_arg_without_exec_group_rejects_tool(self, buck: Buck) -> None:
        for package, attr, macro in [
            ("unmarked_arg_tool", "cmd", "exe"),
            ("unmarked_arg_select", "flags", "location_exec"),
        ]:
            await expect_failure(
                buck.uquery(f"root//errors/{package}:t"),
                stderr_regex=f"Target `root//errors/{package}:t`: attribute `{attr}` contains `\\$\\({macro} root//errors/{package}:tool\\)`, a tool, in an `attrs\\.arg\\(\\)` without `exec_group`",
            )

    @buck_test()
    async def test_mixed_arg_attrs_check_only_args_without_exec_group(
        self, buck: Buck
    ) -> None:
        # Tools in the args of `G` and of `DefaultExecGroup` are accepted, in values and defaults.
        await buck.uquery("root//errors/mixed_args:t")
        for package, attr in [
            ("mixed_args_tuple", "pair"),
            ("mixed_args_one_of", "either"),
            ("mixed_args_dict", "mapping"),
        ]:
            await expect_failure(
                buck.uquery(f"root//errors/{package}:t"),
                stderr_regex=f"Target `root//errors/{package}:t`: attribute `{attr}` contains `\\$\\(exe root//errors/{package}:tool\\)`, a tool, in an `attrs\\.arg\\(\\)` without `exec_group`",
            )
        await expect_failure(
            buck.uquery("root//errors/mixed_args_default:t"),
            stderr_regex="The default of attribute `with_default` contains `\\$\\(exe root//errors/mixed_args_default:tool\\)`, a tool, in an `attrs\\.arg\\(\\)` without `exec_group`",
        )

    @buck_test()
    async def test_arg_default_without_exec_group_rejects_tool(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.uquery("root//errors/unmarked_arg_default:t"),
            stderr_regex="The default of attribute `cmd` contains `\\$\\(exe root//errors/unmarked_arg_default:tool\\)`, a tool, in an `attrs\\.arg\\(\\)` without `exec_group`",
        )

    @buck_test()
    async def test_tool_attrs_without_exec_group(self, buck: Buck) -> None:
        for package, attr, function in [
            ("unmarked_exec_dep", "_tool", "exec_dep"),
            ("unmarked_toolchain_dep", "_toolchain", "toolchain_dep"),
            ("unmarked_nested", "tools", "exec_dep"),
        ]:
            await expect_failure(
                buck.uquery(f"root//errors/{package}:t"),
                stderr_regex=f"Attribute `{attr}` contains an `attrs\\.{function}\\(\\)` without `exec_group`",
            )

    @buck_test()
    async def test_default_group_undeclared_anon_target(self, buck: Buck) -> None:
        await expect_failure(
            buck.aquery("root//errors/default_undeclared_anon:t"),
            stderr_regex="The anon target uses the default execution group, which the rule does not declare",
        )

    @buck_test()
    async def test_default_group_undeclared_test(self, buck: Buck) -> None:
        # The test info fails when it is created, so a rule that returns it fails too.
        for target in ["t", "alias"]:
            await expect_failure(
                buck.test(f"root//errors/default_undeclared_test:{target}"),
                stderr_regex="The test info uses the default execution group, which the rule does not declare. Set `exec_group` or `default_executor`",
            )

    @buck_test()
    async def test_default_group_undeclared_test_with_only_executor_overrides(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.build("root//errors/default_undeclared_test:overrides_only"),
            stderr_regex="The test info uses the default execution group, which the rule does not declare. Set `exec_group` or `default_executor`",
        )

    @buck_test()
    async def test_default_group_undeclared_test_forwarded(self, buck: Buck) -> None:
        # The test info's creator has the default group, the tested target's rule does not.
        await expect_failure(
            buck.test("root//errors/default_undeclared_test:grouped_alias"),
            stderr_regex="The test uses the default execution group, which the rule of `root//errors/default_undeclared_test:grouped_alias[^`]*` does not declare \\(the test info may come from another rule that created it\\)\\. Set `exec_group` or `default_executor` where the test info is created, or add `DefaultExecGroup` to `exec_groups` of the rule of `root//errors/default_undeclared_test:grouped_alias",
        )

    @buck_test()
    async def test_default_group_undeclared_test_explicit_default(
        self, buck: Buck
    ) -> None:
        await expect_failure(
            buck.test("root//errors/default_undeclared_test:explicit_default"),
            stderr_regex="The test info uses the default execution group, which the rule does not declare. Set `exec_group` to a declared group",
        )

    @buck_test()
    async def test_duplicate_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/duplicate:t"),
            stderr_regex="Execution groups of a rule must have distinct names; `G` is listed more than once",
        )

    @buck_test()
    async def test_groups_with_same_name_from_different_files(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/same_name:t"),
            stderr_regex="Execution groups of a rule must have distinct names; `G` is listed more than once \\(defined in `errors/same_name/rules.bzl` and in `errors/same_name/other.bzl`\\)",
        )

    @buck_test()
    async def test_toolchain_rule_with_groups(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/toolchain_rule:t"),
            stderr_regex="Toolchain rules cannot use execution groups",
        )

    @buck_test()
    async def test_toolchain_rule_with_default_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/toolchain_default:t"),
            stderr_regex="Toolchain rules cannot use execution groups",
        )

    @buck_test()
    async def test_configuration_rule_with_groups(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/configuration_rule:t"),
            stderr_regex="Configuration rules cannot use execution groups",
        )

    @buck_test()
    async def test_group_named_default(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/named_default:t"),
            stderr_regex="`DefaultExecGroup` names the built-in default execution group and cannot be assigned an `exec_group\\(\\)`",
        )

    @buck_test()
    async def test_group_not_assigned_to_global(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/unbound:t"),
            stderr_regex="Execution group has not yet been assigned to a global",
        )

    @buck_test()
    async def test_run_in_undeclared_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.build("root//errors/run_undeclared:t"),
            stderr_regex="Execution group `Other` is not declared by this rule. Declared groups: \\[`Declared`\\]",
        )

    @buck_test()
    async def test_write_in_undeclared_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.build("root//errors/write_undeclared:t"),
            stderr_regex="Execution group `Other` is not declared by this rule. Declared groups: \\[`Declared`\\]",
        )

    @buck_test()
    async def test_run_with_non_group_value(self, buck: Buck) -> None:
        await expect_failure(
            buck.build("root//errors/run_wrong_type:t"),
            stderr_regex="parameter `exec_group`.*ExecGroup",
        )

    @buck_test()
    async def test_anon_target_in_group_of_unbound_node(self, buck: Buck) -> None:
        # An unbound node resolves no group: an anon target in a group fails at creation with the
        # error an action in a group fails with.
        for target in ["run", "anon"]:
            await expect_failure(
                buck.build(
                    f"root//errors/anon_unbound:{target}",
                    "--target-platforms=root//config:linux_local",
                ),
                stderr_regex="Execution platform resolution was not performed",
            )

    @buck_test()
    async def test_anon_target_in_group_without_platform(self, buck: Buck) -> None:
        for target in ["t", "plural"]:
            await expect_failure(
                buck.build(
                    f"root//errors/anon_no_group_platform:{target}",
                    "--target-platforms=root//config:linux_local",
                ),
                stderr_regex="Anon target of rule `_anon` is created in execution group `G`, which has no compatible execution platform",
            )
