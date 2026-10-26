# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json
from typing import Any

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash


async def _cquery_node(buck: Buck, target: str) -> dict[str, Any]:
    result = await buck.cquery("--output-all-attributes", target)
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
