# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


def _platform(platform: str) -> str:
    """Strips the configuration hash from a platform reported by cquery."""
    return platform.split("#")[0]


class ExecutionGroupsTest(BuckTestCase):
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
    async def test_unbound_node_keeps_group_deps(self, buck: Buck) -> None:
        # The `select()` key `:constraint` is configured unbound, which resolves no execution
        # platforms. Its analysis reads its grouped exec dep, so that dep must still be a dep.
        result = await buck.cquery(
            "root//unbound:t", "--target-platforms", "root//config:linux_local"
        )
        assert result.stdout.startswith("root//unbound:t "), result.stdout

    @buck_test()
    async def test_attr_with_undeclared_group(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//errors/undeclared:t"),
            stderr_regex="Attribute `_tool` uses execution group `G`, which the rule does not declare. Declared execution groups: \\[\\]",
        )

    @buck_test()
    async def test_default_group_listed(self, buck: Buck) -> None:
        await buck.uquery("root//errors/default_listed:t")

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
