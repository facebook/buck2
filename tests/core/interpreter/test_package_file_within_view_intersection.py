# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr

# `within_view` is enforced while the build file is evaluated, so `uquery` is
# enough to exercise it (no configuration needed).

# Inside an opted-in subtree every refusal names both the target's own list and
# the cap; outside one, the message is the ordinary `within_view` error.
CAP_REGEX = (
    r"Target's effective `within_view` does not allow dependency"
    r".*Capped to.*enforce_within_view_intersection"
)
OWN_LIST_REGEX = r"Target's `within_view` attribute does not allow dependency"


@buck_test()
async def test_optin_inside_dep_is_allowed(buck: Buck) -> None:
    await buck.uquery("root//intersect/inside_dep:c")


@buck_test()
async def test_optin_cap_refuses_public_per_target_within_view(buck: Buck) -> None:
    # A per-target `within_view = ["PUBLIC"]` replaces the PACKAGE default but
    # cannot escape the cap. Locks the diagnostic: the target's own list and
    # the cap are both named.
    result = await expect_failure(
        buck.uquery("root//intersect/public_target:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_cap_refuses_public_per_target_within_view.golden.txt",
    )


@buck_test()
async def test_optin_cap_refuses_per_target_within_view_listing_outside_dep(
    buck: Buck,
) -> None:
    # The target's own `within_view` explicitly names `outside/...`; the cap
    # still refuses it.
    result = await expect_failure(
        buck.uquery("root//intersect/leaky_target:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_cap_refuses_per_target_within_view_listing_outside_dep.golden.txt",
    )


@buck_test()
async def test_per_target_within_view_narrower_than_cap_wins(buck: Buck) -> None:
    # The target's own list is what refuses the dep here (the dep is inside the
    # cap). Inside an opted-in subtree the message still names the cap next to
    # the target's own list, as `enforce_visibility_intersection()` does.
    result = await expect_failure(
        buck.uquery("root//intersect/narrow_target:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_per_target_within_view_narrower_than_cap_wins.golden.txt",
    )
    await buck.uquery("root//intersect/narrow_target_ok:c")


@buck_test()
async def test_nested_package_public_within_view_is_still_capped(
    buck: Buck,
) -> None:
    # `package(inherit = False, within_view = ["PUBLIC"])` in a descendant
    # replaces the default but not the cap.
    await buck.uquery("root//intersect/intermediate/leaf_ok:c")
    result = await expect_failure(
        buck.uquery("root//intersect/intermediate/leaf_bad:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_nested_package_public_within_view_is_still_capped.golden.txt",
    )


@buck_test()
async def test_intersect_grandparent_parent_child(buck: Buck) -> None:
    # Three opted-in levels: the effective cap is the AND of all three lists.
    await buck.uquery("root//intersect/child/grandchild/ok:c")
    result = await expect_failure(
        buck.uquery("root//intersect/child/grandchild/bad:c"),
        stderr_regex=r"Capped to .* AND .* AND .* by `enforce_within_view_intersection\(\)`",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_intersect_grandparent_parent_child.golden.txt",
    )


@buck_test()
async def test_optin_only_package_keeps_parent_default_and_cap(buck: Buck) -> None:
    # A PACKAGE that only calls `enforce_within_view_intersection()` (no
    # `package()`) neither widens the inherited `within_view` default nor
    # changes the cap. The default alone refuses the outside dep; the message
    # names the (inherited) cap as well.
    await buck.uquery("root//intersect/optin_only/ok:c")
    result = await expect_failure(
        buck.uquery("root//intersect/optin_only/bad:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_only_package_keeps_parent_default_and_cap.golden.txt",
    )


@buck_test()
async def test_optin_without_within_view_propagates_parent_cap(buck: Buck) -> None:
    # `package(inherit = True, visibility = [...])` (no `within_view=`) with
    # `enforce_within_view_intersection()` contributes nothing to the cap. The
    # parent's cap still applies -- and catches the PUBLIC default that
    # `inherit = True` with an omitted `within_view` produces.
    await buck.uquery("root//inherit_test/no_wv_child/inside_dep:c")
    result = await expect_failure(
        buck.uquery("root//inherit_test/no_wv_child/outside_dep:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_without_within_view_propagates_parent_cap.golden.txt",
    )


@buck_test()
async def test_inherit_true_child_can_still_tighten_cap(buck: Buck) -> None:
    # With `inherit = True`, the child contributes its EXPLICIT `within_view`
    # to the cap (not `parent ∪ child`), so the cap tightens even though the
    # default `within_view` below is the union.
    await buck.uquery("root//inherit_test/restricted_child/inside:c")
    result = await expect_failure(
        buck.uquery("root//inherit_test/restricted_child/escaping:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_inherit_true_child_can_still_tighten_cap.golden.txt",
    )


@buck_test()
async def test_public_is_identity(buck: Buck) -> None:
    await buck.uquery("root//public_identity/consumer:c")
    stdout = (await buck.audit("package-values", "root//public_identity")).stdout
    assert json.loads(stdout)["root//public_identity"]["within_view_cap"] == ["PUBLIC"]


@buck_test()
async def test_same_package_deps_are_exempt(buck: Buck) -> None:
    await buck.uquery("root//same_package:c")


@buck_test()
async def test_select_arm_is_capped(buck: Buck) -> None:
    result = await expect_failure(
        buck.uquery("root//select_arm:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_select_arm_is_capped.golden.txt",
    )


@buck_test()
async def test_exec_and_toolchain_deps_are_capped(buck: Buck) -> None:
    # `within_view` covers every dep-typed attribute, exec and toolchain deps
    # included; so does the cap.
    result = await expect_failure(
        buck.uquery("root//exec_dep:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_exec_dep_is_capped.golden.txt",
    )
    result = await expect_failure(
        buck.uquery("root//exec_dep/toolchain:c"),
        stderr_regex=CAP_REGEX,
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_toolchain_dep_is_capped.golden.txt",
    )


@buck_test()
async def test_outside_optin_message_is_unchanged(buck: Buck) -> None:
    # No ancestor opted in: the ordinary `within_view` error, without any
    # mention of a cap.
    result = await expect_failure(
        buck.uquery("root//no_optin/bad:c"),
        stderr_regex=OWN_LIST_REGEX,
    )
    assert "Capped to" not in result.stderr
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_outside_optin_message_is_unchanged.golden.txt",
    )


@buck_test()
async def test_audit_package_values_shows_within_view_cap(buck: Buck) -> None:
    stdout = (
        await buck.audit(
            "package-values",
            "root//intersect",
            "root//intersect/intermediate",
            "root//intersect/child/grandchild",
        )
    ).stdout
    result = json.loads(stdout)
    assert result["root//intersect"]["within_view_cap"] == ["root//intersect/..."]
    # A non-opted-in descendant inherits the cap unchanged, while its own
    # `within_view` is what its `package()` call said.
    assert result["root//intersect/intermediate"]["within_view_cap"] == [
        "root//intersect/..."
    ]
    assert result["root//intersect/intermediate"]["within_view"] == ["PUBLIC"]
    assert result["root//intersect/child/grandchild"]["within_view_cap"] == {
        "intersection": [
            ["root//intersect/..."],
            ["root//intersect/child/...", "root//outside/..."],
            [
                "root//intersect/child/grandchild/...",
                "root//intersect/child/lib:",
                "root//outside/...",
            ],
        ]
    }


@buck_test()
async def test_call_from_bzl_is_rejected(buck: Buck) -> None:
    result = await expect_failure(
        buck.uquery("root//indirect_call/leaf:t"),
        stderr_regex=r"`enforce_within_view_intersection\(\)` can only be called from a `PACKAGE` file",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_call_from_bzl_is_rejected.golden.txt",
    )


@buck_test()
async def test_calling_twice_in_one_package_is_rejected(buck: Buck) -> None:
    result = await expect_failure(
        buck.uquery("root//at_most_once/leaf:t"),
        stderr_regex=r"`enforce_within_view_intersection\(\)` function can be used at most once per `PACKAGE` file",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_calling_twice_in_one_package_is_rejected.golden.txt",
    )
