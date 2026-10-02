# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


def set_default_intersection(buck: Buck, mode: str) -> None:
    (buck.cwd / ".bucksettings.toml").write_text(
        f'[package_visibility]\ndefault_intersection = "{mode}"\n'
    )


@buck_test()
async def test_optin_inside_consumer_can_depend_on_public_target(
    buck: Buck,
) -> None:
    # PUBLIC clipped to the intersection; inside consumer matches.
    await buck.ctargets("root//intersect/inside_consumer:c")


@buck_test()
async def test_optin_clips_public_target_for_outside_consumer(
    buck: Buck,
) -> None:
    # PUBLIC silently clipped (not rejected); outside consumer fails.
    # Locks the diagnostic: error mentions visibility attr and the restriction.
    result = await expect_failure(
        buck.ctargets("root//outside_consumer:c"),
        stderr_regex=r"is not visible to.*visibility = .*Restricted to.*by an ancestor PACKAGE's visibility",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_clips_public_target_for_outside_consumer.golden.txt",
    )


@buck_test()
async def test_optin_intersection_blocks_visibility_leak_outside(
    buck: Buck,
) -> None:
    # Target's own `visibility` lists `leak_destination/...`; the
    # intersection blocks it.
    # Differs from `enforce_strict_visibility` which would allow this leak.
    result = await expect_failure(
        buck.ctargets("root//leak_destination/consumer:c"),
        stderr_regex=r"is not visible to.*Restricted to.*by an ancestor PACKAGE's visibility",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_intersection_blocks_visibility_leak_outside.golden.txt",
    )


@buck_test()
async def test_optin_target_own_visibility_match_passes(buck: Buck) -> None:
    # Consumer matches both visibility attr and intersection.
    await buck.ctargets("root//intersect/sub_b/consumer:c")


@buck_test()
async def test_inherit_true_child_can_still_tighten(buck: Buck) -> None:
    # Regression: with `inherit=True`, the child contributes its EXPLICIT
    # `visibility=B` to the intersection (not `parent.visibility ∪ B`), so a
    # tighter child intersection is not silently absorbed into the parent's.
    result = await expect_failure(
        buck.ctargets("root//inherit_test/other/consumer:c"),
        stderr_regex=r"is not visible to",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_inherit_true_child_can_still_tighten.golden.txt",
    )
    await buck.ctargets("root//inherit_test/restricted_child/inside/consumer:c")


@buck_test()
async def test_package_with_omitted_visibility_does_not_empty_intersection(
    buck: Buck,
) -> None:
    # Regression: `package(inherit=True, within_view=[...])` (no `visibility=`)
    # combined with `enforce_visibility_intersection()` must NOT contribute an
    # empty list to the intersection. Before the fix, the omitted `visibility=`
    # defaulted to `[]` and was treated as an explicit empty contribution,
    # narrowing the intersection down to the empty set and blocking all
    # consumers.
    await buck.ctargets("root//inherit_test/no_vis_child/inside_consumer:c")


@buck_test()
async def test_optin_preserves_parent_within_view(buck: Buck) -> None:
    # Regression: opt-in must not widen the inherited `within_view` to PUBLIC
    # (would happen if implementation routed through `package(...)`).
    await buck.ctargets("root//within_view_preserve/child/ok_dep:c")
    result = await expect_failure(
        buck.ctargets("root//within_view_preserve/child_bad/bad_dep:c"),
        stderr_regex=r"within_view",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_preserves_parent_within_view.golden.txt",
    )


@buck_test()
async def test_call_from_bzl_is_rejected(buck: Buck) -> None:
    result = await expect_failure(
        buck.ctargets("root//indirect_call/leaf:t"),
        stderr_regex=r"`enforce_visibility_intersection\(\)` can only be called from a `PACKAGE` file",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_call_from_bzl_is_rejected.golden.txt",
    )


@buck_test()
async def test_calling_twice_in_one_package_is_rejected(buck: Buck) -> None:
    # A second `enforce_visibility_intersection()` in the same PACKAGE file fails.
    result = await expect_failure(
        buck.ctargets("root//at_most_once/leaf:t"),
        stderr_regex=r"`enforce_visibility_intersection\(\)` function can be used at most once per `PACKAGE` file",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_calling_twice_in_one_package_is_rejected.golden.txt",
    )


@buck_test()
@env("BUCK2_HARD_ERROR", "false")
async def test_audit_mode_still_enforces_marker_boundary(buck: Buck) -> None:
    set_default_intersection(buck, "audit")
    for consumer in ["root//outside_consumer:c", "root//leak_destination/consumer:c"]:
        await expect_failure(
            buck.ctargets(consumer),
            stderr_regex=r"is not visible to.*Restricted to.*by an ancestor PACKAGE's visibility",
        )


@buck_test()
@env("BUCK2_HARD_ERROR", "false")
async def test_audit_mode_still_enforces_own_visibility(buck: Buck) -> None:
    set_default_intersection(buck, "audit")
    await expect_failure(
        buck.ctargets("root//intersect/sub_a/consumer:c"),
        stderr_regex=r"is not visible to",
    )
