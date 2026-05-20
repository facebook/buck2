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
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr


@buck_test()
async def test_optin_inside_consumer_can_depend_on_public_target(
    buck: Buck,
) -> None:
    # PUBLIC clipped to cap; inside consumer matches.
    await buck.ctargets("root//intersect/inside_consumer:c")


@buck_test()
async def test_optin_clips_public_target_for_outside_consumer(
    buck: Buck,
) -> None:
    # PUBLIC silently clipped (not rejected); outside consumer fails.
    result = await expect_failure(
        buck.ctargets("root//outside_consumer:c"),
        stderr_regex=r"is not visible to",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_clips_public_target_for_outside_consumer.golden.txt",
    )


@buck_test()
async def test_optin_cap_blocks_target_visibility_leaking_outside_cap(
    buck: Buck,
) -> None:
    # Target's own `visibility` lists `leak_destination/...`; cap blocks it.
    # Differs from `enforce_strict_visibility` which would allow this leak.
    result = await expect_failure(
        buck.ctargets("root//leak_destination/consumer:c"),
        stderr_regex=r"is not visible to",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_optin_cap_blocks_target_visibility_leaking_outside_cap.golden.txt",
    )


@buck_test()
async def test_optin_target_own_visibility_match_passes(buck: Buck) -> None:
    # Consumer matches both visibility attr and cap.
    await buck.ctargets("root//intersect/sub_b/consumer:c")


@buck_test()
async def test_inherit_true_child_can_still_tighten_cap(buck: Buck) -> None:
    # Regression: with `inherit=True`, the child contributes its EXPLICIT
    # `visibility=B` to the cap (not `parent.visibility ∪ B`), so a
    # tighter child cap is not silently absorbed into the parent's.
    result = await expect_failure(
        buck.ctargets("root//inherit_test/other/consumer:c"),
        stderr_regex=r"is not visible to",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/test_inherit_true_child_can_still_tighten_cap.golden.txt",
    )
    await buck.ctargets("root//inherit_test/restricted_child/inside/consumer:c")


@buck_test()
async def test_package_with_omitted_visibility_does_not_empty_cap(
    buck: Buck,
) -> None:
    # Regression: `package(inherit=True, within_view=[...])` (no `visibility=`)
    # combined with `enforce_visibility_intersection()` must NOT contribute an
    # empty list to the cap. Before the fix, the omitted `visibility=` defaulted
    # to `[]` and was treated as an explicit empty contribution, intersecting
    # the cap down to the empty set and blocking all consumers.
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
