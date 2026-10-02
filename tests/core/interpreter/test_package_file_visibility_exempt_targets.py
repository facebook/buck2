# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env


_PACKAGE_VISIBILITY_SETTINGS: dict[str, str] = {
    "audit": '[package_visibility]\ndefault_intersection = "audit"\n',
    "enforce": '[package_visibility]\ndefault_intersection = "enforce"\n',
}


def set_default_intersection(buck: Buck, mode: str) -> None:
    # Read at daemon startup, so this must run before the first command.
    (buck.cwd / ".bucksettings.toml").write_text(_PACKAGE_VISIBILITY_SETTINGS[mode])


@buck_test()
async def test_off_mode_boundary_is_dormant(buck: Buck) -> None:
    await buck.ctargets("root//outside:uses_legacy", "root//outside:uses_strict")


@buck_test()
async def test_enforce_mode_exempt_package_stays_visible(buck: Buck) -> None:
    set_default_intersection(buck, "enforce")
    await buck.ctargets("root//outside:uses_legacy")
    await expect_failure(
        buck.ctargets("root//outside:uses_strict"),
        stderr_regex=r"`root//boundary/strict:t` is not visible to",
    )


@buck_test()
@env("BUCK2_HARD_ERROR", "false")
async def test_audit_mode_reports_instead_of_failing(buck: Buck) -> None:
    set_default_intersection(buck, "audit")
    await buck.ctargets("root//outside:uses_legacy")
    result = await buck.ctargets("root//outside:uses_strict")
    assert "package_visibility_audit_would_block" in result.stderr
    assert "root//boundary/strict:t" in result.stderr


@buck_test()
async def test_exemption_does_not_weaken_ancestor_layer(buck: Buck) -> None:
    await expect_failure(
        buck.ctargets("root//outside:uses_marked_legacy"),
        stderr_regex=r"`root//marked/inner/legacy:t` is not visible to",
    )


@buck_test()
async def test_exemption_does_not_lift_inherited_default_visibility(buck: Buck) -> None:
    set_default_intersection(buck, "enforce")
    await expect_failure(
        buck.ctargets("root//outside:uses_legacy_inherits_default"),
        stderr_regex=r"`root//boundary/legacy:inherits_default` is not visible to",
    )


@buck_test()
async def test_enforce_mode_nested_cell_inherits_parent_layer(buck: Buck) -> None:
    await buck.ctargets("root//outside:uses_nested_cell")
    set_default_intersection(buck, "enforce")
    await buck.kill()
    await expect_failure(
        buck.ctargets("root//outside:uses_nested_cell"),
        stderr_regex=r"`nested//:t` is not visible to",
    )


@buck_test()
@pytest.mark.parametrize(
    "package, stderr_regex",
    [
        ("requires_visibility", r"requires an explicit `visibility=`"),
        ("public_layer", r"is vacuous with a `visibility` list containing"),
        ("target_pattern", r"target patterns \(`cell//pkg:name`\) are not accepted"),
        ("outside_subtree", r"is outside this PACKAGE's subtree"),
        ("covers_subtree", r"covers this PACKAGE's entire subtree"),
    ],
)
async def test_invalid_exemption_is_rejected(
    buck: Buck, package: str, stderr_regex: str
) -> None:
    await expect_failure(
        buck.targets(f"root//{package}:t"),
        stderr_regex=stderr_regex,
    )
