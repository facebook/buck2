# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict
import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def _extract_configuration(s: str) -> list[str]:
    return re.findall(r"\((.*?)\)", s)


@buck_test()
async def test_ctargets_modifier_single_pattern(buck: Buck) -> None:
    result = await buck.ctargets("root//:target?root//:macos")

    [configuration] = _extract_configuration(result.stdout)

    macos_cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in macos_cfg.stdout


@buck_test()
async def test_ctargets_modifier_multiple_patterns(buck: Buck) -> None:
    result = await buck.ctargets(
        "root//:target?root//:macos", "root//:other_target?root//:macos"
    )

    [target_configuration, other_configuration] = _extract_configuration(result.stdout)

    target_cfg = await buck.audit_configurations(target_configuration)
    assert "root//:macos" in target_cfg.stdout

    other_cfg = await buck.audit_configurations(other_configuration)
    assert "root//:macos" in other_cfg.stdout


@buck_test()
async def test_ctargets_modifier_multiple_modifiers(buck: Buck) -> None:
    result = await buck.ctargets("root//:target?root//:macos+root//:arm")

    [configuration] = _extract_configuration(result.stdout)

    multi_cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in multi_cfg.stdout
    assert "root//:arm" in multi_cfg.stdout


@buck_test()
async def test_ctargets_modifier_order_of_modifiers(buck: Buck) -> None:
    # if passing in modifiers of the same constraint setting,
    # the last one should be the one that applies
    result = await buck.ctargets("root//:target?root//:macos+root//:linux")

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)

    assert "root//:linux" in cfg.stdout
    assert "root//:macos" not in cfg.stdout


@buck_test()
async def test_ctargets_modifier_multi_target_pattern(buck: Buck) -> None:
    result = await buck.ctargets("root//:?root//:macos")

    [other_configuration, _, configuration] = _extract_configuration(result.stdout)[5:]

    other_cfg = await buck.audit_configurations(other_configuration)
    assert "root//:macos" in other_cfg.stdout

    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_ctargets_modifier_same_target(buck: Buck) -> None:
    # if the same target has the same modifiers there should only be one instance of it
    result = await buck.ctargets(
        "root//:target?root//:macos", "root//:target?root//:macos"
    )

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_ctargets_fails_with_global_modifier(buck: Buck) -> None:
    await expect_failure(
        buck.ctargets("--modifier", "root//:linux", "root//:target?root//:macos"),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )
