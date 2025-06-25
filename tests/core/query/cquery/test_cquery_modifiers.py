# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import re
from typing import List

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


def _extract_configuration(s: str) -> List[str]:
    return re.findall(r"\((.*?)\)", s)


@buck_test()
async def test_cquery_fails_with_modifier_syntax_in_query(buck: Buck) -> None:
    await expect_failure(
        buck.cquery("root//:target?modifier"),
        stderr_regex=r"root//:target\?modifier\n\s*\^",
    )


@buck_test()
async def test_cquery_fails_with_global_modifier(buck: Buck) -> None:
    await expect_failure(
        buck.cquery(
            "root//:target",
            "--modifier",
            "root//:macos",
            "--target-universe",
            "root//:target?root//:arm",
        ),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )


@buck_test()
async def test_cquery_with_single_universe_single_modifier(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:target",
        "--target-universe",
        "root//:target?root//:macos",
    )

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_cquery_with_single_universe_multiple_modifiers(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:target",
        "--target-universe",
        "root//:target?root//:macos+root//:arm",
    )

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout
    assert "root//:arm" in cfg.stdout


@buck_test()
async def test_cquery_with_multiple_universes_single_modifier(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:target",
        "--target-universe",
        "root//:target?root//:macos,root//:target?root//:linux",
    )

    [linux_configuration, mac_configuration] = _extract_configuration(result.stdout)

    linux_cfg = await buck.audit_configurations(linux_configuration)
    assert "root//:linux" in linux_cfg.stdout

    mac_cfg = await buck.audit_configurations(mac_configuration)
    assert "root//:macos" in mac_cfg.stdout


@buck_test()
async def test_cquery_with_multiple_universes_multiple_modifier(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:target",
        "--target-universe",
        "root//:target?root//:macos+root//:arm,root//:target?root//:linux+root//:x86",
    )

    [mac_configuration, linux_configuration] = _extract_configuration(result.stdout)

    mac_cfg = await buck.audit_configurations(mac_configuration)
    assert "root//:macos" in mac_cfg.stdout
    assert "root//:arm" in mac_cfg.stdout

    linux_cfg = await buck.audit_configurations(linux_configuration)
    assert "root//:linux" in linux_cfg.stdout
    assert "root//:x86" in linux_cfg.stdout


@buck_test()
async def test_cquery_same_universe(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:target",
        "--target-universe",
        "root//:target?root//:macos,root//:target?root//:macos",
    )

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)

    assert "root//:macos" in cfg.stdout


@buck_test()
async def test_cquery_order_of_modifiers(buck: Buck) -> None:
    result = await buck.cquery(
        "root//:target",
        "--target-universe",
        "root//:target?root//:linux+root//:macos",
    )

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout


# There should be an error thrown whenever ?modifier syntax is used outside of a target universe.
# However, the ? character can still show up in regex expressions and those should still be considered valid queries.
@buck_test()
async def test_cquery_with_attrregexfilter(buck: Buck) -> None:
    result = await buck.cquery(
        "attrregexfilter(attribute, 'test_greedys?', root//:attr_regex)"
    )

    assert "root//:attr_regex" in result.stdout

    result = await buck.cquery(
        "attrregexfilter(attribute, 'test_greedys?', root//:attr_regex)",
        "--target-universe",
        "root//:attr_regex?root//:macos",
    )

    [configuration] = _extract_configuration(result.stdout)

    cfg = await buck.audit_configurations(configuration)
    assert "root//:macos" in cfg.stdout
