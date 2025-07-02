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
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


GOLDEN_DIRECTORY = "modifiers/golden/"


@buck_test(data_dir="sorted")
async def test_listed_providers_are_sorted(buck: Buck) -> None:
    result = await buck.audit("providers", "//:target", "--list")

    # "  - DefaultInfo" -> "DefaultInfo"
    providers = [
        line.split("-")[1].strip()
        for line in result.stdout.split("\n")
        if line.strip().startswith("-")
    ]
    assert providers == [
        "AlphaInfo",
        "DefaultInfo",
        "ZetaInfo",
    ]


@buck_test(data_dir="universe")
async def test_audit_providers_universe(buck: Buck) -> None:
    result = await buck.audit("providers", "//:aaa", "--quiet")
    assert "root//:aaa (root//:p-aaa#<HASH>)" == _replace_hash(result.stdout.strip())

    result = await buck.audit(
        "providers", "//:aaa", "--target-universe=//:bbb", "--quiet"
    )
    assert "root//:aaa (root//:p-bbb#<HASH>)" == _replace_hash(result.stdout.strip())


@buck_test(data_dir="modifiers")
async def test_audit_providers_with_single_modifier(buck: Buck) -> None:
    result = await buck.audit("providers", "//:dummy?//:macos")

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path=GOLDEN_DIRECTORY + "audit_providers_with_single_modifier.golden.txt",
    )


@buck_test(data_dir="modifiers")
async def test_audit_providers_with_multiple_modifiers(buck: Buck) -> None:
    result = await buck.audit("providers", "//:dummy?//:macos+//:arm")

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path=GOLDEN_DIRECTORY
        + "audit_providers_with_multiple_modifiers.golden.txt",
    )


@buck_test(data_dir="modifiers")
async def test_audit_providers_order_of_modifiers(buck: Buck) -> None:
    # if passing in modifiers of the same constraint setting,
    # the last one should be the one that applies
    result = await buck.audit("providers", "//:dummy?//:macos+//:linux")

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path=GOLDEN_DIRECTORY + "audit_providers_order_of_modifiers.golden.txt",
    )


@buck_test(data_dir="modifiers")
async def test_audit_providers_all_targets_with_modifiers(buck: Buck) -> None:
    result = await buck.audit("providers", "//:?//:macos")

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path=GOLDEN_DIRECTORY
        + "audit_providers_all_targets_with_modifiers.golden.txt",
    )


@buck_test(data_dir="modifiers")
async def test_audit_providers_recursive_with_modifiers(buck: Buck) -> None:
    result = await buck.audit("providers", "//...?//:macos")

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path=GOLDEN_DIRECTORY
        + "audit_providers_recursive_with_modifiers.golden.txt",
    )


@buck_test(data_dir="modifiers")
async def test_audit_providers_modifiers_with_subtarget(buck: Buck) -> None:
    result = await buck.audit("providers", "//:dummy_with_subtarget[sub]?//:macos")

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path=GOLDEN_DIRECTORY
        + "audit_providers_modifiers_with_subtarget.golden.txt",
    )


@buck_test(data_dir="modifiers")
async def test_audit_providers_modifiers_fail_with_global(buck: Buck) -> None:
    await expect_failure(
        buck.audit("providers", "--modifier", "//:linux", "//:dummy?//:arm"),
        stderr_regex=r"Cannot specify modifiers with \?modifier syntax when global CLI modifiers are set with --modifier flag",
    )
