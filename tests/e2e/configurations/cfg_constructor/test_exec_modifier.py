# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
async def test_exec_modifier(buck: Buck) -> None:
    await buck.bxl(
        "//test_exec_modifier.bxl:main",
        "-c",
        "buck2.preserve_exec_platform_marker=true",
    )


@buck_test(inplace=False)
async def test_exec_modifier_with_constructor_dropping_marker(buck: Buck) -> None:
    await buck.bxl(
        "//test_exec_modifier.bxl:main",
        "-c",
        "test.drop_constructor_marker=true",
        "-c",
        "buck2.preserve_exec_platform_marker=true",
    )


@buck_test(inplace=False)
async def test_exec_modifier_rejects_conflicting_transition_marker(buck: Buck) -> None:
    for transition in ("single", "split"):
        await expect_failure(
            buck.cquery(
                f"deps(//:binary_with_{transition}_transition_exec_dep)",
                "-c",
                "test.replace_transition_marker=true",
                "-c",
                "buck2.preserve_exec_platform_marker=true",
            ),
            stderr_regex=(
                r"cannot change exec marker .* from .*is_exec_platform\[true\].*"
                r"to .*is_exec_platform\[false\]"
            ),
        )


@buck_test(inplace=False)
async def test_exec_modifier_rejects_conflicting_constructor_marker(buck: Buck) -> None:
    await expect_failure(
        buck.cquery(
            "deps(//:binary)",
            "-c",
            "test.replace_constructor_marker=true",
            "-c",
            "buck2.preserve_exec_platform_marker=true",
        ),
        stderr_regex=(
            r"cannot change exec marker .* from .*is_exec_platform\[true\].*"
            r"to .*is_exec_platform\[false\]"
        ),
    )


@buck_test(inplace=False)
async def test_exec_modifier_legacy(buck: Buck) -> None:
    for config in ([], ["-c", "buck2.preserve_exec_platform_marker=false"]):
        await buck.bxl("//test_exec_modifier.bxl:legacy", *config)
        await expect_failure(
            buck.cquery("deps(//:binary_with_single_transition_exec_dep)", *config),
            stderr_regex="did not produce identical `PlatformInfo`",
        )
        for mutation, entrypoint in (
            ("test.replace_transition_marker=true", "legacy_marker"),
            ("test.drop_constructor_marker=true", "legacy_constructor_marker"),
            ("test.replace_constructor_marker=true", "legacy_constructor_marker"),
        ):
            await buck.bxl(
                f"//test_exec_modifier.bxl:{entrypoint}", *config, "-c", mutation
            )


@buck_test(inplace=False)
async def test_exec_marker_flag_changes_in_one_daemon(buck: Buck) -> None:
    for enabled in (True, False, True):
        entrypoint = "preserved_marker" if enabled else "legacy_marker"
        await buck.bxl(
            f"//test_exec_modifier.bxl:{entrypoint}",
            "-c",
            f"buck2.preserve_exec_platform_marker={str(enabled).lower()}",
        )
