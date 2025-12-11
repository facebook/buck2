# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import (
    golden,
    golden_replace_cfg_hash,
    sanitize_stderr,
)


@buck_test()
async def test_ctargets_json_report_basic(buck: Buck) -> None:
    """Test basic --json-report with only compatible targets"""
    result = await buck.ctargets(
        "//a:target1",
        "//a:target2",
        "--target-platforms=root//:p",
        "--json-report",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/basic.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/basic.stderr.golden",
    )


@buck_test()
async def test_ctargets_json_report_with_incompatible(buck: Buck) -> None:
    """Test --json-report with incompatible targets"""
    result = await buck.ctargets(
        "//a:target1",
        "//a:macos_only",
        "//a:target2",
        "--target-platforms=root//:linux_platform",
        "--json-report",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/with_incompatible.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/with_incompatible.stderr.golden",
    )


@buck_test()
async def test_ctargets_json_report_with_transitive_incompatible(buck: Buck) -> None:
    """Test --json-report with transitively incompatible targets"""
    result = await buck.ctargets(
        "//a:target1",
        "//c:depends_on_incompatible",
        "//a:target2",
        "--target-platforms=root//:linux_platform",
        "--keep-going",
        "--json-report",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/transitive_incompatible.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/transitive_incompatible.stderr.golden",
    )


@buck_test()
async def test_ctargets_json_report_with_errors_and_keep_going(buck: Buck) -> None:
    """Test --json-report with errors (should only appear in stderr, not JSON)"""
    result = await buck.ctargets(
        "//a:target1",
        "//b:any",
        "//a:target2",
        "--target-platforms=root//:p",
        "--keep-going",
        "--json-report",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/with_errors.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/with_errors.stderr.golden",
    )


@buck_test()
async def test_ctargets_json_report_mixed(buck: Buck) -> None:
    """Test --json-report with mix of compatible, incompatible, and errors"""
    result = await buck.ctargets(
        "//a:target1",
        "//a:macos_only",
        "//b:any",
        "//a:target2",
        "//c:depends_on_incompatible",
        "--target-platforms=root//:linux_platform",
        "--keep-going",
        "--json-report",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/mixed.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/mixed.stderr.golden",
    )


@buck_test()
async def test_ctargets_json_report_only_incompatible(buck: Buck) -> None:
    """Test --json-report when all targets are incompatible"""
    result = await buck.ctargets(
        "//a:macos_only",
        "--target-platforms=root//:linux_platform",
        "--json-report",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/only_incompatible.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/only_incompatible.stderr.golden",
    )


@buck_test()
async def test_ctargets_json_report_with_attributes(buck: Buck) -> None:
    """Test --json-report with attribute filtering"""
    result = await buck.ctargets(
        "//a:target1",
        "//a:target2",
        "--target-platforms=root//:p",
        "--json-report",
        "--output-attribute",
        "^name$",
    )

    golden_replace_cfg_hash(
        output=result.stdout,
        rel_path="golden/with_attributes.stdout.golden",
    )
    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="golden/with_attributes.stderr.golden",
    )
