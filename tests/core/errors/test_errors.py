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
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.golden import golden


@buck_test()
async def test_soft_error(buck: Buck) -> None:
    await expect_failure(
        buck.targets(":"), stderr_regex="starlark_raised_soft_error.*Will be reported"
    )


@buck_test()
@env("BUCK2_HARD_ERROR", "false")
async def test_soft_error_quiet(buck: Buck) -> None:
    res = await buck.targets("quiet:", ":")
    assert "starlark_raised_soft_error" in res.stderr
    assert "starlark_quiet_soft_error" not in res.stderr


@buck_test()
@env("BUCK2_HARD_ERROR", "false")
async def test_soft_error_no_stack(buck: Buck) -> None:
    res = await buck.targets(":")
    assert "Traceback" in res.stderr

    res = await buck.targets("no_stack:")
    assert "Traceback" not in res.stderr


@buck_test(
    # windows errors are slightly different, just skip for now
    skip_for_os=["windows"],
)
@env("BUCK2_HARD_ERROR", "false")
async def test_package_listing_errors(buck: Buck) -> None:
    outs = []
    for target in [
        # //package_listing/missing does not exist
        "//package_listing/missing/foo/x/y/lmnop:target",
        # //package_listing/ignored is ignored
        "//package_listing/ignored/foo/x/y/lmnop:target",
        # //package_listing/cell is a cell
        "//package_listing/cell/foo/x/y/lmnop:target",
        # //package_listing/missing_targets_file has no TARGETS file
        "//package_listing/missing_targets_file:target",
        # //package_listing/data.file is a file
        "//package_listing/data.file:target",
        "//package_listing/data.file/subdir:target",
        # Missing directory due to typo
        "//package_listings:",
        # Missing directory due to typo shows full path
        "//package_listing/data.file:targets",
        # Missing directory due to being in the wrong cell
        "//something:",
    ]:
        out = await expect_failure(buck.uquery(target, "-v=0", "--console=none"))
        stripped_stderr = re.sub(
            "read_dir(.*)", "read_dir(<stripped absolute path>)", out.stderr
        )
        outs.append(stripped_stderr)

    golden(output="\n\n\n".join(outs), rel_path="package_listing/expected.golden.out")


@buck_test(
    # windows errors are slightly different, just skip for now
    skip_for_os=["windows"],
)
async def test_configured_graph_deps_collapsed_in_errors(buck: Buck) -> None:
    out = await expect_failure(
        buck.cquery(
            "//deps_collapsed:top",
            "-v=0",
            "--console=none",
            "-c",
            "build.execution_platforms=root//deps_collapsed:exec_platforms",
        )
    )
    stderr = re.sub("#[a-f0-9]*\\)", "#00000000)", out.stderr)
    golden(output=stderr, rel_path="deps_collapsed/expected.golden.out")


@buck_test(
    # windows errors are slightly different, just skip for now
    skip_for_os=["windows"],
)
async def test_configured_graph_deps_collapsed_in_errors_2(buck: Buck) -> None:
    out = await expect_failure(
        buck.cquery(
            "//deps_collapsed:top",
            "-v=0",
            "--console=none",
            "-c",
            "build.execution_platforms=root//deps_collapsed:exec_platforms",
            "-c",
            "core_test_errors.broken_select_in_toolchain=1",
        )
    )
    stderr = re.sub("#[a-f0-9]*\\)", "#00000000)", out.stderr)
    golden(output=stderr, rel_path="deps_collapsed/expected_2.golden.out")
