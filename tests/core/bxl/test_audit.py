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


@buck_test()
async def test_bxl_audit_output(buck: Buck) -> None:
    await buck.bxl(
        "//audit.bxl:audit_output_action_exists",
    )

    await buck.bxl(
        "//audit.bxl:audit_output_config_not_match",
    )

    await expect_failure(
        buck.bxl(
            "//audit.bxl:audit_output_invalid_path",
        ),
        stderr_regex="Malformed buck-out path",
    )


@buck_test()
async def test_bxl_audit_content_based_output(buck: Buck) -> None:
    label = "root//:with_content_based_output"
    result = await buck.build(label, "--show-output")
    path = result.get_build_report().output_for_target(label)

    # resolve the symlink that we get as the output from buck to find the underlying content-based path.
    path = (buck.cwd / path).resolve()
    # make it a relative path again
    path = path.relative_to(buck.cwd)

    await buck.bxl(
        "//audit.bxl:audit_content_based_output_action_exists",
        "--",
        "--label",
        label,
        "--path",
        path.as_posix(),
    )
