# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=False)
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