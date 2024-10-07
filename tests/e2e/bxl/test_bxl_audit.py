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

from buck2.tests.e2e_util.helper.utils import is_running_on_linux


@buck_test(inplace=True)
async def test_bxl_audit_output(buck: Buck) -> None:
    result = await buck.bxl(
        "//buck2/tests/targets/bxl/simple/bxl/audit.bxl:audit_output_action_exists",
    )
    assert "fbcode//buck2/tests/targets/rules/shell:gen" in result.stdout
    assert "ovr_config//platform/linux:x86_64-fbcode-platform" in result.stdout

    if is_running_on_linux():
        result = await buck.bxl(
            "//buck2/tests/targets/bxl/simple/bxl/audit.bxl:audit_output_dynamic_action_exists",
        )
        assert "fbcode//buck2/tests/targets/rules/ocaml:native" in result.stdout
        assert "ovr_config//platform/linux:x86_64-fbcode-platform" in result.stdout

    result = await buck.bxl(
        "//buck2/tests/targets/bxl/simple/bxl/audit.bxl:audit_output_config_not_match",
    )
    # we should get the unconfigured target label
    assert "fbcode//buck2/tests/targets/rules/shell:gen\n" == result.stdout

    await expect_failure(
        buck.bxl(
            "//buck2/tests/targets/bxl/simple/bxl/audit.bxl:audit_output_invalid_path",
        ),
        stderr_regex="Malformed buck-out path",
    )
