# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os

from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform


# This is just a template test case for `check_dependencies_test` to use buck2's e2e test framework.
# It does not need to be edited for new `check_dependencies_test`.


def pass_list_arg(from_env_var: str, to_bxl_param: str) -> list[str]:
    list_env = os.environ[from_env_var]
    split_list = [] if list_env == "" else list_env.split(",")
    return [elem for item in split_list for elem in (to_bxl_param, item)]


@buck_test(inplace=True)
async def test_check_dependencies_bxl(buck) -> None:
    allowlist_args = pass_list_arg(
        from_env_var="ALLOWLIST", to_bxl_param="--allowlist_patterns"
    )
    blocklist_args = pass_list_arg(
        from_env_var="BLOCKLIST", to_bxl_param="--blocklist_patterns"
    )
    expect_failure_msg = os.environ["EXPECT_FAILURE_MSG"]

    fbcode_build_mode = os.environ.get("CHECK_DEPENDENCIES_TEST_FBCODE_BUILD_MODE")
    if fbcode_build_mode:
        mode_argfile = get_mode_from_platform(
            fbcode_build_mode, skip_validation_i_know_what_im_doing=True
        )
    else:
        mode_argfile = get_mode_from_platform()

    bxl_call = buck.bxl(
        os.environ["BXL_MAIN"],
        mode_argfile,
        "--",
        "--target",
        os.environ["TARGET"],
        "--verification_mode",
        os.environ["VERIFICATION_MODE"],
        *allowlist_args,
        *blocklist_args,
    )
    if expect_failure_msg == "":
        await bxl_call
    else:
        await expect_failure(bxl_call, stderr_regex=expect_failure_msg)
