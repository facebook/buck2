# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os

from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


# This is just a template test case for `check_dependencies_test` to use buck2's e2e test framework.
# It does not need to be edited for new `check_dependencies_test`.


def process_list_arg() -> list[str]:
    list_env = os.environ["DEPS"]
    split_list = [] if list_env == "" else list_env.split(",")
    return [elem for item in split_list for elem in ("--deps", item)]


@buck_test(inplace=True)
async def test_check_dependencies_bxl(buck) -> None:
    dep_list = process_list_arg()
    expect_failure_msg = os.environ["EXPECT_FAILURE_MSG"]
    bxl_call = buck.bxl(
        os.environ["BXL_MAIN"],
        "--",
        "--target",
        os.environ["TARGET"],
        *dep_list,
    )
    if expect_failure_msg == "":
        await bxl_call
    else:
        await expect_failure(bxl_call, stderr_regex=expect_failure_msg)
