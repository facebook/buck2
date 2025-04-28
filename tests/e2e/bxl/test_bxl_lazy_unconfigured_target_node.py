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


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_unconfigured_target(buck: Buck) -> None:
    await buck.bxl(
        "//bxl/lazy_unconfigured_target_node.bxl:lazy_unconfigured_target_node_resolve",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_unconfigured_target_error(buck: Buck) -> None:
    await expect_failure(
        buck.bxl(
            "//bxl/lazy_unconfigured_target_node.bxl:lazy_unconfigured_target_node_resolve_error"
        ),
        stderr_regex="error: Unknown target `not_exist` from package `root//bin`.",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_unconfigured_target_catch_error(buck: Buck) -> None:
    await buck.bxl(
        "//bxl/lazy_unconfigured_target_node.bxl:lazy_unconfigured_target_node_resolve_catch_error",
    )


@buck_test(inplace=False, data_dir="bxl/simple")
async def test_lazy_unconfigured_target_node_pattern(buck: Buck) -> None:
    await buck.bxl(
        "//bxl/lazy_unconfigured_target_node.bxl:lazy_unconfigured_target_node_pattern",
    )
