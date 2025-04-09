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
from buck2.tests.e2e_util.helper.golden import golden

_PASS_TARGETS = [
    "select_resolution_error_after_compatibility",
    "select_resolution_error_in_dep_after_self_compatibility",
    "select_resolution_error_in_dep_after_dep_compatibility",
    "select_resolution_error_in_exec_dep_after_exec_dep_compatibility_1",
    "select_resolution_error_in_exec_dep_after_exec_dep_compatibility_2",
    "configured_dep_platform_error_after_compatibility",
    "default_target_platform_no_error_in_dep",
    "default_target_platform_no_error_if_global_override",
]

_FAIL_TARGETS = [
    "select_keys_error_in_dead_branch",
    "select_keys_error_before_compatibility",
    "configured_dep_platform_error_in_dead_branch",
    "configured_dep_platform_error_before_compatibility_when_incoming_edge_transition",
    "configured_dep_platform_error_before_compatibility_when_toolchain",
    "compatibility_attrs_error_in_dead_branch",
    "compatibility_attrs_exec_compatible_errors_before_compatibility",
]


@buck_test(allow_soft_errors=True)
async def test_evaluation_order(buck: Buck) -> None:
    for t in _PASS_TARGETS:
        extra_flags = []
        if t == "default_target_platform_no_error_if_global_override":
            extra_flags = ["--target-platforms", "root//:p-cat"]
        await buck.ctargets(":" + t, *extra_flags)
    for t in _FAIL_TARGETS:
        res = await expect_failure(buck.ctargets(":" + t, "-v0", "--console=none"))
        golden(
            output=res.stderr,
            rel_path=f"golden/{t}.golden.stderr",
        )
