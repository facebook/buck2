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


@buck_test()
async def test_transition_success_if_attr_value_has_not_changed(buck: Buck) -> None:
    await buck.build("root//:target_where_transition_does_not_change_attr")


@buck_test()
async def test_transition_dep_success_if_attr_value_has_not_changed(buck: Buck) -> None:
    await buck.build("root//:target_with_transition_dep")


@buck_test()
async def test_transition_failed_if_attr_value_has_changed(buck: Buck) -> None:
    err_msg = (
        r"Target root//:target_where_transition_changes_attr configuration transitioned\n"
        r"\s+old: root//:iphone#.*\n"
        r"\s+new: <transitioned-from-watch>#.*\n"
        r"\s+but attribute: extra\n"
        r"\s+resolved with old configuration to: \"phone\"\n"
        r"\s+resolved with new configuration to: \"watch\""
    )

    await expect_failure(
        buck.build("root//:target_where_transition_changes_attr"),
        stderr_regex=err_msg,
    )


@buck_test()
async def test_transition_failed_if_attr_value_cycle(buck: Buck) -> None:
    err_msg = (
        r"Configured target cycle detected \(`->` means \"depends on\"\):\n"
        r"\s+root//:target_where_transition_cycles_via_changed_attrs \(<transitioned-from-.*>#.*\) ->.*\n"
        r"\s+root//:target_where_transition_cycles_via_changed_attrs \(<transitioned-from-.*>#.*\) ->.*\n"
    )

    await expect_failure(
        buck.build("root//:target_where_transition_cycles_via_changed_attrs"),
        stderr_regex=err_msg,
    )
