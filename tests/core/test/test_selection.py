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
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class SelectionTest(BuckTestCase):
    @buck_test()
    async def test_ok(self, buck: Buck) -> None:
        await buck.test("//:ok")

    @buck_test()
    async def test_fail(self, buck: Buck) -> None:
        await expect_failure(
            buck.test("//:fail"), stderr_regex="Fail: root//:fail - unmanaged"
        )

    @buck_test()
    async def test_tests_attribute(self, buck: Buck) -> None:
        await expect_failure(
            buck.test("//:noop_references_fail"),
            stderr_regex="Fail: root//:fail - unmanaged",
        )

    @buck_test()
    async def test_tests_attribute_transitive(self, buck: Buck) -> None:
        await expect_failure(
            buck.test(
                "//:noop_transitively_references_fail",
            ),
            stderr_regex="Fail: root//:fail - unmanaged",
        )

    @buck_test()
    async def test_tests_attribute_cycle(self, buck: Buck) -> None:
        buck.test(
            "//:noop_cycle1",
        )

    @buck_test()
    async def test_tests_attribute_self_transition(self, buck: Buck) -> None:
        await expect_failure(
            buck.test("//:noop_self_transition_references_fail"),
            stderr_regex="Fail: root//:fail - unmanaged",
        )
