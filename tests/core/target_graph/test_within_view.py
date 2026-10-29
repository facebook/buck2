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


class WithinViewTest(BuckTestCase):
    @buck_test()
    async def test_within_view(self, buck: Buck) -> None:
        res = await buck.targets("//a/...", "--json-lines")
        assert res.get_target_list() == ["prelude//a:a"]

    @buck_test()
    async def test_within_view_outside_view(self, buck: Buck) -> None:
        await expect_failure(
            buck.targets("//b/..."),
            stderr_regex="Target's `within_view` attribute does not allow dependency `prelude//a:a`",
        )

    @buck_test()
    async def test_within_view_default_outofview(self, buck: Buck) -> None:
        res = await buck.targets("//default/...", "--json-lines")
        assert res.get_target_list() == ["prelude//default:a"]

    @buck_test()
    async def test_within_view_default_outofview_withnone(self, buck: Buck) -> None:
        res = await buck.targets("//default_withnone/none/...", "--json-lines")
        assert res.get_target_list() == [
            "prelude//default_withnone/none:target",
        ]

    @buck_test()
    async def test_within_view_default_outofview_withnoneselect(
        self, buck: Buck
    ) -> None:
        res = await buck.targets("//default_withnone/select/...", "--json-lines")
        assert res.get_target_list() == [
            "prelude//default_withnone/select:target",
        ]

    @buck_test()
    async def test_within_view_default_outofview_withdefault(self, buck: Buck) -> None:
        res = await buck.targets("//default_withvalue/value/...", "--json-lines")
        assert res.get_target_list() == [
            "prelude//default_withvalue/value:target",
        ]

    @buck_test()
    async def test_within_view_default_outofview_withdefaultselect(
        self, buck: Buck
    ) -> None:
        res = await buck.targets("//default_withvalue/select/...", "--json-lines")
        assert res.get_target_list() == [
            "prelude//default_withvalue/select:target",
        ]
