# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase, env


class Buck1FlavorsTest(BuckTestCase):
    @buck_test()
    @env("BUCK2_HARD_ERROR", "false")
    async def test_buck1_flavors_are_mapped(self, buck: Buck) -> None:
        out = await buck.uquery("deps(root//subtarget:subtarget_flavor)")
        assert "root//lib:lib" in out.stdout
        out = await buck.cquery(
            "root//subtarget:subtarget_flavor", "--output-attribute=dep"
        )
        assert "root//lib:lib[shared]" in out.stdout
        out = await buck.cquery(
            "root//platform:platform_flavor", "--output-attribute=dep"
        )
        assert "root//lib:lib[shared]" in out.stdout
        await buck.build("root//file:hash_in_file_name")

    @buck_test()
    @env("BUCK2_HARD_ERROR", "only=legacy_flavor_subtarget")
    async def test_subtarget_flavor_hard_error(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//subtarget:subtarget_flavor"),
            stderr_regex=r"buck1 flavor `#shared` on `//lib:lib#shared` is deprecated.*write the subtarget as `\[shared\]`",
        )

    @buck_test()
    @env("BUCK2_HARD_ERROR", "true")
    async def test_hash_in_file_name_is_not_a_flavor(self, buck: Buck) -> None:
        # `attrs.one_of(attrs.dep(), attrs.source())` probes `data#1.json` as a label first;
        # without a `:` it is not a flavored label, so no soft error is reported.
        await buck.build("root//file:hash_in_file_name")

    @buck_test()
    @env("BUCK2_HARD_ERROR", "only=platform_flavor")
    async def test_platform_flavor_hard_error(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//platform:platform_flavor"),
            stderr_regex=r"buck1 flavor `#android-arm64,shared` on `//lib:lib#android-arm64,shared` is deprecated.*write the subtarget as `\[shared\]`",
        )
        await expect_failure(
            buck.uquery("root//lib:lib#iphoneos-arm64-tests"),
            stderr_regex=r"buck1 flavor `#iphoneos-arm64-tests` on `root//lib:lib#iphoneos-arm64-tests` is deprecated.*it has no effect in buck2; remove it",
        )
        # Subtarget-only flavors are a different category.
        await buck.uquery("root//subtarget:subtarget_flavor")

    @buck_test()
    @env("BUCK2_HARD_ERROR", "only=legacy_flavor_unrecognized")
    async def test_unrecognized_flavor_hard_error(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//unrecognized:unrecognized_flavor"),
            stderr_regex=r"buck1 flavor `#no-such-flavor` on `//lib:lib#no-such-flavor` is deprecated.*buck2 does not support it",
        )
