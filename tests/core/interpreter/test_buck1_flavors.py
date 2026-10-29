# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class Buck1FlavorsTest(BuckTestCase):
    @buck_test()
    async def test_subtarget_flavor_is_an_error(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//subtarget:subtarget_flavor"),
            stderr_regex=r"buck1 flavor `#shared` on `//lib:lib#shared` is not supported.*write the subtarget as `\[shared\]`",
        )
        # The same label spelled as a subtarget works.
        out = await buck.audit("providers", "root//lib:lib[shared]")
        assert "root//lib:lib[shared]" in out.stdout
        # A string that is not a label still coerces as a file.
        await buck.build("root//file:hash_in_file_name")

    @buck_test()
    async def test_platform_flavor_is_an_error(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//platform:platform_flavor"),
            stderr_regex=r"buck1 flavor `#android-arm64,shared` on `//lib:lib#android-arm64,shared` is not supported.*write the subtarget as `\[shared\]`",
        )
        await expect_failure(
            buck.uquery("root//lib:lib#iphoneos-arm64-tests"),
            stderr_regex=r"buck1 flavor `#iphoneos-arm64-tests` on `root//lib:lib#iphoneos-arm64-tests` is not supported.*it has no effect in buck2; remove it",
        )

    @buck_test()
    async def test_unrecognized_flavor_is_an_error(self, buck: Buck) -> None:
        await expect_failure(
            buck.uquery("root//unrecognized:unrecognized_flavor"),
            stderr_regex=r"buck1 flavor `#no-such-flavor` on `//lib:lib#no-such-flavor` is not supported.*buck2 has no equivalent for it",
        )

    @buck_test()
    async def test_one_of_does_not_swallow_a_flavored_label(self, buck: Buck) -> None:
        # `attrs.one_of(attrs.dep(), attrs.string())` would otherwise accept the label as text.
        await expect_failure(
            buck.uquery("root//one_of:flavored_dep_or_string"),
            stderr_regex=r"buck1 flavor `#shared` on `//lib:lib#shared` is not supported",
        )
