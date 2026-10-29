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


class TypingValidationTest(BuckTestCase):
    @buck_test()
    async def test_typing_validation_fails_on_type_errors(self, buck: Buck) -> None:
        await expect_failure(
            buck.build(":bad_types_validated"),
            stderr_regex="Validation for .+ failed.*is not assignable to parameter",
        )

    @buck_test()
    async def test_typing_validation_passes_on_clean_types(self, buck: Buck) -> None:
        await buck.build(":good_types_validated")

    @buck_test()
    async def test_typing_validation_off_ignores_errors(self, buck: Buck) -> None:
        await buck.build(":bad_types_no_validation")

    @buck_test()
    async def test_typing_disabled_skips_validation(self, buck: Buck) -> None:
        await buck.build(":typing_disabled")
