# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class ConfigurationRuleUnboundTest(BuckTestCase):
    @buck_test()
    async def test_configuration_rule_unbound(self, buck: Buck) -> None:
        result = await buck.cquery(
            # platform argument is ignored
            "--target-platforms=root//:p",
            "root//:the-test",
        )
        # Note configuration is unbound here.
        assert "root//:the-test (<unbound>)\n" == result.stdout
