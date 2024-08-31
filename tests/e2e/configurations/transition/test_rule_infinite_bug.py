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


@buck_test(inplace=True)
async def test_configuration_transition_rule_infinite_bug(buck: Buck) -> None:
    # TODO(nga): this is a bug: query should not attempt to create an infinite graph.
    #   This command should succeed.
    #   It fails because `xx` target is transitioned,
    #   and transitioned target is transitioned again, and so on.
    result = await expect_failure(
        buck.cquery(
            "-c",
            "aaa.bbb=ccc",
            "deps(fbcode//buck2/tests/targets/configurations/transition/rule_infinite_bug:xx)",
        )
    )
    assert "did not produce identical" in result.stderr
