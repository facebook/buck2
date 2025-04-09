# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_configuration_transition_attr(buck: Buck) -> None:
    result = await buck.cquery("deps(root//:the-test)")
    result.check_returncode()
    # Default configuration is iphoneos and it should be transitioned to watchos
    assert ":watchos_resource" in result.stdout
    assert ":default_resource" not in result.stdout
