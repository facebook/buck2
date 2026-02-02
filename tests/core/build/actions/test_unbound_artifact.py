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
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_unbound_artifact(buck: Buck) -> None:
    await expect_failure(
        buck.build("root//:action_with_unbound_artifact"),
        stderr_regex="error: Artifact must be bound by now",
    )


@buck_test()
async def test_unbound_artifact_inside_tset(buck: Buck) -> None:
    await expect_failure(
        buck.build("root//:action_with_unbound_artifact"),
        stderr_regex="error: Artifact must be bound by now",
    )
