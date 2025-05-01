# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import random
import string

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_validation_concurrent(buck: Buck) -> None:
    # There are 2 actions — slow build action and fast validation action.
    # Check that validation doesn't wait for a slow DefaultInfo artifact to be built and fails the build first.
    await expect_failure(
        buck.build(
            ":plate",
            "-c",
            f"test.cache_buster={_random_string()}",
        ),
        stderr_regex="Validation for `.+` failed",
    )


def _random_string() -> str:
    return "".join(random.choice(string.ascii_lowercase) for _ in range(256))
