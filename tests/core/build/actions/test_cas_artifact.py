# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import hashlib

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test()
async def test_cas_artifact_expiration_out_of_range(buck: Buck) -> None:
    # Regression test: this used to panic the daemon instead of reporting an error.
    await expect_failure(
        buck.build("root//:out_of_range_expiration"),
        stderr_regex="Out-of-range value `4611686018427387904` for expires_after_timestamp",
    )


@buck_test()
async def test_cas_artifact_missing_digest_is_a_clear_error(buck: Buck) -> None:
    content = random_string().encode()
    # patternlint-disable-next-line poor-choice-of-hash-function
    digest = f"{hashlib.sha1(content).hexdigest()}:{len(content)}"
    await expect_failure(
        buck.build("root//:missing", "-c", f"test.missing_digest={digest}"),
        stderr_regex=f"The digest `{digest}` was not found in the CAS under use case `buck2-testing`",
    )
