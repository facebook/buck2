# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_select_refine(buck: Buck) -> None:
    # Smoke test for select refinement:
    # the most specific option is picked even if it is not listed first.
    out = await buck.cquery(
        "--target-platforms=//buck2/tests/targets/configurations/select_refine:p-good-domestic",
        "deps(//buck2/tests/targets/configurations/select_refine:the-test)",
    )
    out.check_returncode()
    lines = [re.sub(" .*", "", line.strip()) for line in out.stdout.splitlines()]
    assert (
        "fbcode//buck2/tests/targets/configurations/select_refine:t-good-domestic"
        in lines
    ), lines
