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
async def test_what_failed(buck: Buck) -> None:
    pkg = "fbcode//buck2/tests/targets/rules/genrule/bad"
    bad = "my_genrule_bad_with_dep"
    good = "stub"

    await expect_failure(buck.build(f"{pkg}:{bad}"))
    out = await buck.log("what-failed")

    # Only the failed command should be in what-failed.
    assert f"{pkg}:{bad}" in out.stdout
    assert f"{pkg}:{good}" not in out.stdout

    # Even though both commands are here.
    out = await buck.log("what-ran")
    assert f"{pkg}:{bad}" in out.stdout
    assert f"{pkg}:{good}" in out.stdout
