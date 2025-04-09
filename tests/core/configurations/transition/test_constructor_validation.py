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


@buck_test()
async def test_construction_validation_good(buck: Buck) -> None:
    await buck.targets("//good:")


@buck_test()
async def test_construction_validation_bad(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//bad:"),
        stderr_regex=r"`impl` function signature is incorrect",
    )


@buck_test()
async def test_construction_validation_bad_param_types(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//bad_param_types:"),
        stderr_regex=r"`impl` function signature is incorrect",
    )


@buck_test()
async def test_construction_validation_bad_param_types_vnew(buck: Buck) -> None:
    # FIXME(JakobDegen): Evaluate whether we can implement this. The performance
    # concerns are a bit higher here because the code is hotter.
    await buck.build("//bad_param_types_vnew:")


@buck_test()
async def test_construction_validation_bad_return_type(buck: Buck) -> None:
    await expect_failure(
        buck.targets("//bad_return_type:"),
        stderr_regex=r"`impl` function signature is incorrect",
    )
