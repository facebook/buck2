# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import random_string


@buck_test()
async def test_build_id_env_var_is_set_locally(buck: Buck) -> None:
    result = await buck.build(
        "root//:top",
        "--local-only",
        "--no-remote-cache",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    output = result.get_build_report().output_for_target("root//:top")
    assert output.exists()
    with open(output) as f:
        assert f.read().strip() == result.buck_build_id


@buck_test()
async def test_build_id_env_var_is_set_remotely(buck: Buck) -> None:
    result = await buck.build(
        "root//:top",
        "--remote-only",
        "--no-remote-cache",
        "-c",
        f"test.cache_buster={random_string()}",
    )

    output = result.get_build_report().output_for_target("root//:top")
    assert output.exists()
    with open(output) as f:
        assert f.read().strip() == result.buck_build_id
