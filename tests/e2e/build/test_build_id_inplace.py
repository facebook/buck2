# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.utils import random_string


class BuildIdInplaceTest(BuckTestCase):
    @buck_test(inplace=True)
    async def test_buck_build_id_is_the_action_build_id(self, buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/genrule:build_id"
        result = await buck.build(
            target,
            "--local-only",
            "--no-remote-cache",
            "-c",
            f"test.cache_buster={random_string()}",
        )

        output = buck.cwd / result.get_build_report().output_for_target(target)
        assert output.read_text().strip() == result.buck_build_id
