# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_default_target_platform_is_subtarget(buck: Buck) -> None:
    # FIXME(JakobDegen): Bug. The target specifies a subtarget that does have an appropriate
    # provider.
    await expect_failure(
        buck.cquery(":stub"),
        stderr_regex="Expected `root//:alias_platform` to be a `platform\\(\\)` target",
    )


@buck_test()
async def test_subtarget_in_select_key(buck: Buck) -> None:
    res = await buck.uquery(
        "root//:with_constraint_key_dep", "-a", "buck.configuration_deps"
    )
    res = json.loads(res.stdout)
    # FIXME(JakobDegen): Bug. `buck.deps`-like attributes do not include subtargets
    assert list(res.values())[0]["buck.configuration_deps"] == ["root//:cat_alias[sub]"]
