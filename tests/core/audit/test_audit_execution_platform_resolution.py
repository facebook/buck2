# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden_replace_cfg_hash


@buck_test()
async def test_audit_execition_platform_resolution(buck: Buck) -> None:
    result = await buck.audit("execution-platform-resolution", "//:target")
    golden_replace_cfg_hash(output=result.stdout, rel_path="out.txt.golden")
