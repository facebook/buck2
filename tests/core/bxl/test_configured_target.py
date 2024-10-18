# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test(data_dir="")
async def test_unwrap_forward(buck: Buck) -> None:
    await buck.bxl("//bxl/configured_target.bxl:unwrap_forward")


@buck_test(data_dir="")
async def test_configured_targets_with_modifiers(buck: Buck) -> None:
    result = await buck.bxl(
        "//bxl/configured_target.bxl:configured_targets_with_modifiers"
    )
    configurations = [line.strip() for line in result.stdout.splitlines()]
    linux_cfg = await buck.audit_configurations(configurations[0])
    assert "root//:linux" in linux_cfg.stdout
    macos_cfg = await buck.audit_configurations(configurations[1])
    assert "root//:macos" in macos_cfg.stdout
