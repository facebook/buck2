# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test

# TODO(iguridi) or TODO(raulgarcia4):
# New `audit` commands have been added since these tests were created.
# Test them if necessary.


@buck_test()
@pytest.mark.parametrize(  # type: ignore
    "cmd",
    [
        "audit_visibility",
        "audit_configurations",
        "audit_config",
        "audit_visibility",
    ],
)
async def test_pass_common_opts_func(buck: Buck, cmd: str) -> None:
    cmd_call = getattr(buck, cmd)
    await cmd_call("--config", "client.id=placeholder_id")


@buck_test()
@pytest.mark.parametrize(  # type: ignore
    "cmd",
    [
        "analysis-queries",
        "cell",
        "execution-platform-resolution",
        "includes",
        "prelude",
        "providers",
        "subtargets",
    ],
)
async def test_pass_common_opts(buck: Buck, cmd: str) -> None:
    commands_requiring_target_pattern_arg_value = {"providers", "subtargets"}
    arg = "//:dummy"

    if cmd in commands_requiring_target_pattern_arg_value:
        await buck.audit(cmd, arg, "--config", "client.id=placeholder_id")
    else:
        await buck.audit(cmd, "--config", "client.id=placeholder_id")
