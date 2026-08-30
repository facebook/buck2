# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_docs_agent_prints_agent_context_schema(buck: Buck) -> None:
    result = await buck.docs("agent")
    result.check_returncode()

    assert (
        result.stdout
        == """For builds at Meta, use Buck2.

# Rules

ALWAYS pass `--agent-context key=value` to every `buck2` or `buck` invocation, following the repository-defined schema below. For multiple entries, repeat the flag: `--agent-context key1=value1 --agent-context key2=value2`.

# `--agent-context` schema

Fields:
- `attempt`
  Required: true
  Description: Which attempt number this is for the same logical task
  Allowed values: any
- `intent`
  Required: true
  Description: The purpose of this Buck2 invocation
  Allowed values: build, test, query, fix, investigate
- `prior_error`
  Required: false
  Description: The error category from the previous failed build
  Allowed values: kotlin_unresolved_reference, missing_target, starlark_error, action_command_failure
"""
    )
