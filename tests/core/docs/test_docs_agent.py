# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase
from buck2.tests.e2e_util.helper.utils import filter_events


class DocsAgentTest(BuckTestCase):
    @buck_test()
    async def test_docs_agent_does_not_start_daemon(self, buck: Buck) -> None:
        await buck.docs("agent")

        status = await buck.status()
        assert "no buckd running" in status.stderr

    @buck_test()
    async def test_docs_agent_preserves_daemon_config(self, buck: Buck) -> None:
        await buck.audit_config("--config", "foo.bar=bar")
        version_path = (
            "Event",
            "data",
            "SpanStart",
            "data",
            "CommandCritical",
            "dice_version",
        )
        before = await filter_events(buck, *version_path)
        assert len(before) == 1

        await buck.docs("agent")

        result = await buck.audit_config(
            "foo.bar", "--reuse-current-config", "--style", "json"
        )
        assert result.get_json() == {"foo.bar": "bar"}
        assert await filter_events(buck, *version_path) == before

        await buck.audit_config("--config", "foo.bar=bar")
        assert await filter_events(buck, *version_path) == before
        assert (
            await filter_events(
                buck, "Event", "data", "Instant", "data", "CellHasNewConfigs"
            )
            == []
        )

    @buck_test()
    async def test_docs_agent_reads_includes_and_overrides(self, buck: Buck) -> None:
        (buck.cwd / "guidance.bcfg").write_text(
            "[agent_context#included]\n description = From the included file\n"
        )
        with (buck.cwd / ".buckconfig").open("a") as config:
            config.write("\n<file:guidance.bcfg>\n")
        (buck.cwd / "nested").mkdir()

        result = await buck.docs("agent", rel_cwd=Path("nested"))
        assert "Description: From the included file" in result.stdout

        overrides = buck.cwd / "overrides.bcfg"
        overrides.write_text(
            "[agent_context#included]\n description = From the config override file\n"
        )
        flag = "agent_context#included.description=From the command line"
        result = await buck.docs(
            "agent", "--config-file", str(overrides), "--config", flag
        )
        assert "Description: From the command line" in result.stdout
        assert "From the config override file" not in result.stdout

        result = await buck.docs(
            "agent", "--config", flag, "--config-file", str(overrides)
        )
        assert "Description: From the config override file" in result.stdout
        assert "From the command line" not in result.stdout

    @buck_test()
    async def test_docs_agent_prints_agent_context_schema(self, buck: Buck) -> None:
        result = await buck.docs("agent")
        result.check_returncode()

        assert (
            result.stdout
            == """For builds at Meta, use Buck2.

# Rules

ALWAYS pass `--agent-context direct_call=true` for every `buck2` or `buck` command you invoke directly.

Also provide `--agent-context key=value` entries following the repository-defined schema below. For multiple entries, repeat the flag: `--agent-context direct_call=true --agent-context key=value`.

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
