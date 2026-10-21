# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

from __future__ import annotations

import asyncio
import json
import shutil
import sys
import unittest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, BuckTestCase


class AgentAdviceTest(BuckTestCase):
    @buck_test()
    async def test_block_before_target_resolution(self, buck: Buck) -> None:
        result = await expect_failure(
            buck.build("//:missing", "--agent-context", "direct_call=true"),
            stderr_regex=r"\[build_intent\]",
        )
        assert result.stderr.count("Choose check, test, or build.") == 1
        assert "advice_ack=build_intent" in result.stderr
        assert "Unknown target" not in result.stderr
        assert result.stdout == ""
        assert json.loads((await buck.status()).stdout)["process_info"]["pid"] > 0

    @buck_test()
    async def test_warn_preserves_build_output(self, buck: Buck) -> None:
        baseline = await buck.build_without_report("//:pass", "--show-output")
        result = await buck.build_without_report(
            "//:pass",
            "--show-output",
            "--agent-context",
            "direct_call=true",
            "--setting",
            "agent_advice.build_intent_mode=warn",
        )
        assert result.stderr.count("[build_intent]") == 1
        assert result.stderr.count("Choose check, test, or build.") == 1
        assert result.stdout == baseline.stdout

    @buck_test()
    async def test_only_explicit_direct_calls_receive_advice(self, buck: Buck) -> None:
        for context in ["", "direct_call=false", "direct_call=invalid"]:
            args = ["--agent-context", context] if context else []
            result = await buck.build("//:pass", *args)
            assert "[build_intent]" not in result.stderr

        result = await buck.build(
            "//:pass",
            "--agent-context",
            "direct_call=true",
            "--agent-context",
            "direct_call=false",
        )
        assert "[build_intent]" not in result.stderr

    @buck_test()
    async def test_acknowledgment_is_scoped_and_schema_exempt(self, buck: Buck) -> None:
        await expect_failure(
            buck.build(
                "//:pass",
                "--agent-context",
                "direct_call=true,advice_ack=another_advice",
            ),
            stderr_regex=r"\[build_intent\]",
        )
        result = await buck.build(
            "//:pass",
            "--client-metadata",
            "id=test_enforced_client",
            "--agent-context",
            "direct_call=true,advice_ack=build_intent,intent=build,attempt=1",
        )
        assert "[build_intent]" not in result.stderr

        await expect_failure(
            buck.build(
                "//:pass",
                "--client-metadata",
                "id=test_enforced_client",
                "--agent-context",
                "direct_call=true,advice_ack=build_intent,intent=build",
            ),
            stderr_regex=r"Missing required agent-context field\(s\):\s+- attempt",
        )

    @buck_test()
    async def test_check_subtargets_do_not_receive_build_advice(
        self, buck: Buck
    ) -> None:
        result = await buck.build(
            "//:pass[check]",
            "//:other[check]",
            "--agent-context",
            "direct_call=true",
        )
        assert "[build_intent]" not in result.stderr

        await expect_failure(
            buck.build(
                "//:pass[check]", "//:other", "--agent-context", "direct_call=true"
            ),
            stderr_regex=r"\[build_intent\]",
        )

    @buck_test()
    async def test_other_commands_do_not_receive_build_advice(self, buck: Buck) -> None:
        result = await buck.targets("//:pass", "--agent-context", "direct_call=true")
        assert "[build_intent]" not in result.stderr
        result = await expect_failure(
            buck.run("//:missing", "--agent-context", "direct_call=true"),
            stderr_regex="Unknown target",
        )
        assert "[build_intent]" not in result.stderr

    @buck_test()
    async def test_missing_or_blank_message_does_not_block(self, buck: Buck) -> None:
        for settings in [
            '[agent_advice]\nbuild_intent_mode = "block"\n',
            '[agent_advice]\nbuild_intent_mode = "block"\nbuild_intent_message = "  "\n',
        ]:
            (buck.cwd / ".bucksettings.toml").write_text(settings)
            result = await buck.build("//:pass", "--agent-context", "direct_call=true")
            assert result.stderr.count("build_intent_message") == 1
            assert "advice_ack=build_intent" not in result.stderr

    @buck_test()
    async def test_default_and_cli_disabled_advice(self, buck: Buck) -> None:
        result = await buck.build(
            "//:pass",
            "--agent-context",
            "direct_call=true",
            "--setting",
            "agent_advice.build_intent_mode=off",
        )
        assert "[build_intent]" not in result.stderr

        (buck.cwd / ".bucksettings.toml").write_text(
            '[agent_advice]\nbuild_intent_message = "Choose check, test, or build."\n'
        )
        result = await buck.build("//:pass", "--agent-context", "direct_call=true")
        assert "[build_intent]" not in result.stderr

    @buck_test()
    async def test_advice_settings_use_startup_compatibility(self, buck: Buck) -> None:
        await buck.build("//:pass")
        before = json.loads((await buck.status()).stdout)["process_info"]["pid"]

        (buck.cwd / ".bucksettings.toml").write_text(
            '[agent_advice]\nbuild_intent_mode = "warn"\n'
            'build_intent_message = "Updated advice text."\n'
        )
        result = await buck.build("//:pass", "--agent-context", "direct_call=true")
        assert result.stderr.count("Updated advice text.") == 1
        assert "Choose check, test, or build." not in result.stderr
        after = json.loads((await buck.status()).stdout)["process_info"]["pid"]
        assert after != before

    @unittest.skipIf(
        sys.platform != "linux", reason="Ancestry inference is enabled only on Linux"
    )
    @unittest.skip("Ancestry inference requires a Meta internal build") # @oss-enable
    @buck_test()
    async def test_detect_direct_agent_and_stop_at_tools(self, buck: Buck) -> None:
        buck._env.pop("CODING_AGENT_METADATA", None)
        # Copy a shell so its executable identity is stable without depending on an
        # installed agent or an LLM service. Keep it alive while Buck reads its parent.
        agent = buck.cwd / "codex"
        shutil.copy2("/bin/sh", agent)
        script = buck.cwd / "tool.sh"
        script.write_text('"$@"\nexit $?\n')
        python_tool = buck.cwd / "tool.py"
        python_tool.write_text(
            "import subprocess, sys\nsys.exit(subprocess.call(sys.argv[1:]))\n"
        )
        exec_script = buck.cwd / "exec_tool.sh"
        exec_script.write_text('exec "$@"\n')
        record_path = buck.cwd / "record.json"

        async def run(prefix: list[str], *extra: str) -> tuple[int, str, dict]:
            command = buck.construct_buck_command(
                "build",
                "//:pass",
                "--unstable-write-invocation-record",
                str(record_path),
                *extra,
            )
            process = await asyncio.create_subprocess_exec(
                str(agent),
                "-c",
                '"$@"; exit $?',
                "agent",
                *prefix,
                *command,
                cwd=buck.cwd,
                env=buck._get_command_env(
                    {"CODING_AGENT_METADATA": "id=codex,direct_call=true"}
                ),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            _, stderr = await asyncio.wait_for(process.communicate(), timeout=120)
            assert process.returncode is not None
            return (
                process.returncode,
                stderr.decode(),
                json.loads(record_path.read_text())["data"]["Record"]["data"][
                    "InvocationRecord"
                ],
            )

        # Reuse an agent-started daemon from tools and the test runner. Only the
        # client's ancestry contributes to inference; only explicit markers gate advice.
        direct = ["/bin/sh", "-c", '"$@"; exit $?', "shell"]
        code, stderr, record = await run(direct)
        assert code == 0, stderr
        assert "[build_intent]" not in stderr
        assert record.get("agent_direct_call") is None
        assert record["agent_inferred_direct_call"] is True
        assert record["agent_context"] == [{"key": "id", "value": "codex"}]
        daemon = json.loads((await buck.status()).stdout)["process_info"]["pid"]

        tools = [["/bin/sh", str(script)], [sys.executable, str(python_tool)]]
        for prefix in [direct, *tools]:
            for value in [None, "true", "false", "invalid"]:
                extra = (
                    [] if value is None else ["--agent-context", f"direct_call={value}"]
                )
                code, stderr, record = await run(prefix, *extra)
                assert (code != 0) == (value == "true"), stderr
                assert ("[build_intent]" in stderr) == (value == "true")
                assert record.get("agent_direct_call") is (
                    None if value in [None, "invalid"] else value == "true"
                )
                assert record["agent_inferred_direct_call"] is (prefix == direct)

        # exec and sourced scripts erase the visible tool boundary. These known
        # false positives must remain observations rather than advice opt-ins.
        for prefix in [
            ["/bin/sh", str(exec_script)],
            ["/bin/sh", "-c", f'. "{script}"', "shell"],
        ]:
            code, stderr, record = await run(prefix)
            assert code == 0, stderr
            assert "[build_intent]" not in stderr
            assert record.get("agent_direct_call") is None
            assert record["agent_inferred_direct_call"] is True

        code, stderr, record = await run(
            [],
            "--agent-context",
            "direct_call=true",
            "--agent-context",
            "advice_ack=build_intent",
        )
        assert code == 0, stderr
        assert record["agent_direct_call"] is True
        assert record["agent_inferred_direct_call"] is True
        result = await buck.build(
            "//:pass",
            "--unstable-write-invocation-record",
            str(record_path),
        )
        assert "[build_intent]" not in result.stderr
        record = json.loads(record_path.read_text())["data"]["Record"]["data"][
            "InvocationRecord"
        ]
        assert record["agent_inferred_direct_call"] is False
        assert record.get("agent_direct_call") is None
        assert not record.get("agent_context")
        assert json.loads((await buck.status()).stdout)["process_info"]["pid"] == daemon
