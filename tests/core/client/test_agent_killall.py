# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

from __future__ import annotations

import json
import uuid
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckResult
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


async def scoped_killall(buck: Buck, *args: str) -> BuckResult:
    # Keep even a broken guard confined to this test's daemon.
    assert buck.isolation_prefix
    return await buck.run_buck_command(
        "killall", "--repo", "--in-isolation-dir", buck.isolation_prefix, *args
    )


@buck_test(write_invocation_record=True)
async def test_block_preserves_daemon_and_records_advice(buck: Buck) -> None:
    buck.set_isolation_prefix(uuid.uuid4().hex)
    await buck.audit_config()
    before = json.loads((await buck.status()).stdout)["process_info"]["pid"]
    result = await expect_failure(
        scoped_killall(
            buck,
            "--setting=agent_interactive.enabled=true",
            "--agent-context=direct_call=true",
        ),
        stderr_regex=r"Buck advice \[killall\]",
    )
    assert result.stdout == ""
    assert "No processes have been killed" in result.stderr
    assert "advice_ack=killall" in result.stderr
    assert "reason=" in result.stderr
    assert json.loads((await buck.status()).stdout)["process_info"]["pid"] == before
    record = result.invocation_record()
    assert record["agent_direct_call"] is True
    assert "agent_interactive.killall.block" in record["tags"]
    assert "AGENT_ADVICE_BLOCKED" in record.single_error()["tags"]


@buck_test(write_invocation_record=True)
async def test_acknowledgement_needs_exact_id_and_nonempty_reason(buck: Buck) -> None:
    buck.set_isolation_prefix(uuid.uuid4().hex)
    for context in [
        "advice_ack=build_intent,reason=Restart test daemon",
        "advice_ack=killall",
        "advice_ack=killall,reason=   ",
        "advice_ack=killall,reason=old,reason=",
        "reason=Restart test daemon",
    ]:
        await expect_failure(
            scoped_killall(
                buck,
                "--setting=agent_interactive.enabled=true",
                "--agent-context=direct_call=true",
                f"--agent-context={context}",
            ),
            stderr_regex=r"Buck advice \[killall\]",
        )
    assert "no buckd running" in (await buck.status()).stderr

    await buck.audit_config()
    result = await scoped_killall(
        buck,
        "--setting=agent_interactive.enabled=true",
        "--agent-context=direct_call=true,advice_ack=killall,reason=Restart test daemon",
    )
    assert "Buck advice" not in result.stderr
    assert "agent_interactive.killall.ack" in result.invocation_record()["tags"]
    assert "no buckd running" in (await buck.status()).stderr


@buck_test()
async def test_disabled_and_non_direct_calls_keep_existing_behavior(buck: Buck) -> None:
    buck.set_isolation_prefix(uuid.uuid4().hex)
    for args in [
        ("--agent-context=direct_call=true",),
        (
            "--setting=agent_interactive.enabled=false",
            "--agent-context=direct_call=true",
        ),
        ("--setting=agent_interactive.enabled=true",),
        (
            "--setting=agent_interactive.enabled=true",
            "--agent-context=direct_call=false",
        ),
        (
            "--setting=agent_interactive.enabled=true",
            "--agent-context=direct_call=invalid",
        ),
        (
            "--setting=agent_interactive.enabled=true",
            "--agent-context=direct_call=true,direct_call=false",
        ),
    ]:
        await buck.audit_config()
        result = await scoped_killall(buck, *args)
        assert "Buck advice" not in result.stderr
        assert "no buckd running" in (await buck.status()).stderr


@buck_test()
async def test_guard_works_without_valid_build_config(buck: Buck) -> None:
    buck.set_isolation_prefix(uuid.uuid4().hex)
    (buck.cwd / ".buckconfig").write_text("invalid buckconfig")
    await expect_failure(
        scoped_killall(
            buck,
            "--setting=agent_interactive.enabled=true",
            "--agent-context=direct_call=true",
        ),
        stderr_regex=r"Buck advice \[killall\]",
    )


@buck_test()
async def test_outside_repository_keeps_existing_behavior(buck: Buck) -> None:
    buck.set_isolation_prefix(uuid.uuid4().hex)
    await buck.audit_config()
    before = json.loads((await buck.status()).stdout)["process_info"]["pid"]
    outside = buck.cwd / "outside"
    outside.mkdir()
    (outside / ".buckroot").touch()
    assert buck.isolation_prefix
    args = (
        "--in-isolation-dir",
        buck.isolation_prefix,
        "--setting=agent_interactive.enabled=true",
        "--agent-context=direct_call=true",
    )
    await expect_failure(
        buck.run_buck_command(
            "killall",
            "--repo",
            *args,
            rel_cwd=Path("outside"),
        ),
        stderr_regex=r"`--repo` requires running from within a repository",
    )
    assert json.loads((await buck.status()).stdout)["process_info"]["pid"] == before
    result = await buck.run_buck_command("killall", *args, rel_cwd=Path("outside"))
    assert "Buck advice" not in result.stderr
    assert "no buckd running" in (await buck.status()).stderr
