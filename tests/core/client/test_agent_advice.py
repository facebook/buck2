# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

from __future__ import annotations

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_block_before_target_resolution(buck: Buck) -> None:
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
async def test_warn_preserves_build_output(buck: Buck) -> None:
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
async def test_only_explicit_direct_calls_receive_advice(buck: Buck) -> None:
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
async def test_acknowledgment_is_scoped_and_schema_exempt(buck: Buck) -> None:
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
async def test_check_subtargets_do_not_receive_build_advice(buck: Buck) -> None:
    result = await buck.build(
        "//:pass[check]",
        "//:other[check]",
        "--agent-context",
        "direct_call=true",
    )
    assert "[build_intent]" not in result.stderr

    await expect_failure(
        buck.build("//:pass[check]", "//:other", "--agent-context", "direct_call=true"),
        stderr_regex=r"\[build_intent\]",
    )


@buck_test()
async def test_other_commands_do_not_receive_build_advice(buck: Buck) -> None:
    result = await buck.targets("//:pass", "--agent-context", "direct_call=true")
    assert "[build_intent]" not in result.stderr
    result = await expect_failure(
        buck.run("//:missing", "--agent-context", "direct_call=true"),
        stderr_regex="Unknown target",
    )
    assert "[build_intent]" not in result.stderr


@buck_test()
async def test_missing_or_blank_message_does_not_block(buck: Buck) -> None:
    for settings in [
        '[agent_advice]\nbuild_intent_mode = "block"\n',
        '[agent_advice]\nbuild_intent_mode = "block"\nbuild_intent_message = "  "\n',
    ]:
        (buck.cwd / ".bucksettings.toml").write_text(settings)
        result = await buck.build("//:pass", "--agent-context", "direct_call=true")
        assert result.stderr.count("build_intent_message") == 1
        assert "advice_ack=build_intent" not in result.stderr


@buck_test()
async def test_default_and_cli_disabled_advice(buck: Buck) -> None:
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
async def test_advice_settings_use_startup_compatibility(buck: Buck) -> None:
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
