# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


@buck_test()
async def test_agent_context_valid_non_enforced(buck: Buck) -> None:
    """Non-enforced client with valid agent context should succeed."""
    await buck.build(
        "//:pass",
        "--agent-context",
        "intent=build,attempt=1",
    )


@buck_test()
async def test_agent_context_non_enforced_no_validation(buck: Buck) -> None:
    """Non-enforced client with invalid values should still succeed (stored as-is)."""
    await buck.build(
        "//:pass",
        "--agent-context",
        "intent=garbage_value,attempt=1",
    )


@buck_test()
async def test_agent_context_enforced_valid(buck: Buck) -> None:
    """Enforced client with all required fields and valid values should succeed."""
    await buck.build(
        "//:pass",
        "--client-metadata",
        "id=test_enforced_client",
        "--agent-context",
        "intent=build,attempt=1",
    )


@buck_test()
async def test_agent_context_enforced_with_optional_field(buck: Buck) -> None:
    """Enforced client with required + optional fields should succeed."""
    await buck.build(
        "//:pass",
        "--client-metadata",
        "id=test_enforced_client",
        "--agent-context",
        "intent=fix,attempt=2,prior_error=missing_target",
    )


@buck_test()
async def test_agent_context_enforced_invalid_value(buck: Buck) -> None:
    """Enforced client with invalid value for constrained field should fail
    with the exact error message including valid values and description."""
    await expect_failure(
        buck.build(
            "//:pass",
            "--client-metadata",
            "id=test_enforced_client",
            "--agent-context",
            "intent=invalid_value,attempt=1",
        ),
        stderr_regex=(
            r"Invalid agent-context value `invalid_value` for key `intent`\."
            r"\s+intent: The purpose of this buck2 invocation"
            r"\s+Valid values: build, test, query, fix, investigate"
        ),
    )


@buck_test()
async def test_agent_context_enforced_unknown_key(buck: Buck) -> None:
    """Enforced client with unknown key should fail with sorted list of valid keys."""
    await expect_failure(
        buck.build(
            "//:pass",
            "--client-metadata",
            "id=test_enforced_client",
            "--agent-context",
            "intent=build,attempt=1,unknown_key=foo",
        ),
        stderr_regex=(
            r"Unknown agent-context key `unknown_key`\."
            r"\s+Valid keys: attempt, intent, prior_error"
        ),
    )


@buck_test()
async def test_agent_context_enforced_missing_required(buck: Buck) -> None:
    """Enforced client missing required fields should list all missing fields sorted."""
    await expect_failure(
        buck.build(
            "//:pass",
            "--client-metadata",
            "id=test_enforced_client",
            "--agent-context",
            "prior_error=missing_target",
        ),
        stderr_regex=(
            r"Missing required agent-context field\(s\):"
            r"\s+- attempt: Which attempt number this is for the same logical task"
            r"\s+- intent: The purpose of this buck2 invocation"
        ),
    )


@buck_test()
async def test_agent_context_enforced_empty_value_counts_as_missing(
    buck: Buck,
) -> None:
    """Enforced client with empty value for required field should report it as missing."""
    await expect_failure(
        buck.build(
            "//:pass",
            "--client-metadata",
            "id=test_enforced_client",
            "--agent-context",
            "intent=,attempt=1",
        ),
        stderr_regex=(
            r"Missing required agent-context field\(s\):"
            r"\s+- intent: The purpose of this buck2 invocation"
        ),
    )


@buck_test()
async def test_agent_context_no_context_passes(buck: Buck) -> None:
    """Build without --agent-context should always succeed."""
    await buck.build("//:pass")


@buck_test()
async def test_agent_context_enforced_no_context_passes(buck: Buck) -> None:
    """Enforced client without --agent-context should succeed (not required in v1)."""
    await buck.build(
        "//:pass",
        "--client-metadata",
        "id=test_enforced_client",
    )


@buck_test()
async def test_agent_context_invalid_format(buck: Buck) -> None:
    """Malformed --agent-context should fail with the exact format error."""
    await expect_failure(
        buck.build(
            "//:pass",
            "--agent-context",
            "not_a_valid_format",
        ),
        stderr_regex=(
            r"Invalid agent-context format: `not_a_valid_format`\."
            r" Each entry must be a `key=value` pair\."
        ),
    )


@buck_test()
async def test_agent_context_invalid_key_format(buck: Buck) -> None:
    """Non-snake_case key should fail with the exact key format error."""
    await expect_failure(
        buck.build(
            "//:pass",
            "--agent-context",
            "InvalidKey=value",
        ),
        stderr_regex=(
            r"Invalid agent-context key: `InvalidKey`\."
            r" Keys must be snake_case identifiers\."
        ),
    )


@buck_test(write_invocation_record=True)
async def test_agent_context_argfile_before_subcommand(buck: Buck) -> None:
    (buck.cwd / "context.args").write_text("--agent-context=intent=build\n")
    result = await buck.run_buck_command(
        "@context.args",
        "--client-metadata",
        "id=test_enforced_client",
        "build",
        "//:pass",
        "--agent-context",
        "attempt=1",
    )
    context = result.invocation_record()["agent_context"]
    assert [entry for entry in context if entry["key"] in {"intent", "attempt"}] == [
        {"key": "intent", "value": "build"},
        {"key": "attempt", "value": "1"},
    ]


@buck_test(write_invocation_record=True)
async def test_agent_context_logged_to_invocation_record(buck: Buck) -> None:
    """Agent context should appear in the invocation record."""
    res = await buck.build(
        "//:pass",
        "--agent-context",
        "intent=build,attempt=1",
    )

    record = res.invocation_record()
    agent_context = record.get("agent_context")
    assert agent_context is not None, (
        "agent_context should be present in invocation record"
    )

    entry_map = {e["key"]: e["value"] for e in agent_context}
    assert entry_map["intent"] == "build"
    assert entry_map["attempt"] == "1"
