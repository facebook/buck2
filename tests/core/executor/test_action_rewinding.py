# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import hashlib
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.utils import filter_events
from buck2.tests.e2e_util.helper.utils import read_what_ran


def _digest(content: str) -> str:
    return f"{hashlib.sha1(content.encode()).hexdigest()}:{len(content)}"


PRODUCER_CONTENT = "generated input for action rewind\n"
PRODUCER_DIGEST = _digest(PRODUCER_CONTENT)
FIRST_PRODUCER_CONTENT = "first generated input for action rewind\n"
FIRST_PRODUCER_DIGEST = _digest(FIRST_PRODUCER_CONTENT)
SECOND_PRODUCER_CONTENT = "second generated input for action rewind\n"
SECOND_PRODUCER_DIGEST = _digest(SECOND_PRODUCER_CONTENT)
TREE_PRODUCER_CONTENT = "tree generated input for action rewind\n"
TREE_PRODUCER_DIGEST = _digest(TREE_PRODUCER_CONTENT)
TOP_LEVEL_OUTPUT_CONTENT = "top-level generated output for action rewind\n"
TOP_LEVEL_OUTPUT_DIGEST = _digest(TOP_LEVEL_OUTPUT_CONTENT)

CONSUMER_CONTENT = f"consumer saw: {PRODUCER_CONTENT}"
CONSUMER_DIGEST = _digest(CONSUMER_CONTENT)
CONSUMER_TWO_CONTENT = (
    f"multi consumer saw:\n{FIRST_PRODUCER_CONTENT}{SECOND_PRODUCER_CONTENT}"
)
CONSUMER_TWO_DIGEST = _digest(CONSUMER_TWO_CONTENT)
TREE_CONSUMER_CONTENT = f"tree consumer saw: {TREE_PRODUCER_CONTENT}"
TREE_CONSUMER_DIGEST = _digest(TREE_CONSUMER_CONTENT)
TREE_DIR_CONSUMER_CONTENT = f"tree dir consumer saw: {TREE_PRODUCER_CONTENT}"
TREE_DIR_CONSUMER_DIGEST = _digest(TREE_DIR_CONSUMER_CONTENT)
MANY_INPUT_COUNT = 25
MANY_PRODUCER_CONTENTS = [
    f"many generated input {i} for action rewind\n" for i in range(MANY_INPUT_COUNT)
]
MANY_PRODUCER_DIGESTS = [_digest(content) for content in MANY_PRODUCER_CONTENTS]
CONSUMER_MANY_CONTENT = "many consumer saw:\n" + "".join(MANY_PRODUCER_CONTENTS)
CONSUMER_MANY_DIGEST = _digest(CONSUMER_MANY_CONTENT)

TARGET = "root//:consumer"
LOCAL_INPUT_CONSUMER_TARGET = "root//:local_input_consumer"
CONSUMER_TWO_TARGET = "root//:consumer_two"
CONSUMER_MANY_TARGET = "root//:consumer_many"
TREE_CONSUMER_TARGET = "root//:tree_consumer"
TREE_DIR_CONSUMER_TARGET = "root//:tree_dir_consumer"
TOP_LEVEL_OUTPUT_TARGET = "root//:top_level_output"
NONDETERMINISTIC_PRODUCER_TARGET = "root//:nondeterministic_producer"
NONDETERMINISTIC_CONSUMER_TARGET = "root//:nondeterministic_consumer"
NONDETERMINISTIC_MIDDLE_TARGET = "root//:nondeterministic_middle"
NONDETERMINISTIC_CHAIN_CONSUMER_TARGET = "root//:nondeterministic_chain_consumer"

REMOTE_ARGS = [
    "--remote-only",
    "--no-remote-cache",
]


async def _restart_with_test_env(buck: Buck, env: dict[str, str]) -> None:
    for key, value in env.items():
        buck.set_env(key, value)
    await buck.kill()


async def _assert_remote_actions_ran(
    buck: Buck,
    expected_identity_fragments: list[str],
) -> None:
    what_ran = await read_what_ran(buck)
    actions = {
        entry["identity"]: entry["reproducer"]["executor"]
        for entry in what_ran
        if "reproducer" in entry
    }

    for fragment in expected_identity_fragments:
        assert any(fragment in identity for identity in actions), actions
    assert all(executor == "Re" for executor in actions.values()), actions


async def _assert_actions_ran_on(
    buck: Buck,
    expected_executors: dict[str, str],
) -> None:
    what_ran = await read_what_ran(buck)
    actions = {
        entry["identity"]: entry["reproducer"]["executor"]
        for entry in what_ran
        if "reproducer" in entry
    }

    for fragment, expected_executor in expected_executors.items():
        matching_executors = [
            executor for identity, executor in actions.items() if fragment in identity
        ]
        assert matching_executors, actions
        assert expected_executor in matching_executors, actions


async def _seed_stale_digest_from_action_event(
    buck: Buck,
    fail_digests_file: Path,
    target: str,
    output_path: str,
) -> None:
    await buck.build(
        target,
        *REMOTE_ARGS,
        "--materializations=none",
    )
    fail_digests_file.write_text(
        await _single_action_output_digest(buck, output_path),
        encoding="utf-8",
    )


async def _action_output_digests(buck: Buck, output_path: str) -> list[str]:
    action_executions = await filter_events(
        buck,
        "Event",
        "data",
        "SpanEnd",
        "data",
        "ActionExecution",
    )
    return [
        output["digest"]
        for execution in action_executions
        for output in execution.get("outputs", [])
        if output.get("path") == output_path
    ]


async def _single_action_output_digest(buck: Buck, output_path: str) -> str:
    digests = await _action_output_digests(buck, output_path)
    assert len(digests) == 1, digests
    return digests[0]


async def _last_action_output_digest(buck: Buck, output_path: str) -> str:
    digests = await _action_output_digests(buck, output_path)
    assert digests, output_path
    return digests[-1]


async def _assert_action_output_digest(
    buck: Buck,
    output_path: str,
    expected_digest: str,
) -> None:
    assert await _single_action_output_digest(buck, output_path) == expected_digest


def _single_recorded_digest(path: Path) -> str:
    digests = path.read_text(encoding="utf-8").splitlines()
    assert len(digests) == 1, digests
    return digests[0]


@buck_test()
async def test_rewinds_generated_input_evicted_from_remote_cache_mid_build(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE": PRODUCER_DIGEST,
        },
    )
    await buck.build(
        TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    await _assert_action_output_digest(buck, "consumer.txt", CONSUMER_DIGEST)
    await _assert_remote_actions_ran(buck, ["root//:producer", "root//:consumer"])


@buck_test()
async def test_rewinds_generated_input_evicted_during_local_materialization(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_FAIL_RE_DOWNLOAD_DIGESTS_ONCE": PRODUCER_DIGEST,
        },
    )
    result = await buck.build(
        LOCAL_INPUT_CONSUMER_TARGET,
        "--prefer-remote",
        "--no-remote-cache",
    )

    output = result.get_build_report().output_for_target(LOCAL_INPUT_CONSUMER_TARGET)
    assert output.read_text() == CONSUMER_CONTENT
    await _assert_actions_ran_on(
        buck,
        {
            "root//:local_input_producer": "Re",
            "root//:local_input_consumer": "Local",
        },
    )


@buck_test()
async def test_rewinds_worker_reported_missing_generated_input(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_FAIL_RE_EXECUTE_MISSING_INPUTS_ONCE": "true",
        },
    )
    await buck.build(
        TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    await _assert_action_output_digest(buck, "consumer.txt", CONSUMER_DIGEST)
    await _assert_remote_actions_ran(buck, ["root//:producer", "root//:consumer"])


@buck_test()
async def test_rewinds_multiple_regular_inputs_evicted_from_remote_cache_mid_build(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE": (
                f"{FIRST_PRODUCER_DIGEST} {SECOND_PRODUCER_DIGEST}"
            ),
        },
    )
    await buck.build(
        CONSUMER_TWO_TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    await _assert_action_output_digest(buck, "consumer_two.txt", CONSUMER_TWO_DIGEST)
    await _assert_remote_actions_ran(
        buck,
        ["root//:producer_first", "root//:producer_second", "root//:consumer_two"],
    )


@buck_test()
async def test_rewinds_many_regular_inputs_evicted_from_remote_cache_in_one_retry(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE": " ".join(
                MANY_PRODUCER_DIGESTS
            ),
        },
    )
    await buck.build(
        CONSUMER_MANY_TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    await _assert_action_output_digest(buck, "consumer_many.txt", CONSUMER_MANY_DIGEST)
    await _assert_remote_actions_ran(
        buck,
        ["root//:consumer_many"]
        + [f"root//:producer_many_{i}" for i in range(MANY_INPUT_COUNT)],
    )


@buck_test()
async def test_rewinds_tree_file_input_evicted_from_remote_cache_mid_build(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE": TREE_PRODUCER_DIGEST,
        },
    )
    await buck.build(
        TREE_CONSUMER_TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    await _assert_action_output_digest(
        buck,
        "tree_consumer.txt",
        TREE_CONSUMER_DIGEST,
    )
    await _assert_remote_actions_ran(
        buck,
        ["root//:tree_producer", "root//:tree_consumer"],
    )


@buck_test()
async def test_rewinds_directory_input_leaf_evicted_from_remote_cache_mid_build(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE": TREE_PRODUCER_DIGEST,
        },
    )
    await buck.build(
        TREE_DIR_CONSUMER_TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    await _assert_action_output_digest(
        buck,
        "tree_dir_consumer.txt",
        TREE_DIR_CONSUMER_DIGEST,
    )
    await _assert_remote_actions_ran(
        buck,
        ["root//:tree_dir_producer", "root//:tree_dir_consumer"],
    )


@buck_test()
async def test_rewinds_requested_top_level_output_evicted_during_default_materialization(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_FAIL_RE_DOWNLOAD_DIGESTS_ONCE": TOP_LEVEL_OUTPUT_DIGEST,
        },
    )
    result = await buck.build(
        TOP_LEVEL_OUTPUT_TARGET,
        *REMOTE_ARGS,
    )

    output = result.get_build_report().output_for_target(TOP_LEVEL_OUTPUT_TARGET)
    assert output.read_text() == TOP_LEVEL_OUTPUT_CONTENT
    await _assert_remote_actions_ran(buck, ["root//:top_level_output"])


@buck_test()
async def test_rewinds_requested_output_evicted_during_final_upload_with_materialization(
    buck: Buck,
) -> None:
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE": TOP_LEVEL_OUTPUT_DIGEST,
        },
    )
    result = await buck.build(
        TOP_LEVEL_OUTPUT_TARGET,
        *REMOTE_ARGS,
        "--upload-final-artifacts=always",
    )

    output = result.get_build_report().output_for_target(TOP_LEVEL_OUTPUT_TARGET)
    assert output.read_text() == TOP_LEVEL_OUTPUT_CONTENT
    assert await _action_output_digests(buck, "producer.txt") == [
        TOP_LEVEL_OUTPUT_DIGEST,
        TOP_LEVEL_OUTPUT_DIGEST,
    ]
    await _assert_remote_actions_ran(buck, ["root//:top_level_output"])


@buck_test()
async def test_rewinds_nondeterministic_generated_input_with_fresh_digest(
    buck: Buck,
    tmp_path: Path,
) -> None:
    fail_digests_file = tmp_path / "lost-input-digests"
    fail_digests_file.write_text("", encoding="utf-8")
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE_FILE": str(fail_digests_file),
        },
    )
    await _seed_stale_digest_from_action_event(
        buck,
        fail_digests_file,
        NONDETERMINISTIC_PRODUCER_TARGET,
        "nondeterministic.txt",
    )

    await buck.build(
        NONDETERMINISTIC_CONSUMER_TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    assert await _last_action_output_digest(
        buck,
        "nondeterministic.txt",
    ) != _single_recorded_digest(fail_digests_file)
    await _assert_remote_actions_ran(
        buck,
        ["root//:nondeterministic_producer", "root//:nondeterministic_consumer"],
    )


@buck_test()
async def test_rewinds_nondeterministic_intermediate_action_with_fresh_digest(
    buck: Buck,
    tmp_path: Path,
) -> None:
    fail_digests_file = tmp_path / "lost-intermediate-digests"
    fail_digests_file.write_text("", encoding="utf-8")
    await _restart_with_test_env(
        buck,
        {
            "BUCK2_TEST_INJECTED_MISSING_DIGESTS_ONCE_FILE": str(fail_digests_file),
        },
    )
    await _seed_stale_digest_from_action_event(
        buck,
        fail_digests_file,
        NONDETERMINISTIC_MIDDLE_TARGET,
        "nondeterministic_middle.txt",
    )

    await buck.build(
        NONDETERMINISTIC_CHAIN_CONSUMER_TARGET,
        *REMOTE_ARGS,
        "--materializations=none",
    )

    assert await _last_action_output_digest(
        buck,
        "nondeterministic_middle.txt",
    ) != _single_recorded_digest(fail_digests_file)
    await _assert_remote_actions_ran(
        buck,
        [
            "root//:nondeterministic_middle",
            "root//:nondeterministic_chain_consumer",
        ],
    )
