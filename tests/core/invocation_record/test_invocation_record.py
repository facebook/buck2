# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import json
import signal
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
from buck2.tests.e2e_util.helper.golden import golden, sanitize_stderr
from buck2.tests.e2e_util.helper.utils import read_invocation_record

# FIXME(JakobDegen): Flakey in CI
if False:

    @buck_test(skip_for_os=["windows"])  # TODO(T154836875)
    async def test_has_end_of_stream_false(buck: Buck, tmp_path: Path) -> None:
        hang_path = tmp_path / "hang_path"
        record = tmp_path / "record.json"

        cmd = await buck.build(
            ":hang",
            "-c",
            f"test.hang_path={hang_path}",
            "--unstable-write-invocation-record",
            str(record),
            "--local-only",
            "--no-remote-cache",
        ).start()

        for _ in range(10):
            if hang_path.exists():
                break
            await asyncio.sleep(1)
        else:
            print(await cmd.communicate())
            raise Exception(f"Signal file never created: {hang_path}")

        cmd.send_signal(signal.SIGINT)
        await cmd.communicate()

        record = read_invocation_record(record)

        assert not record["has_end_of_stream"]
        assert not record["has_command_result"]


@buck_test()
async def test_has_end_of_stream_true(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    await buck.build(":pass", "--unstable-write-invocation-record", str(record))

    record = read_invocation_record(record)

    assert record["has_end_of_stream"]
    assert record["has_command_result"]


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_has_no_command_result(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build()

    status = json.loads((await buck.status()).stdout)
    pid = status["process_info"]["pid"]

    result = await expect_failure(
        buck.build(
            ":kill",
            "-c",
            f"test.pid={pid}",
            "--unstable-write-invocation-record",
            str(record),
            "--local-only",
            "--no-remote-cache",
        ),
        stderr_regex="Buck daemon event bus encountered an error",
    )

    record = read_invocation_record(record)

    assert record["has_end_of_stream"]
    assert not record["has_command_result"]

    golden(
        output=sanitize_stderr(result.stderr),
        rel_path="fixtures/test_has_no_command_result.golden.txt",
    )


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_metadata(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build("--unstable-write-invocation-record", str(record))

    record = read_invocation_record(record)

    assert "username" in record["metadata"]["strings"]


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_client_metadata(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build(
        "--client-metadata=foo=bar",
        "--client-metadata=id=baz",
        "--unstable-write-invocation-record",
        str(record),
    )

    record = read_invocation_record(record)

    assert record["client_metadata"] == [
        {"key": "foo", "value": "bar"},
        {"key": "id", "value": "baz"},
    ]

    assert record["metadata"]["strings"]["client"] == "baz"


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_client_metadata_env(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build(
        "--client-metadata=foo=bar",
        "--client-metadata=id=baz",
        "--unstable-write-invocation-record",
        str(record),
        env={"BUCK2_CLIENT_METADATA": "env_foo=env_bar,id=foobar"},
    )

    record = read_invocation_record(record)

    assert record["client_metadata"] == [
        {"key": "env_foo", "value": "env_bar"},
        {"key": "id", "value": "foobar"},
        {"key": "foo", "value": "bar"},
        {"key": "id", "value": "baz"},
    ]

    assert record["metadata"]["strings"]["client"] == "baz"


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_client_metadata_clean(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.clean(
        "--client-metadata=foo=bar",
        "--client-metadata=id=baz",
        "--unstable-write-invocation-record",
        str(record),
    )

    record = read_invocation_record(record)

    assert record["client_metadata"] == [
        {"key": "foo", "value": "bar"},
        {"key": "id", "value": "baz"},
    ]

    assert record["metadata"]["strings"]["client"] == "baz"


@buck_test(skip_for_os=["windows"])
async def test_client_metadata_debug(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"
    # buck.debug() doesn't start the daemon, so we need to start it with a build
    await buck.build()

    await buck.debug(
        "allocator-stats",
        "--client-metadata=foo=bar",
        "--client-metadata=id=baz",
        "--unstable-write-invocation-record",
        str(record),
    )

    record = read_invocation_record(record)

    assert record["client_metadata"] == [
        {"key": "foo", "value": "bar"},
        {"key": "id", "value": "baz"},
    ]

    assert record["metadata"]["strings"]["client"] == "baz"


@buck_test()
async def test_action_error_message_in_record(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"
    await expect_failure(
        buck.build(":fail", "--unstable-write-invocation-record", str(record))
    )

    record = read_invocation_record(record)

    assert len(record["errors"]) == 1
    assert (
        record["errors"][0]["message"]
        == "Failed to build 'root//:fail (<unspecified>)'"
    )
    assert "Hi from stderr!" in record["errors"][0]["telemetry_message"]


@buck_test()
async def test_non_action_error_message_in_record(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"
    await expect_failure(
        buck.build(":missing_target", "--unstable-write-invocation-record", str(record))
    )

    record = read_invocation_record(record)

    assert len(record["errors"]) == 1
    assert record["errors"][0]["message"].startswith(
        "Unknown target `missing_target` from package `root//`"
    )


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_rule_type_names_ci(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build(
        ":duplicate",
        ":and_a_two",
        ":last_three",
        ":a_one",
        "--unstable-write-invocation-record",
        str(record),
        env={"CI": "true"},
    )

    record = read_invocation_record(record)

    assert record["target_rule_type_names"] == [
        "one",
        "pass_",
        "two",
    ]


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_rule_type_names_sandcastle(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build(
        ":duplicate",
        ":and_a_two",
        ":last_three",
        ":a_one",
        "--unstable-write-invocation-record",
        str(record),
        env={"SANDCASTLE": "my_fake_job"},
    )

    record = read_invocation_record(record)

    assert record["target_rule_type_names"] == [
        "one",
        "pass_",
        "two",
    ]


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_rule_type_names_user(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build(
        ":and_a_two",
        ":last_three",
        ":a_one",
        "--unstable-write-invocation-record",
        str(record),
    )

    record = read_invocation_record(record)

    assert record["target_rule_type_names"] == [
        "one",
        "pass_",
        "two",
    ]


@buck_test(skip_for_os=["windows"])  # TODO(T154836632)
async def test_rule_type_names_on_failure(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await expect_failure(
        buck.build(
            ":fail",
            ":last_three",
            ":a_one",
            "--unstable-write-invocation-record",
            str(record),
        )
    )

    record = read_invocation_record(record)

    assert record["target_rule_type_names"] == [
        "fail",
        "one",
        "pass_",
    ]


@buck_test()
async def test_active_networks_kinds(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build("--unstable-write-invocation-record", str(record))

    record = read_invocation_record(record)

    assert "active_networks_kinds" in record


@buck_test()
async def test_peak_memory_and_disk(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Start the daemon
    await buck.build("--unstable-write-invocation-record", str(record))

    record = read_invocation_record(record)

    assert (
        "peak_used_disk_space_bytes" in record and "peak_process_memory_bytes" in record
    )


@buck_test(setup_eden=True, skip_for_os=["darwin"])
async def test_version_control_collector_slow(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Force a 5 second sleep, hg commands should finish within that period of time
    await buck.build(
        ":sleep",
        "--unstable-write-invocation-record",
        str(record),
        "--local-only",
        "--no-remote-cache",
    )

    record = read_invocation_record(record)

    assert "has_local_changes" in record and "hg_revision" in record
    assert record["hg_revision"] is not None


# NOTE: Delete or disable if flaky, ran a bunch of times on my devserver and it passes fine
@buck_test(setup_eden=True, skip_for_os=["darwin", "windows"])
async def test_version_control_collector_fast(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    await buck.targets(
        ":",
        "--unstable-write-invocation-record",
        str(record),
    )

    record = read_invocation_record(record)

    assert "has_local_changes" in record and "hg_revision" in record
    assert record["hg_revision"] is not None


@buck_test()
async def test_peak_stats(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    await buck.build(
        ":run",
        "--unstable-write-invocation-record",
        str(record),
        "--no-remote-cache",
        "--local-only",
    )

    record = read_invocation_record(record)
    assert record
    assert record["max_in_progress_actions"] == 1
    assert record["max_in_progress_local_actions"] == 1
    assert record["max_in_progress_remote_actions"] == 0
    assert record["max_in_progress_remote_uploads"] == 0


@buck_test()
async def test_parallelism_logging(buck: Buck, tmp_path: Path) -> None:
    # Test multiple parallelism values
    parallelism_values = [1, 4, 8]

    for parallelism in parallelism_values:
        record = tmp_path / f"record_{parallelism}.json"

        # Test with different -j values to control concurrency
        await buck.build(
            ":pass",
            "-j",
            str(parallelism),
            "--unstable-write-invocation-record",
            str(record),
        )

        record = read_invocation_record(record)

        # Verify that command_options is present and contains parallelism data
        assert "command_options" in record
        command_options = record["command_options"]

        assert "configured_parallelism" in command_options
        assert "available_parallelism" in command_options

        # The configured parallelism should match what we passed via -j
        assert command_options["configured_parallelism"] == parallelism

        # The available parallelism should be a positive integer (system dependent)
        assert isinstance(command_options["available_parallelism"], int)
        assert command_options["available_parallelism"] > 0

    # Test without -j flag - configured_parallelism should be null
    record_no_j = tmp_path / "record_no_j.json"
    await buck.build(
        ":pass",
        "--unstable-write-invocation-record",
        str(record_no_j),
    )

    record_no_j = read_invocation_record(record_no_j)

    # Verify that command_options is present and contains parallelism data
    assert "command_options" in record_no_j
    command_options_no_j = record_no_j["command_options"]

    assert "configured_parallelism" in command_options_no_j
    assert "available_parallelism" in command_options_no_j

    # When no -j is specified, configured_parallelism should equal available_parallelism
    assert (
        command_options_no_j["configured_parallelism"]
        == command_options_no_j["available_parallelism"]
    )

    # The available parallelism should still be a positive integer (system dependent)
    assert isinstance(command_options_no_j["available_parallelism"], int)
    assert command_options_no_j["available_parallelism"] > 0


@buck_test()
async def test_client_metadata_vscode_fallback(buck: Buck, tmp_path: Path) -> None:
    record = tmp_path / "record.json"

    # Test that vscode-fallback is set when VSCODE_PID is present
    await buck.build(
        ":pass",
        "--unstable-write-invocation-record",
        str(record),
        env={"VSCODE_PID": "12345"},
        stdin=None,  # Ensure stdin is not a terminal
    )

    record = read_invocation_record(record)

    # Should have vscode-fallback as client id
    client_metadata_dict = {m["key"]: m["value"] for m in record["client_metadata"]}
    assert client_metadata_dict.get("id") == "vscode-fallback"
    assert record["metadata"]["strings"]["client"] == "vscode-fallback"
