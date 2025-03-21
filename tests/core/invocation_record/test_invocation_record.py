# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
import json
import signal
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test
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

    await expect_failure(
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

    assert len(record["command_end"]["errors"]) == 0
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
    assert len(record["command_end"]["errors"]) == 0


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
