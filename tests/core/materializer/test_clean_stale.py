# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import re
import shutil
import time
from datetime import datetime, timedelta
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env


def modify_acess_times_updates(buck: Buck, new_status: str) -> None:
    config_file = buck.cwd / ".buckconfig"
    replace_in_file(
        "update_access_times = full",
        f"update_access_times = {new_status}",
        file=config_file,
    )


def replace_in_file(old: str, new: str, file: Path, encoding: str = "utf-8") -> None:
    with open(file, encoding=encoding) as f:
        file_content = f.read()
    file_content = file_content.replace(old, new)
    with open(file, "w", encoding=encoding) as f:
        f.write(file_content)


@buck_test()
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_artifact_access_time(buck: Buck) -> None:
    # drop microseconds to match 1s precision from materializer
    start = datetime.utcnow().replace(microsecond=0)
    target = "root//:copy"
    result = await buck.build(target)
    assert result.get_build_report().output_for_target(target).exists()

    async def audit_materialized() -> list[str]:
        return list(
            filter(
                lambda x: "\tmaterialized" in x,
                (await buck.audit("deferred-materializer", "list"))
                .stdout.strip()
                .splitlines(),
            )
        )

    def parse_entry_ts(entry: str) -> datetime:
        match = re.search("\tmaterialized \\(ts=([^ ,]*)", entry)
        assert match
        timestamp = datetime.strptime(match.group(1), "%Y-%m-%dT%H:%M:%SZ")
        assert timestamp, match.group(1)
        return timestamp

    materialized_entries = await audit_materialized()
    assert len(materialized_entries) == 1
    materialized_time = parse_entry_ts(materialized_entries[0])
    assert materialized_time >= start

    # Check that access time set after daemon restart
    await buck.kill()
    materialized_entries = await audit_materialized()
    assert len(materialized_entries) == 1
    materialized_time = parse_entry_ts(materialized_entries[0])
    assert materialized_time >= start

    # Check that access time is updated following build
    time.sleep(1)
    await buck.build(target)

    materialized_entries = await audit_materialized()

    assert len(materialized_entries) == 1
    access_time = parse_entry_ts(materialized_entries[0])
    assert access_time > materialized_time


@buck_test()
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
@env("BUCK_ACCESS_TIME_UPDATE_MAX_BUFFER_SIZE", "0")
async def test_clean_stale_artifacts(buck: Buck) -> None:
    target_1 = "root//:copy"
    result_1 = await buck.build(target_1)
    output_1 = result_1.get_build_report().output_for_target(target_1)

    # ensure timestamp is after first materialization and before second
    # (resolution for access timestamps is 1 second)
    time.sleep(1)
    after_first_build = int(time.time())
    time.sleep(1)

    target_2 = "root//:copy_2"
    result_2 = await buck.build(target_2)
    output_2 = result_2.get_build_report().output_for_target(target_2)

    # Check output is correctly materialized
    assert output_1.exists()
    assert output_2.exists()

    await buck.clean(f"--keep-since-time={after_first_build}")
    # Check output_1 still materialized, it's stale but it was built by running daemon
    assert output_1.exists()

    await buck.kill()
    res = await buck.clean(f"--keep-since-time={after_first_build}")
    # Check output_1 was cleaned because it's stale and not declared by running daemon
    assert "1 stale artifact" in res.stderr and "4 bytes cleaned" in res.stderr
    assert not output_1.exists()
    assert output_2.exists()

    future_time = int((datetime.now() + timedelta(weeks=7)).timestamp())

    # Check that a previously materialized output re-declared by new daemon is not cleaned
    await buck.build(target_2)
    await buck.clean(f"--keep-since-time={future_time}")
    assert output_2.exists()

    # Check that setting keep-since-time in the future cleans non-active artifacts
    await buck.kill()
    await buck.clean(f"--keep-since-time={future_time}")
    assert "1 stale artifact" in res.stderr and "4 bytes cleaned" in res.stderr
    assert not output_2.exists()


@buck_test()
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_clean_stale_artifact_dir(buck: Buck) -> None:
    target_1 = "root//:copy_dir"
    result_1 = await buck.build(target_1)
    output_1 = result_1.get_build_report().output_for_target(target_1)
    assert output_1.exists()
    await buck.kill()
    future_time = int((datetime.now() + timedelta(weeks=7)).timestamp())
    res = await buck.clean(f"--keep-since-time={future_time}")
    assert "4 bytes cleaned" in res.stderr
    assert not output_1.exists()
    # NOTE: Currently we require clean twice to delete empty dirs, which is ...
    # probably fine.
    await buck.clean(f"--keep-since-time={future_time}")
    output_parent = output_1.parent
    while not output_parent.exists():
        output_parent = output_parent.parent
    assert output_parent.parts[-3:] == ("buck-out", "v2", "art")


@buck_test()
@env("BUCK_ACCESS_TIME_UPDATE_MAX_BUFFER_SIZE", "0")
async def test_clean_stale_buck_out_empty(buck: Buck) -> None:
    output = await buck.clean("--stale")
    assert "Nothing to clean" in output.stderr


@buck_test()
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
@env("BUCK_ACCESS_TIME_UPDATE_MAX_BUFFER_SIZE", "0")
async def test_clean_stale_actions(buck: Buck) -> None:
    query_res = await buck.cquery("root//...")
    targets = [
        target.split(" ")[0] for target in query_res.stdout.split("\n") if target
    ]

    outputs = []
    for target in targets:
        res = await buck.build(target)
        output = res.get_build_report().outputs_for_target(target)
        outputs += output

    assert len(outputs) >= len(targets)
    for output in outputs:
        assert output.exists()

    await buck.clean("--stale")
    for output in outputs:
        assert output.exists()


@buck_test()
async def test_clean_stale_declared(buck: Buck) -> None:
    await buck.build("//declared:declared")
    await buck.kill()

    # Drop the state. The path exists on disk.
    shutil.rmtree(buck.cwd / "buck-out/v2/cache/materializer_state")

    # Build again, start by declaring, then clean, then require locally.
    await buck.build("//declared:remote")
    await buck.clean("--stale")
    await buck.build("//declared:local")


@buck_test()
async def test_clean_stale_scheduled(buck: Buck) -> None:
    # Need to write to .buckconfig instead of passing cmd line args because
    # the config used when creating daemon state does not include cmd line args (but maybe it should).
    config_file = buck.cwd / ".buckconfig.local"
    with open(config_file, "w") as f:
        f.write(
            """
[buck2]
clean_stale_enabled = true
clean_stale_artifact_ttl_hours = 0
clean_stale_start_offset_hours = 0
# 0.0001h = 360ms
clean_stale_period_hours = 0.0001
        """
        )

    # Just test that a clean runs if enabled via config.
    # Build a target, output is stale immediately but won't be cleaned until restart.
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    # Create a new daemon and build something else (could be any command that starts a daemon).
    await buck.build("//declared:declared")
    # Wait for at least one clean to run (but should have finished multiple cleans).
    time.sleep(3)
    # Original output should be cleaned.
    assert not output.exists()


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_scheduled_high_disk_usage(buck: Buck) -> None:
    # Need to write to .buckconfig instead of passing cmd line args because
    # the config used when creating daemon state does not include cmd line args (but maybe it should).
    config_file = buck.cwd / ".buckconfig.local"
    with open(config_file, "w") as f:
        f.write(
            """
[buck2]
clean_stale_enabled = true
clean_stale_artifact_ttl_hours = 8
clean_stale_start_offset_hours = 0
# 0.0001h = 360ms
clean_stale_period_hours = 0.0001
clean_stale_low_disk_threshold = 100.0
clean_stale_low_disk_artifact_ttl_hours = 0.0
        """
        )

    # Just test that a clean runs if enabled via config.
    # Build a target, output is stale immediately but won't be cleaned until restart.
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    # Create a new daemon and build something else (could be any command that starts a daemon).
    await buck.build("//declared:declared")
    # Wait for at least one clean to run (but should have finished multiple cleans).
    time.sleep(3)
    # Original output should be cleaned.
    assert not output.exists()
