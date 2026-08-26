# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import asyncio
import re
import shutil
import time
from datetime import datetime, timedelta, UTC
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env
from buck2.tests.e2e_util.helper.golden import golden, sanitize_hashes
from buck2.tests.e2e_util.helper.utils import expect_exec_count


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


def configure_active_unmaterialization(buck: Buck, enabled: bool) -> None:
    config_file = buck.cwd / ".buckconfig.local"
    with open(config_file, "w") as f:
        f.write(
            f"""
[buck2]
ttl_refresh_enabled = true
clean_stale_enabled = true
clean_stale_artifact_ttl_hours = 8
clean_stale_start_offset_hours = 0.001
clean_stale_period_hours = 0.0001
clean_stale_low_disk_threshold = 100.0
clean_stale_low_disk_adaptive_enabled = true
clean_stale_low_disk_adaptive_min_ttl_hours = 24
clean_stale_low_disk_adaptive_unmaterialize_active = {str(enabled).lower()}
        """
        )


def configure_clean_stale(buck: Buck, settings: str) -> None:
    config_file = buck.cwd / ".buckconfig.local"
    with open(config_file, "w") as f:
        f.write(f"[buck2]\n{settings}")


async def audit_entry(buck: Buck, artifact_name: str) -> str:
    entries = (await buck.audit("deferred-materializer", "list")).stdout.splitlines()
    entry = next(entry for entry in entries if artifact_name in entry)
    assert entry is not None, f"no entry for {artifact_name}: {entries}"
    return entry


def golden_audit_entries(*, entries: list[str], rel_path: str) -> None:
    output = re.sub(r"ts=[^,)]*", "ts=<TIMESTAMP>", "\n".join(entries))
    golden(output=sanitize_hashes(output), rel_path=rel_path)


@buck_test()
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_artifact_access_time(buck: Buck) -> None:
    # drop microseconds to match 1s precision from materializer
    start = datetime.now(UTC).replace(microsecond=0)
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
        timestamp = datetime.strptime(match.group(1), "%Y-%m-%dT%H:%M:%SZ").replace(
            tzinfo=UTC
        )
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
async def test_clean_stale_uses_configured_ttl_when_scheduling_disabled(
    buck: Buck,
) -> None:
    configure_clean_stale(buck, "clean_stale_artifact_ttl_hours = 0\n")
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()

    await buck.kill()
    await buck.clean("--stale")
    assert not output.exists()


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_uses_configured_adaptive_policy(buck: Buck) -> None:
    configure_clean_stale(
        buck,
        """clean_stale_artifact_ttl_hours = 8
clean_stale_low_disk_threshold = 100.0
clean_stale_low_disk_adaptive_enabled = true
clean_stale_low_disk_adaptive_min_ttl_hours = 0
""",
    )
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()

    await buck.kill()
    await buck.clean("--stale")
    assert not output.exists()


@buck_test()
async def test_clean_stale_uses_configured_dry_run(buck: Buck) -> None:
    configure_clean_stale(
        buck,
        """clean_stale_artifact_ttl_hours = 0
clean_stale_dry_run = true
""",
    )
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()

    await buck.kill()
    await buck.clean("--stale")
    assert output.exists()


@buck_test(skip_for_os=["windows"])
async def test_explicit_clean_stale_duration_ignores_configured_adaptive_policy(
    buck: Buck,
) -> None:
    configure_clean_stale(
        buck,
        """clean_stale_artifact_ttl_hours = 0
clean_stale_low_disk_threshold = 100.0
clean_stale_low_disk_adaptive_enabled = true
clean_stale_low_disk_adaptive_min_ttl_hours = 0
""",
    )
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()

    await buck.kill()
    await buck.clean("--stale=10000d")
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


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_scheduled_adaptive_high_disk_usage(buck: Buck) -> None:
    # Threshold of 100.0 guarantees free disk % is always "below" it, so the
    # adaptive loop must promote retained, non-active artifacts to stale even
    # though the regular ttl (8h) would have kept them.
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
clean_stale_low_disk_adaptive_enabled = true
clean_stale_low_disk_adaptive_min_ttl_hours = 0
        """
        )

    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    await buck.build("//declared:declared")
    time.sleep(3)
    assert not output.exists()


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_scheduled_adaptive_threshold_not_tripped(buck: Buck) -> None:
    # Threshold of 0.0 guarantees free disk % is always above it, so the
    # adaptive loop must never engage and the retained artifact survives.
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
clean_stale_low_disk_threshold = 0.0
clean_stale_low_disk_adaptive_enabled = true
clean_stale_low_disk_adaptive_min_ttl_hours = 0
        """
        )

    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    await buck.build("//declared:declared")
    time.sleep(3)
    assert output.exists()


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_scheduled_adaptive_min_ttl_protects_recent(
    buck: Buck,
) -> None:
    # Threshold of 100.0 always trips adaptive promotion, but the freshly
    # built artifact is well within the 24h adaptive min-TTL floor — it must
    # survive even though disk pressure persists.
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
clean_stale_low_disk_adaptive_enabled = true
clean_stale_low_disk_adaptive_min_ttl_hours = 24
        """
        )

    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    await buck.build("//declared:declared")
    time.sleep(3)
    assert output.exists()


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_cli_adaptive_promotes_retained(buck: Buck) -> None:
    # `--stale=10000d` alone would not clean a freshly-built artifact, but
    # `--adaptive-low-disk-threshold=100.0` always trips the adaptive branch
    # (free disk % is always <= 100%) and `--adaptive-min-ttl=0s` protects
    # nothing, so the retained, non-active artifact must be promoted to stale
    # and removed.
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    # New daemon — original artifact is retained but no longer active.
    await buck.build("//declared:declared")
    res = await buck.clean(
        "--stale=10000d",
        "--adaptive-low-disk-threshold=100.0",
        "--adaptive-min-ttl=0s",
    )
    assert "Adaptive low-disk promotion enabled at 100%" in res.stderr
    assert not output.exists()


@buck_test(skip_for_os=["windows"])
async def test_clean_stale_cli_adaptive_min_ttl_protects_recent(buck: Buck) -> None:
    # Adaptive is tripped (threshold=100%), but `--adaptive-min-ttl=24h`
    # protects every retained artifact accessed within the last 24h, so the
    # freshly-built output survives.
    result = await buck.build("root//:copy")
    output = result.get_build_report().output_for_target("root//:copy")
    assert output.exists()
    await buck.kill()
    await buck.build("//declared:declared")
    await buck.clean(
        "--stale=10000d",
        "--adaptive-low-disk-threshold=100.0",
        "--adaptive-min-ttl=24h",
    )
    assert output.exists()


@buck_test(skip_for_os=["windows"])
async def test_adaptive_unmaterializes_active_remote_intermediate(
    buck: Buck,
) -> None:
    configure_active_unmaterialization(buck, enabled=True)
    result = await buck.build(
        "root//:consume_remote", "--local-only", "--no-remote-cache"
    )
    output = result.get_build_report().output_for_target("root//:consume_remote")
    assert output.exists()
    audit_entries = [await audit_entry(buck, "__download_deferred__")]

    await asyncio.sleep(30)
    audit_entries.append(await audit_entry(buck, "__download_deferred__"))

    remote = await buck.build("root//:download_deferred")
    await expect_exec_count(buck, 0)
    assert (
        remote.get_build_report().output_for_target("root//:download_deferred").exists()
    )
    audit_entries.append(await audit_entry(buck, "__download_deferred__"))
    golden_audit_entries(
        entries=audit_entries,
        rel_path="golden/test_adaptive_unmaterializes_active_remote_intermediate.golden.txt",
    )


@buck_test(skip_for_os=["windows"])
async def test_adaptive_does_not_unmaterialize_active_local_intermediate(
    buck: Buck,
) -> None:
    configure_active_unmaterialization(buck, enabled=True)
    result = await buck.build(
        "root//:consume_local", "--local-only", "--no-remote-cache"
    )
    assert result.get_build_report().output_for_target("root//:consume_local").exists()
    audit_entries = [await audit_entry(buck, "__write__")]

    await asyncio.sleep(30)
    audit_entries.append(await audit_entry(buck, "__write__"))
    golden_audit_entries(
        entries=audit_entries,
        rel_path="golden/test_adaptive_does_not_unmaterialize_active_local_intermediate.golden.txt",
    )


@buck_test(skip_for_os=["windows"])
async def test_adaptive_does_not_unmaterialize_active_final_output(buck: Buck) -> None:
    configure_active_unmaterialization(buck, enabled=True)
    result = await buck.build("root//:download_deferred")
    assert (
        result.get_build_report().output_for_target("root//:download_deferred").exists()
    )

    await asyncio.sleep(30)
    golden_audit_entries(
        entries=[await audit_entry(buck, "__download_deferred__")],
        rel_path="golden/test_adaptive_does_not_unmaterialize_active_final_output.golden.txt",
    )


@buck_test(skip_for_os=["windows"])
async def test_adaptive_does_not_unmaterialize_when_disabled(buck: Buck) -> None:
    configure_active_unmaterialization(buck, enabled=False)
    result = await buck.build(
        "root//:consume_remote", "--local-only", "--no-remote-cache"
    )
    assert result.get_build_report().output_for_target("root//:consume_remote").exists()
    audit_entries = [await audit_entry(buck, "__download_deferred__")]

    await asyncio.sleep(30)
    audit_entries.append(await audit_entry(buck, "__download_deferred__"))
    golden_audit_entries(
        entries=audit_entries,
        rel_path="golden/test_adaptive_does_not_unmaterialize_when_disabled.golden.txt",
    )


@buck_test(skip_for_os=["windows", "darwin"])
async def test_clean_scratch_on_idle(buck: Buck) -> None:
    """Scratch (buck-out/<iso>/tmp) is swept once the daemon goes idle."""
    with open(buck.cwd / ".buckconfig.local", "w") as f:
        f.write("[buck2]\nclean_scratch_on_idle = true\n")

    # Dead scratch from past actions: deleted regardless of age.
    dead = (
        buck.cwd / "buck-out" / "v2" / "tmp" / "root" / "aaaa" / "cat" / "dead_action"
    )
    dead.mkdir(parents=True)
    (dead / "junk").write_text("x" * 16)

    # A sibling scratch root the sweep cannot read: skipped, never deleted.
    # (Kept out of `tmp/root` so its failed deletion cannot shadow `dead`'s.)
    unreadable = buck.cwd / "buck-out" / "v2" / "tmp" / "unreadable"
    unreadable.mkdir()
    (unreadable / "junk").write_text("y")
    unreadable.chmod(0o000)

    try:
        await buck.build("root//:copy")
        # The sweep starts shortly after the command finishes, once the daemon
        # is idle.
        for _ in range(60):
            if not dead.exists():
                break
            time.sleep(0.5)
    finally:
        unreadable.chmod(0o755)

    assert not dead.exists()
    assert (unreadable / "junk").exists()
