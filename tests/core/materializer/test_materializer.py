# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import sys
from pathlib import Path

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env


def watchman_dependency_linux_only() -> bool:
    return sys.platform == "linux"


def replace_in_file(old: str, new: str, file: Path, encoding: str = "utf-8") -> None:
    with open(file, encoding=encoding) as f:
        file_content = f.read()
    file_content = file_content.replace(old, new)
    with open(file, "w", encoding=encoding) as f:
        f.write(file_content)


@buck_test(data_dir="modify_deferred_materialization")
async def test_modify_input_source(buck: Buck) -> None:
    await buck.build("//:urandom_dep")

    targets_file = buck.cwd / "TARGETS.fixture"

    # Change the label in Targets.
    replace_in_file("__NOT_A_REAL_LABEL__", "buck2_test_local_exec", file=targets_file)

    await buck.build("//:urandom_dep")


@buck_test(
    data_dir="modify_deferred_materialization_deps",
    skip_for_os=["windows"],  # TODO(marwhal): Fix and enable on Windows
)
async def test_modify_dep_materialization(buck: Buck) -> None:
    target = "//:check"

    # Build, expect the symlink to work. We'll materialize the first time.

    result = await buck.build(target)
    with open(result.get_build_report().output_for_target(target)) as f:
        assert f.read().strip() == "TEXT"

    # Build again, expect the symlink to work. We'll materialize just deps.

    with open(buck.cwd / "text", "w", encoding="utf-8") as f:
        f.write("TEXT2")

    result = await buck.build(target)
    with open(result.get_build_report().output_for_target(target)) as f:
        assert f.read().strip() == "TEXT2"

    # Build again, expect the symlink to work. We'll materialize just deps
    # again. However this time our state is a little different since the
    # previous future was a check-deps only future.

    with open(buck.cwd / "text", "w", encoding="utf-8") as f:
        f.write("TEXT3")

    result = await buck.build(target)
    with open(result.get_build_report().output_for_target(target)) as f:
        assert f.read().strip() == "TEXT3"


@buck_test(
    data_dir="deferred_materializer_matching_artifact_optimization",
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_matching_artifact_optimization(buck: Buck) -> None:
    target = "root//:copy"
    result = await buck.build(target)
    # Check output is correctly materialized
    assert result.get_build_report().output_for_target(target).exists()

    # In this case, modifying `hidden` does not change the output, so the output should not
    # need to be rematerialized
    with open(buck.cwd / "hidden", "w", encoding="utf-8") as f:
        f.write("HIDDEN2")

    result = await buck.build(target)
    # Check output still exists
    assert result.get_build_report().output_for_target(target).exists()
    # Check that materializer did not report any rematerialization
    assert "already materialized, updating deps only" in result.stderr
    assert "materialize artifact" not in result.stderr

    # In this case, modifying `src` changes the output, so the output should be rematerialized
    with open(buck.cwd / "src", "w", encoding="utf-8") as f:
        f.write("SRC2")

    result = await buck.build(target)
    # Check output still exists
    output = result.get_build_report().output_for_target(target)
    assert output.exists()
    with open(output) as f:
        assert f.read().strip() == "SRC2"


@buck_test(
    data_dir="deferred_materializer_matching_artifact_optimization",
)
async def test_cache_directory_cleanup(buck: Buck) -> None:
    # sqlite materializer state is already enabled
    cache_dir = Path(buck.cwd, "buck-out", "v2", "cache")
    materializer_state_dir = cache_dir / "materializer_state"
    command_hashes_dir = cache_dir / "command_hashes"
    materializer_state_dir.mkdir(parents=True)
    command_hashes_dir.mkdir(parents=True)

    # Need to run a command to start the daemon.
    await buck.audit_config()

    cache_dir_listing = list(cache_dir.iterdir())
    assert cache_dir_listing == [materializer_state_dir]

    await buck.kill()
    disable_sqlite_materializer_state(buck)
    await buck.audit_config()

    cache_dir_listing = list(cache_dir.iterdir())
    assert cache_dir_listing == []


@buck_test(
    data_dir="deferred_materializer_matching_artifact_optimization",
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_sqlite_materializer_state_matching_artifact_optimization(
    buck: Buck,
) -> None:
    # sqlite materializer state is already enabled
    target = "root//:copy"
    res = await buck.build(target)
    # Check output is correctly materialized
    assert res.get_build_report().output_for_target(target).exists()

    await buck.kill()

    res = await buck.build(target)
    # Check that materializer did not report any rematerialization
    assert "already materialized, updating deps only" in res.stderr, res.stderr
    assert "materialize artifact" not in res.stderr

    await buck.kill()

    # In this case, modifying `src` changes the output, so the output should be rematerialized
    with open(buck.cwd / "src", "w", encoding="utf-8") as f:
        f.write("SRC2")

    res = await buck.build(target)
    # Check output still exists
    output = res.get_build_report().output_for_target(target)
    assert output.exists()
    with open(output) as f:
        assert f.read().strip() == "SRC2"


@buck_test(
    data_dir="deferred_materializer_matching_artifact_optimization",
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_download_file_sqlite_matching_artifact_optimization(
    buck: Buck,
) -> None:
    # sqlite materializer state is already enabled
    target = "root//:download"
    res = await buck.build(target)
    # Check output is correctly materialized
    assert res.get_build_report().output_for_target(target).exists()

    await buck.kill()

    res = await buck.build(target)
    # Check that materializer did not report any rematerialization
    assert "already materialized, updating deps only" in res.stderr, res.stderr
    assert "materialize artifact" not in res.stderr


@buck_test(
    data_dir="deferred_materializer_matching_artifact_optimization",
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_sqlite_materializer_state_disabled(
    buck: Buck,
) -> None:
    disable_sqlite_materializer_state(buck)

    target = "root//:copy"
    result = await buck.build(target)
    # Check output is correctly materialized
    assert result.get_build_report().output_for_target(target).exists()

    await buck.kill()

    result = await buck.build(target)
    # Check that materializer did have to rematerialize the same artifact
    assert "already materialized, updating deps only" not in result.stderr
    assert "materialize artifact" in result.stderr


@buck_test(
    data_dir="deferred_materializer_matching_artifact_optimization",
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_sqlite_materializer_state_buckconfig_version_change(
    buck: Buck,
) -> None:
    # sqlite materializer state is already enabled
    target = "root//:copy"
    result = await buck.build(target)
    # Check output is correctly materialized
    assert result.get_build_report().output_for_target(target).exists()

    await buck.kill()

    # Bump the buckconfig version of sqlite materializer state to invalidate the existing sqlite db
    replace_in_file(
        "sqlite_materializer_state_version = 0",
        "sqlite_materializer_state_version = 1",
        buck.cwd / ".buckconfig",
    )

    # just starting the buck2 daemon should delete the sqlite materializer state
    await buck.audit_config()


def disable_sqlite_materializer_state(buck: Buck) -> None:
    config_file = buck.cwd / ".buckconfig"
    replace_in_file(
        "sqlite_materializer_state = true",
        "sqlite_materializer_state = false",
        file=config_file,
    )


@buck_test(
    data_dir="modify_deferred_materialization_deps",
    skip_for_os=["windows"],  # TODO(marwhal): Fix and enable on Windows
)
async def test_debug_materialize(buck: Buck) -> None:
    result = await buck.build("//:remote_text", "--materializations=None")
    out = result.get_build_report().output_for_target(
        "root//:remote_text", rel_path=True
    )
    assert not Path(buck.cwd, out).exists()

    await buck.debug("materialize", str(out))
    assert Path(buck.cwd, out).exists()
