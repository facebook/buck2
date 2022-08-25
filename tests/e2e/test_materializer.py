import sys
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env


"""
If you need to add a directory that's isolated in buck2/test/targets
(ex. some test of form @buck_test(inplace=False, data_dir=some_new_directory)),
then you will need to update isolated_targets in buck2/test/targets/TARGETS.
Otherwise the test will fail because it cannot recognize the new directory.
"""

# Eden materializer only available on Linux
def eden_linux_only() -> bool:
    return sys.platform == "linux"


def watchman_dependency_linux_only() -> bool:
    return sys.platform == "linux"


def replace_in_file(old: str, new: str, file: Path, encoding: str = "utf-8") -> None:
    with open(file, encoding=encoding) as f:
        file_content = f.read()
    file_content = file_content.replace(old, new)
    with open(file, "w", encoding=encoding) as f:
        f.write(file_content)


@buck_test(inplace=False, data_dir="modify_deferred_materialization")
async def test_modify_input_source(buck: Buck) -> None:
    await buck.build("//:urandom_dep")

    targets_file = buck.cwd / "TARGETS.fixture"

    # Change the label in Targets.
    replace_in_file("__NOT_A_REAL_LABEL__", "buck2_test_local_exec", file=targets_file)

    await buck.build("//:urandom_dep")


@buck_test(inplace=False, data_dir="modify_deferred_materialization_deps")
async def test_modify_dep_materialization(buck: Buck) -> None:
    await buck.build("//:check")

    with open(buck.cwd / "text", "w", encoding="utf-8") as f:
        f.write("TEXT2")

    await buck.build("//:check")


@buck_test(
    inplace=False, data_dir="deferred_materializer_matching_artifact_optimization"
)
@env("BUCK_LOG", "buck2_build_api::execute::materializer=trace")
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
    assert "already materialized, no need to declare again" in result.stderr
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
    inplace=False, data_dir="deferred_materializer_matching_artifact_optimization"
)
@env("BUCK_LOG", "buck2_build_api::execute::materializer=trace")
async def test_disabling_matching_artifact_optimization(buck: Buck) -> None:
    # Disable local caching of RE artifacts
    buckconfig_file = buck.cwd / ".buckconfig"
    replace_in_file(
        "enable_local_caching_of_re_artifacts = true",
        "enable_local_caching_of_re_artifacts = false",
        file=buckconfig_file,
    )

    target = "root//:copy"
    result = await buck.build(target)
    # Check output is correctly materialized
    assert result.get_build_report().output_for_target(target).exists()

    with open(buck.cwd / "hidden", "w", encoding="utf-8") as f:
        f.write("HIDDEN2")

    result = await buck.build(target)
    # Check output still exists
    assert result.get_build_report().output_for_target(target).exists()
    # Check that materializer did have to rematerialize in this case
    assert "already materialized, no need to declare again" not in result.stderr
    assert "materialize artifact" in result.stderr


# Doesn't matter which fake repository we use. We just a generic one.
@buck_test(inplace=False, data_dir="modify_deferred_materialization")
async def test_cache_directory_is_always_empty(buck: Buck) -> None:
    cache_dir = Path(buck.cwd, "buck-out", "v2", "cache")
    cache_dir.mkdir(parents=True)

    # Need to run a command to start the daemon.
    await buck.audit_config()

    cache_dir_listing = list(cache_dir.iterdir())
    assert len(cache_dir_listing) == 0


if eden_linux_only():

    @buck_test(inplace=False, data_dir="eden_materializer")
    async def test_eden_materialization_simple(buck: Buck) -> None:
        await buck.build("//:simple")


def set_materializer(buck: Buck, old: str, new: str) -> None:
    config_file = buck.cwd / ".buckconfig"

    # Change the label in Targets.
    old_config = "materializations = {}".format(old)
    new_config = "materializations = {}".format(new)
    replace_in_file(old_config, new_config, file=config_file)


if eden_linux_only():

    @buck_test(inplace=False, data_dir="eden_materializer")
    async def test_eden_materialization_clean_after_config_change(buck: Buck) -> None:
        set_materializer(buck, "eden", "deferred")
        await buck.build("//:simple")

        set_materializer(buck, "deferred", "eden")
        await buck.kill()
        await buck.build("//:simple")


if eden_linux_only():

    @buck_test(inplace=False, data_dir="eden_materializer")
    async def test_eden_materialization_no_config_change(buck: Buck) -> None:
        await buck.build("//:simple")
        await buck.kill()
        await buck.build("//:simple")
