# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

import json

import os
import shutil
import sys
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

_PACKAGE = "fbobjc/buck2/e2e_scenarios/targets/incremental_bundling"
_TARGET_LABEL = f"fbsource//{_PACKAGE}:App"


def is_running_on_macos() -> bool:
    return sys.platform == "darwin"


async def _build_app(buck: Buck, incremental: bool = True) -> Path:
    incremental_args = (
        ["-c", "apple.incremental_bundling_enabled=true"] if incremental else []
    )
    result = await buck.build(
        _TARGET_LABEL,
        "-c",
        "apple.codesign_type_override=skip",
        "--show-output",
        *incremental_args,
    )
    bundle_output_path = (
        _project_dir(buck) / result.get_target_to_build_output()[_TARGET_LABEL]
    )
    assert bundle_output_path.is_dir()
    return bundle_output_path


def _project_dir(buck: Buck) -> Path:
    # We are running from `fbcode`
    return buck.cwd.parent


def _mark_bundle_contents_via_mtime(bundle_path: Path) -> None:
    for path, _dirs, files in os.walk(bundle_path):
        for file in files:
            os.utime(os.path.join(path, file), (0, 0))


def _generate_build_trigger(buck: Buck, content: str = "") -> None:
    package_path = _project_dir(buck) / _PACKAGE
    build_trigger_path = package_path / "App/BuildTrigger.m"
    with open(build_trigger_path, "w") as f:
        f.write(content)


if not is_running_on_macos():

    def test_dummy_on_linux() -> None:
        pass


if is_running_on_macos():

    @buck_test(inplace=True)
    async def test_incremental_state_produced_on_re_is_materialized_prior_next_local_build(
        buck: Buck,
    ) -> None:
        package_path = _project_dir(buck) / _PACKAGE
        # First force remote build
        with open(package_path / "BUCK.fixture", "r") as src, open(
            package_path / "BUCK", "w"
        ) as dst:
            for line in src:
                if "_apple_tools" in line:
                    dst.write('_apple_tools = ":enforce_remote_execution",\n')
                else:
                    dst.write(line)

        _generate_build_trigger(buck)

        bundle_output_path = await _build_app(buck)
        incremental_state_path = bundle_output_path.parent / "incremental_state.json"
        assert not incremental_state_path.exists()

        _mark_bundle_contents_via_mtime(bundle_output_path)

        # Change the binary
        _generate_build_trigger(buck, content="int i = 1;")

        # Now build locally
        shutil.copy2(package_path / "BUCK.fixture", package_path / "BUCK")

        bundle_output_path = await _build_app(buck)

        for path, _dirs, files in os.walk(bundle_output_path):
            for file in files:
                file_path = os.path.join(path, file)
                statinfo = os.stat(file_path)
                if file == "App":
                    assert statinfo.st_mtime > 0, f"`{file_path}` should be modified"
                else:
                    assert (
                        statinfo.st_mtime == 0
                    ), f"`{file_path}` should not be modified"


if is_running_on_macos():

    @buck_test(inplace=True)
    async def test_when_only_binary_is_changed_then_only_binary_is_substituted(
        buck: Buck,
    ) -> None:
        package_path = _project_dir(buck) / _PACKAGE
        shutil.copy2(package_path / "BUCK.fixture", package_path / "BUCK")
        _generate_build_trigger(buck)

        bundle_output_path = await _build_app(buck)

        _mark_bundle_contents_via_mtime(bundle_output_path)

        _generate_build_trigger(buck, content="int i = 1;")

        bundle_output_path = await _build_app(buck)

        for path, _dirs, files in os.walk(bundle_output_path):
            for file in files:
                file_path = os.path.join(path, file)
                statinfo = os.stat(file_path)
                if file == "App":
                    assert statinfo.st_mtime > 0, f"`{file_path}` should be modified"
                else:
                    assert (
                        statinfo.st_mtime == 0
                    ), f"`{file_path}` should not be modified"


if is_running_on_macos():

    @buck_test(inplace=True)
    async def test_when_resource_dependency_is_deleted_then_only_resource_is_removed_from_bundle(
        buck: Buck,
    ) -> None:
        package_path = _project_dir(buck) / _PACKAGE
        shutil.copy2(package_path / "BUCK.fixture", package_path / "BUCK")
        _generate_build_trigger(buck)

        bundle_output_path = await _build_app(buck)

        _mark_bundle_contents_via_mtime(bundle_output_path)

        with open(package_path / "BUCK.fixture", "r") as src, open(
            package_path / "BUCK", "w"
        ) as dst:
            for line in src:
                if ":Resource" in line:
                    continue
                dst.write(line)

        bundle_output_path = await _build_app(buck)

        assert not (
            bundle_output_path / "xx.lproj"
        ).exists(), "Empty redundant directories should be removed"
        # Everything else should be untouched
        for path, _dirs, files in os.walk(bundle_output_path):
            for file in files:
                file_path = os.path.join(path, file)
                statinfo = os.stat(file_path)
                assert statinfo.st_mtime == 0, f"`{file_path}` should not be modified"


if is_running_on_macos():

    @buck_test(inplace=True)
    async def test_when_build_non_incremental_given_previous_build_was_incremental_then_incremental_state_is_invalidated(
        buck: Buck,
    ) -> None:
        package_path = _project_dir(buck) / _PACKAGE
        shutil.copy2(package_path / "BUCK.fixture", package_path / "BUCK")
        _generate_build_trigger(buck)

        bundle_output_path = await _build_app(buck)

        _mark_bundle_contents_via_mtime(bundle_output_path)

        bundle_output_path = await _build_app(buck, incremental=False)

        incremental_state_path = bundle_output_path.parent / "incremental_state.json"
        assert incremental_state_path.is_file()
        with open(incremental_state_path, "r") as f:
            incremental_state = json.load(f)
            assert (
                len(incremental_state) == 0
            ), "Incremental state should be invalidated"

        # Everything should be created from scratch
        for path, dirs, files in os.walk(bundle_output_path):
            for item in files + dirs:
                file_path = os.path.join(path, item)
                statinfo = os.stat(file_path)
                assert statinfo.st_mtime > 0, f"`{item}` should be modified"
