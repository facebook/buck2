# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import asyncio
import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict, Optional, Tuple

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.api.buck_result import BuckException, BuildResult
from buck2.tests.e2e_util.api.process import Process
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, env, get_mode_from_platform
from buck2.tests.e2e_util.helper.utils import json_get, random_string, read_what_ran


# rust rule implementations hardcode invocation of `/bin/jq` which is not available on Mac RE workers (or mac laptops)
def rust_linux_only() -> bool:
    return sys.platform == "linux"


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


@buck_test(inplace=True)
async def test_buildfiles(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile")


@buck_test(inplace=True)
async def test_build_output(buck: Buck) -> None:
    output_path = os.path.join(
        "fbcode",
        "buck2",
        "tests",
        "targets",
        "interpreter",
        "buildfiles",
        "TARGETS",
    )

    result = await buck.build_without_report(
        "fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile",
        "--show-output",
    )
    assert (
        f"fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile {output_path}\n"
        == result.stdout
    )

    result = await buck.build_without_report(
        "fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile",
        "--show-simple-output",
    )
    assert f"{output_path}\n" == result.stdout

    result = await buck.build_without_report(
        "fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile",
        "--show-json-output",
    )

    # Escaping backslashes needed for windows paths
    json_escaped_output_path = output_path.replace("\\", "\\\\")
    assert (
        f'{{"fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile":"{json_escaped_output_path}"}}\n'
        == result.stdout
    )


def extract_gen_folder(output: str) -> str:
    return output[: output.find("{0}gen{0}".format(os.path.sep)) + 4]


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_build_symlink_rust_rule(buck: Buck) -> None:
        args = [
            "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
            "--show-full-output",
        ]
        if sys.platform == "darwin":
            args.append("@//mode/mac")
        result = await buck.build(*args)

        output_dict = result.get_target_to_build_output()
        for _target, output in output_dict.items():
            gen_folder = extract_gen_folder(output)
            # v1: buck2/tests/targets/rules/rust/hello_world/welcome#binary/welcome
            symlink = (
                Path(gen_folder)
                / "fbcode"
                / "buck2"
                / "tests"
                / "targets"
                / "rules"
                / "rust"
                / "hello_world"
                / "welcome"
            )
            assert symlink.is_symlink()


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_build_symlink_python_rule(buck: Buck) -> None:
        args = [
            "fbcode//buck2/tests/targets/rules/python/hello_world:welcome",
            "--show-full-output",
        ]
        if sys.platform == "darwin":
            args.append("@//mode/mac")
        result = await buck.build(*args)
        output_dict = result.get_target_to_build_output()
        for _target, output in output_dict.items():
            gen_folder = extract_gen_folder(output)
            # v1: buck2/tests/targets/rules/python/hello_world/welcome.par
            symlink = (
                Path(gen_folder)
                / "fbcode"
                / "buck2"
                / "tests"
                / "targets"
                / "rules"
                / "python"
                / "hello_world"
                / "welcome.par"
            )
            assert symlink.is_symlink()


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_build_symlink_cpp_rule(buck: Buck) -> None:
        args = [
            "fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome",
            "--show-full-output",
        ]
        if sys.platform == "darwin":
            args.append("@//mode/mac")
        result = await buck.build(*args)
        output_dict = result.get_target_to_build_output()
        for _target, output in output_dict.items():
            gen_folder = extract_gen_folder(output)
            # v1: buck2/tests/targets/rules/cxx/hello_world/welcome
            symlink = (
                Path(gen_folder)
                / "fbcode"
                / "buck2"
                / "tests"
                / "targets"
                / "rules"
                / "cxx"
                / "hello_world"
                / "welcome"
            )
            assert symlink.is_symlink()


@buck_test(inplace=True)
async def test_build_symlink_genrule_rule(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/genrule/hello_world:welcome",
        "--show-full-output",
        get_mode_from_platform(),
    ]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        gen_folder = extract_gen_folder(output)
        # v1: buck2/tests/targets/rules/genrule/hello_world/welcome/out.txt
        symlink = (
            Path(gen_folder)
            / "fbcode"
            / "buck2"
            / "tests"
            / "targets"
            / "rules"
            / "genrule"
            / "hello_world"
            / "out"
            / "out.txt"
        )
        assert symlink.is_symlink()


@buck_test(inplace=True)
async def test_build_symlink_genrule_rule_outs(buck: Buck) -> None:
    # Test this using projected artifacts.
    args = [
        "fbcode//buck2/tests/targets/rules/genrule/hello_world:outs",
        "--show-full-output",
        get_mode_from_platform(),
    ]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        gen_folder = extract_gen_folder(output)
        symlink = (
            Path(gen_folder)
            / "fbcode"
            / "buck2"
            / "tests"
            / "targets"
            / "rules"
            / "genrule"
            / "hello_world"
            / "out"
        )
        assert symlink.is_symlink()
        assert (symlink / "foo" / "out.txt").is_file()


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_build_symlink_sh_binary(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/shell:diff"
    args = [target, "--show-full-output", get_mode_from_platform()]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()

    output = output_dict[target]
    gen_folder = extract_gen_folder(output)
    symlink = (
        Path(gen_folder) / "fbcode" / "buck2" / "tests" / "targets" / "rules" / "shell"
    )
    if sys.platform == "win32":
        symlink /= "diff.bat"
    else:
        symlink /= "diff"

    # Verify we can both versions:
    subprocess.check_call([output])
    subprocess.check_call([symlink])


@buck_test(inplace=True)
async def test_build_symlink_does_not_traverse_existing_symlinks(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/shell:diff"

    args = [target, "--show-full-output", get_mode_from_platform()]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()

    output = output_dict[target]
    gen_folder = extract_gen_folder(output)
    symlink_folder = (
        Path(gen_folder) / "fbcode" / "buck2" / "tests" / "targets" / "rules" / "shell"
    )

    # Now, overwrite part of the symlink path with something we cannot traverse.
    path = symlink_folder.parent
    shutil.rmtree(path)
    # On Windows this is just non existing path.
    os.symlink("/dev/null", path)

    # Can we still build? If we delete the symlink when walking up the path, we
    # can. If we traverse it, we can't.
    await buck.build(*args)


@buck_test(inplace=True)
async def test_sh_binary_no_append_extension(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/shell:no_extension"
    args = [target, "--show-full-output", get_mode_from_platform()]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    output = Path(output_dict[target])

    # Verify that we created the script symlink without an extension
    assert (output.parent / "resources" / "no_extension").is_symlink()

    # And that we're calling it without an extension as well
    last_script_line = output.read_text().splitlines()[-1]
    if sys.platform == "win32":
        assert "%BUCK_PROJECT_ROOT%\\no_extension %*" in last_script_line
    else:
        assert '"$BUCK_PROJECT_ROOT/no_extension" "$@"' in last_script_line


@buck_test(inplace=True)
async def test_cquery(buck: Buck) -> None:
    result = await buck.cquery(
        """deps(fbcode//buck2/tests/targets/commands:exported)"""
    )
    assert "fbcode//buck2/tests/targets/commands:exported" in result.stdout


@buck_test(inplace=True)
async def test_cquery_with_config_value(buck: Buck) -> None:
    deps_enabled_result = await buck.cquery(
        "--config",
        "user.deps_enabled=true",
        "deps(fbcode//buck2/tests/targets/commands:lib)",
    )
    assert "fbcode//buck2/tests/targets/commands:dynamic" in deps_enabled_result.stdout

    deps_disabled_result = await buck.cquery(
        "--config",
        "user.deps_enabled=false",
        "deps(fbcode//buck2/tests/targets/commands:lib)",
    )
    assert (
        "fbcode//buck2/tests/targets/commands:dynamic"
        not in deps_disabled_result.stdout
    )


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_show_output(buck: Buck) -> None:
        TARGET = "fbcode//buck2/tests/targets/rules/genrule:executable_helper"
        result = await buck.build(TARGET, "--show-output")

        build_report = result.get_build_report()
        build_report_outputs = [
            (TARGET, str(output)) for output in build_report.outputs_for_target(TARGET)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs

        TARGET = "fbcode//buck2/tests/targets/rules/rust:hello_explicit"
        result = await buck.build(TARGET, "--show-output")

        build_report = result.get_build_report()
        build_report_outputs = [
            (TARGET, str(output)) for output in build_report.outputs_for_target(TARGET)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs

        TARGET = "fbcode//buck2/tests/targets/rules/cxx:my_cpp1"
        SUBTARGET = "compilation-database"
        TARGET_WITH_SUBTARGET = (
            "fbcode//buck2/tests/targets/rules/cxx:my_cpp1[compilation-database]"
        )
        result = await buck.build(TARGET_WITH_SUBTARGET, "--show-output")

        build_report = result.get_build_report()
        build_report_outputs = [
            (TARGET_WITH_SUBTARGET, str(output))
            for output in build_report.outputs_for_target(TARGET, SUBTARGET)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs


@buck_test(inplace=True)
async def test_show_full_output(buck: Buck) -> None:
    TARGET = "fbcode//buck2/tests/targets/rules/genrule:executable_helper"
    result = await buck.build(TARGET, "--show-full-output")

    build_report = result.get_build_report()
    build_report_outputs = [
        (TARGET, str(output)) for output in build_report.outputs_for_target(TARGET)
    ]
    show_output_outputs = list(result.get_target_to_build_output().items())

    assert show_output_outputs == build_report_outputs

    for _, output in show_output_outputs:
        assert os.path.isabs(output), f"Output path must be absolute, got `{output}`."
        assert os.path.exists(output), f"Output path `{output}` does not exist!"


@buck_test(inplace=True)
@env("BUCK_LOG", "info")
async def test_consistent_build(buck: Buck) -> None:
    args = ["fbcode//buck2/tests/targets/rules/genrule:"]
    if sys.platform == "win32":
        args.append("@//mode/win")
    result0 = await buck.build(*args)
    await buck.kill()
    result1 = await buck.build(*args)
    # Don't know if action key should stay consistent between clean builds,
    # but number of cache misses should.
    assert sum(result0.get_action_to_cache_miss_count().values()) == sum(
        result1.get_action_to_cache_miss_count().values()
    )

    build_report0 = result0.get_build_report()
    build_report1 = result1.get_build_report()

    # Output path should stay the same between builds, in particular the configuration hash.
    TARGET = "fbcode//buck2/tests/targets/rules/genrule:my_genrule1"
    build_report0_outputs = [
        (TARGET, str(output)) for output in build_report0.outputs_for_target(TARGET)
    ]
    build_report1_outputs = [
        (TARGET, str(output)) for output in build_report1.outputs_for_target(TARGET)
    ]
    assert build_report0_outputs == build_report1_outputs


@buck_test(inplace=True)
@env("BUCK_LOG", "info")
async def test_cached_build(buck: Buck) -> None:
    args = ["fbcode//buck2/tests/targets/rules/genrule:"]
    if sys.platform == "win32":
        args.append("@//mode/win")
    await buck.build(*args)
    result = await buck.build(*args)
    # Should be empty since nothing needs to be rebuilt
    assert sum(result.get_action_to_cache_miss_count().values()) == 0


@buck_test(inplace=True)
async def test_build_test_dependencies(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/sh_test:test_with_env"
    build = await buck.build(
        target,
        "--build-test-info",
        "--build-report",
        "-",
    )
    report = build.get_build_report().build_report

    path = ["results", target, "other_outputs"]
    for p in path:
        report = report[p]

    has_file = False
    for artifact in report:
        if "__file__" in artifact:
            has_file = True

    assert not has_file


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_fat_platforms(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/fat_platforms:example_use"
    result = await buck.build(
        target,
        "-c",
        "build.execution_platforms=fbcode//buck2/tests/targets/fat_platforms:platforms",
        "--show-full-output",
    )
    output = result.get_target_to_build_output()[target]
    with open(output) as output:
        s = output.read()
        assert "darwin" in s, "expected 'darwin' in output: `{}`".format(output)
        assert "linux" in s, "expected 'darwin' in output: `{}`".format(output)


@buck_test(inplace=True)
async def test_classpath_query(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/template_placeholder/...")


@buck_test(inplace=True)
async def test_missing_outputs_error(buck: Buck) -> None:
    # Check that we a) say what went wrong, b) show the command
    await expect_failure(
        buck.build(
            "fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad",
            # We really should make this an isolated test to avoid having to set this.
            "-c",
            "build.use_limited_hybrid=True",
        ),
        stderr_regex="(Action failed to produce output.*frecli|frecli.*OUTMISS)",
    )

    # Same, but locally.
    await expect_failure(
        buck.build(
            "fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_local"
        ),
        stderr_regex="Action failed to produce outputs.*Stdout:\nHELLO_STDOUT.*Stderr:\nHELLO_STDERR",
    )


@buck_test(inplace=True)
async def test_local_execution(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/genrule:echo_pythonpath"

    await buck.kill()
    res = await buck.build(target, env={"PYTHONPATH": "foobar"})

    build_report = res.get_build_report()
    output = build_report.output_for_target(target)
    assert output.read_text().rstrip() == ""


if fbcode_linux_only():  # noqa: C901

    @buck_test(inplace=True)
    async def test_instruction_count_disabled(buck: Buck) -> None:
        package = "fbcode//buck2/tests/targets/rules/instruction_counts"
        name = "three_billion_instructions"

        await buck.build(
            f"{package}:{name}",
            "-c",
            "buck2.miniperf2=false",
            "--no-remote-cache",
            "--local-only",
            "-c",
            f"test.cache_buster={random_string()}",
        )

        log = (await buck.log("show")).stdout.strip().splitlines()
        for line in log:
            commands = json_get(
                line, "Event", "data", "SpanEnd", "data", "ActionExecution", "commands"
            )

            for c in commands or []:
                assert c["details"]["metadata"].get("execution_stats") is None

    async def get_matching_details(
        buck: Buck, package: str, name: str
    ) -> Dict[str, Any]:
        details = None
        log = (await buck.log("show")).stdout.strip().splitlines()
        for line in log:
            action = json_get(
                line,
                "Event",
                "data",
                "SpanEnd",
                "data",
                "ActionExecution",
            )

            if action is None:
                continue

            if action["name"]["category"] != "genrule":
                continue

            label = action["key"]["owner"]["TargetLabel"]["label"]
            if label["package"] != package:
                continue
            if label["name"] != name:
                continue

            details = action["commands"][-1]["details"]
            return details

        raise AssertionError("did not find the expected target")

    @buck_test(inplace=True)
    async def test_instruction_count_enabled(buck: Buck) -> None:
        package = "fbcode//buck2/tests/targets/rules/instruction_counts"
        name = "three_billion_instructions"
        await buck.build(
            f"{package}:{name}",
            "-c",
            "buck2.miniperf2=true",
            "--no-remote-cache",
            "--local-only",
            "-c",
            f"test.cache_buster={random_string()}",
        )

        details = await get_matching_details(buck, package, name)
        assert "OmittedLocalCommand" in details["command_kind"]["command"]

        # Check that we are within 10%
        instruction_count = details["metadata"]["execution_stats"][
            "cpu_instructions_user"
        ]
        assert instruction_count > 2850000000
        assert instruction_count < 3150000000

    @buck_test(inplace=True)
    async def test_instruction_count_remote(buck: Buck) -> None:
        package = "fbcode//buck2/tests/targets/rules/instruction_counts"
        name = "three_billion_instructions"
        await buck.build(
            f"{package}:{name}",
            "--no-remote-cache",
            "--write-to-cache-anyway",
            "--remote-only",
        )

        details = await get_matching_details(buck, package, name)
        assert not details["command_kind"]["command"]["RemoteCommand"]["cache_hit"]

        # Check that we are within 10%
        instruction_count = details["metadata"]["execution_stats"][
            "cpu_instructions_user"
        ]
        assert instruction_count > 2850000000
        assert instruction_count < 3150000000

        # Check we also get it on a cache hit.

        await buck.kill()
        await buck.build(
            f"{package}:{name}",
            "--remote-only",
        )

        details = await get_matching_details(buck, package, name)
        assert details["command_kind"]["command"]["RemoteCommand"]["cache_hit"]

        # Check that we are within 10%
        instruction_count = details["metadata"]["execution_stats"][
            "cpu_instructions_user"
        ]
        assert instruction_count > 2850000000
        assert instruction_count < 3150000000


# This test relies on `buck2-asic-devinfra` use-case and `asic-grid` platform.
# In case of timeouts and failures, best would be to just disable this test.
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_asic_platforms(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/asic_platforms:uses_asic_grid_tool"
    result = await buck.build(
        target,
        "--show-full-output",
    )
    output = result.get_target_to_build_output()[target]
    with open(output) as output:
        s = output.read()
        assert "facebook.com" in s, "expected 'facebook.com' in output: `{}`".format(
            output
        )


@buck_test(inplace=True)
async def test_exit_when_different_state(buck: Buck) -> None:
    a = buck.build(
        "@fbcode//mode/dev",
        "--exit-when-different-state",
        "fbcode//buck2/tests/targets/exit_when_different_state:long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    b = buck.build(
        "@fbcode//mode/opt",
        "--exit-when-different-state",
        "fbcode//buck2/tests/targets/exit_when_different_state:long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    # create a coroutine that can return a result
    async def process(
        p: Process[BuildResult, BuckException],
    ) -> Tuple[Optional[int], str]:
        result = await expect_failure(p)
        return (result.process.returncode, result.stderr)

    done, pending = await asyncio.wait(
        [process(a), process(b)],
        timeout=10,
        return_when=asyncio.FIRST_COMPLETED,
    )

    assert len(done) == 1
    assert len(pending) == 1

    # these are sets, so can't index them.
    for task in done:
        exit_code, stderr = task.result()
        assert "daemon is busy" in stderr
        assert exit_code == 4


@buck_test(inplace=True)
@pytest.mark.parametrize("same_state", [True, False])
async def test_exit_when_preemptible_always(buck: Buck, same_state: bool) -> None:
    a = buck.build(
        "@fbcode//mode/dev",
        "--preemptible=always",
        "fbcode//buck2/tests/targets/exit_when_different_state:long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    b = buck.build(
        # We expect to ALWAYS preempt commands, to prevent blocking new callees
        "@fbcode//mode/dev" if same_state else "@fbcode//mode/opt",
        "--preemptible=always",
        "fbcode//buck2/tests/targets/exit_when_different_state:long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    # create a coroutine that can return a result
    async def process(
        p: Process[BuildResult, BuckException],
    ) -> Tuple[Optional[int], str]:
        result = await expect_failure(p)
        return (result.process.returncode, result.stderr)

    done, pending = await asyncio.wait(
        [process(a), process(b)],
        timeout=10,
        return_when=asyncio.FIRST_COMPLETED,
    )

    assert len(done) == 1
    assert len(pending) == 1

    # these are sets, so can't index them.
    for task in done:
        exit_code, stderr = task.result()
        assert "daemon preempted" in stderr
        assert exit_code == 5


@buck_test(inplace=True)
@pytest.mark.parametrize("same_state", [True, False])
async def test_exit_when_preemptible_on_different_state(
    buck: Buck, same_state: bool
) -> None:
    a = buck.build(
        "@fbcode//mode/dev",
        "--preemptible=ondifferentstate",
        "fbcode//buck2/tests/targets/exit_when_different_state:long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    b = buck.build(
        # We expect to ALWAYS preempt commands, to prevent blocking new callees
        "@fbcode//mode/dev" if same_state else "@fbcode//mode/opt",
        "--preemptible=ondifferentstate",
        "fbcode//buck2/tests/targets/exit_when_different_state:long_running_target",
        "--local-only",
        "--no-remote-cache",
    )

    # create a coroutine that can return a result
    async def process(
        p: Process[BuildResult, BuckException],
    ) -> Tuple[Optional[int], str]:
        result = await expect_failure(p)
        return (result.process.returncode, result.stderr)

    done, pending = await asyncio.wait(
        [process(a), process(b)],
        timeout=10,
        return_when=asyncio.FIRST_COMPLETED,
    )

    if same_state:
        # No preempt when state is the same
        assert len(done) == 0
        assert len(pending) == 2
    else:
        assert len(done) == 1
        assert len(pending) == 1

    # These are sets, so can't index them. Expect all done tasks to be "done" because they're preempted
    for task in done:
        exit_code, stderr = task.result()
        assert "daemon preempted" in stderr
        assert exit_code == 5


@buck_test(inplace=True)
async def test_genrule_with_remote_execution_dependencies(buck: Buck) -> None:
    result = await buck.build(
        get_mode_from_platform(),
        "fbcode//buck2/tests/targets/rules/genrule/re_dependencies:remote_execution_dependencies",
        "--config",
        "build.default_remote_execution_use_case=buck2-testing",
        "--no-remote-cache",
        "--remote-only",
        "--show-full-output",
    )
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        with Path(output).open() as f:
            deps = json.load(f)
        assert len(deps) == 1
        assert deps[0]["smc_tier"] == "noop"
        assert deps[0]["id"] == "foo"
        # reservation_id is a random string which is 20 characters long
        assert len(deps[0]["reservation_id"]) == 20


async def read_io_provider_for_last_build(buck: Buck) -> None:
    log = (await buck.log("show")).stdout
    for line in log.splitlines():
        io_provider = json_get(
            line,
            "Event",
            "data",
            "SpanStart",
            "data",
            "Command",
            "metadata",
            "io_provider",
        )
        if io_provider:
            return io_provider

    raise Exception("Could not find io_provider")


# FIXME(JakobDegen): This test is flakey due to something in the apple toolchain that I don't
# understand. The flakeyness needs to be fixed, or better yet, this needs to be made isolated so
# that people don't have to learn things about apple toolchains to debug it
if False:

    @buck_test(
        inplace=True,
        skip_for_os=["windows"],
        extra_buck_config={
            "buck2": {
                "allow_eden_io": "false",
                "digest_algorithms": "BLAKE3-KEYED",
                "source_digest_algorithm": "BLAKE3-KEYED",
            }
        },
    )
    async def test_source_hashing_blake3_only(buck: Buck) -> None:
        target = "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome"

        await buck.build(target, "--no-remote-cache", "--remote-only")
        run1 = await read_what_ran(buck)

        io_provider = await read_io_provider_for_last_build(buck)
        assert io_provider == "fs"

        await buck.kill()
        await buck.build(
            target,
            "--no-remote-cache",
            "--remote-only",
            env={"BUCK2_DISABLE_FILE_ATTR": "true"},
        )
        run2 = await read_what_ran(buck)

        def key(entry: Dict[str, Any]) -> str:
            return entry["identity"]

        assert sorted(run1, key=key) == sorted(run2, key=key)

    @buck_test(
        inplace=True,
        skip_for_os=["windows"],
        extra_buck_config={
            "buck2": {
                "allow_eden_io": "true",
                "digest_algorithms": "BLAKE3-KEYED",
                "source_digest_algorithm": "BLAKE3-KEYED",
            }
        },
    )
    async def test_source_hashing_eden_blake3_only(buck: Buck) -> None:
        if not os.path.exists(buck.cwd / ".eden"):
            pytest.skip("This test is meaningless if not using Eden")  # pyre-ignore

        # Check we have Eden I/O
        await buck.build()
        io_provider = await read_io_provider_for_last_build(buck)

        # If our test didn't use Eden then that means the current host's Eden is too old.
        # Skip in this case, unless

        if io_provider != "eden" and os.environ.get("SANDCASTLE") is None:
            pytest.skip("Unsupported Eden version")  # pyre-ignore

        # On Sandcastle we'll assert we *are* using Eden to make sure this test
        # isn't just always skipping.
        assert io_provider == "eden"

        target = "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome"

        await buck.build(target, "--no-remote-cache", "--remote-only")
        run1 = await read_what_ran(buck)

        with open(buck._env["BUCK2_TEST_EXTRA_EXTERNAL_CONFIG"], "a") as f:
            f.write("[buck2]\n")
            f.write("allow_eden_io = false")

        await buck.kill()
        await buck.build(
            target,
            "--no-remote-cache",
            "--remote-only",
            env={"BUCK2_DISABLE_FILE_ATTR": "true"},
        )
        run2 = await read_what_ran(buck)

        io_provider = await read_io_provider_for_last_build(buck)
        assert io_provider == "fs"

        def key(entry: Dict[str, Any]) -> str:
            return entry["identity"]

        assert sorted(run1, key=key) == sorted(run2, key=key)

    @buck_test(
        inplace=True,
        skip_for_os=["windows"],
        extra_buck_config={
            "buck2": {
                "allow_eden_io": "false",
                "digest_algorithms": "BLAKE3-KEYED,SHA1",
                "source_digest_algorithm": "SHA1",
            }
        },
    )
    async def test_source_hashing(buck: Buck) -> None:
        if not os.path.exists(buck.cwd / ".eden"):
            pytest.skip("This test is meaningless if not using Eden")  # pyre-ignore

        target = "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome"

        await buck.build(target, "--no-remote-cache", "--remote-only")
        run1 = await read_what_ran(buck)

        await buck.kill()
        await buck.build(
            target,
            "--no-remote-cache",
            "--remote-only",
            env={"BUCK2_DISABLE_FILE_ATTR": "true"},
        )
        run2 = await read_what_ran(buck)

        def key(entry: Dict[str, Any]) -> str:
            return entry["identity"]

        assert sorted(run1, key=key) == sorted(run2, key=key)


@buck_test(inplace=True, allow_soft_errors=True)
async def test_eden_io_with_mismatched_root(buck: Buck) -> None:
    cwd = Path("buck2") / "tests" / "targets" / "eden_io"
    await buck.build("//...", rel_cwd=cwd)
