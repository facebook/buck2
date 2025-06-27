# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


import json
import os
import shutil
import subprocess
import sys
from pathlib import Path
from typing import Any, Dict

import pytest

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform
from buck2.tests.e2e_util.helper.utils import json_get, random_string, read_what_ran


# rust rule implementations hardcode invocation of `/bin/jq` which is not available on Mac RE workers (or mac laptops)
def rust_linux_only() -> bool:
    return sys.platform == "linux"


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


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
        # disable resource control as it uses miniperf to read cgroup's memory peak
        env = {"BUCK2_TEST_RESOURCE_CONTROL_CONFIG": '{"status":"Off"}'}

        await buck.build(
            f"{package}:{name}",
            "-c",
            "buck2.miniperf2=false",
            "--no-remote-cache",
            "--local-only",
            "-c",
            f"test.cache_buster={random_string()}",
            env=env,
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
