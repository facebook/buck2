# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict


import json
import os
import sys
from pathlib import Path
from typing import Any, Dict

import pytest
from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test, get_mode_from_platform
from buck2.tests.e2e_util.helper.utils import json_get, read_what_ran


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
            "--remote-only",
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


# In case of timeouts and failures, best would be to just disable this test.
@buck_test(inplace=True, skip_for_os=["windows"])
async def test_asic_platforms(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/asic_platforms:uses_asic_tool"
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
