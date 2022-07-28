import json
import re
import sys
import tempfile
from pathlib import Path

import pytest
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env, is_deployed_buck2


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


def remove_ansi_escape_sequences(ansi_str: str) -> str:
    """convert ansi_str to str"""
    ansi_escape = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")
    return ansi_escape.sub("", ansi_str)


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
async def test_sh_test(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test",
    )

    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_fail",
        ),
        stderr_regex="1 TESTS FAILED",
    )


@buck_test(inplace=True)
async def test_test_build_fail(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "hewwo_buck",
        ),
        stderr_regex="No such file or directory",
    )

    await buck.test("fbcode//buck2/tests/targets/rules/sh_test:test")


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_cpp_test(buck: Buck) -> None:
        await buck.test(
            "fbcode//buck2/tests/targets/rules/cxx:cpp_test_pass",
        )

        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/cxx:cpp_test_fail",
            ),
            stderr_regex="1 TESTS FAILED",
        )


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_python_test(buck: Buck) -> None:
        await buck.test("fbcode//buck2/tests/targets/rules/python/test:test")

        await buck.test("fbcode//buck2/tests/targets/rules/python/test:test_env")

        await expect_failure(
            buck.test("fbcode//buck2/tests/targets/rules/python/test:test_fail"),
            stderr_regex="1 TESTS FAILED",
        )

        await expect_failure(
            buck.test("fbcode//buck2/tests/targets/rules/python/test:test_fatal"),
            stderr_regex="1 TESTS FATALS",
        )

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_python_needed_coverage(buck: Buck) -> None:
        await buck.test(
            "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_pass",
            "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_pass_specific_file",
        )
        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_fail"
            ),
            stderr_regex="ERROR: Actual coverage [0-9.]*% is smaller than expected 100.% for file",
        )
        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/needed_coverage:test_fail_fractional"
            ),
            stderr_regex="ERROR: Actual coverage [0-9.]*% is smaller than expected [0-9.]*% for file",
        )

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_python_coverage(buck: Buck) -> None:
        for new_interface in ("true", "false"):
            with tempfile.NamedTemporaryFile("w") as covfile:
                await buck.test(
                    "@fbcode//mode/dbgo-cov",
                    "-c",
                    f"fbcode.use_new_testpilot_interface={new_interface}",
                    "fbcode//buck2/tests/targets/rules/python/coverage:test",
                    "--",
                    "--collect-coverage",
                    f"--coverage-output={covfile.name}",
                )
                paths = []
                with open(covfile.name) as results:
                    for line in results:
                        paths.append(json.loads(line)["filepath"])
            assert (
                "fbcode/buck2/tests/targets/rules/python/coverage/lib.py" in paths
            ), str(paths)


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_tests_attribute(buck: Buck) -> None:
        lib_tests = await buck.test("fbcode//buck2/tests/targets/rules/python/test:lib")

        assert "Pass 1" in remove_ansi_escape_sequences(lib_tests.stdout)


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_listing_failure(buck: Buck) -> None:
        await expect_failure(
            buck.test("fbcode//buck2/tests/targets/rules/python/broken:broken"),
            stdout_regex="Listing Fail 1",
        )


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_tests_dedupe(buck: Buck) -> None:
        lib_tests = await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:lib",
            "fbcode//buck2/tests/targets/rules/python/test:tests_for_lib",
        )

        assert "Pass 1" in remove_ansi_escape_sequences(lib_tests.stdout)


@pytest.mark.parametrize("build_filtered", [(True), (False)])
@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
async def test_label_filtering(buck: Buck, build_filtered: bool) -> None:
    cmd = ["fbcode//buck2/tests/targets/rules/label_test_filtering:"]
    if build_filtered:
        cmd.append("--build-filtered")

    await expect_failure(buck.test(*cmd), stderr_regex="1 TESTS FAILED")

    await expect_failure(
        buck.test(*cmd, "--exclude", "label-pass"), stderr_regex="1 TESTS FAILED"
    )

    await expect_failure(
        buck.test(*cmd, "--include", "label-fail"), stderr_regex="1 TESTS FAILED"
    )

    await expect_failure(
        buck.test(*cmd, "--include", "label-fail", "--exclude", "label-pass"),
        stderr_regex="1 TESTS FAILED",
    )

    await expect_failure(
        buck.test(
            *cmd,
        ),
        stderr_regex="1 TESTS FAILED",
    )

    await buck.test(*cmd, "--include", "label-pass")

    await buck.test(*cmd, "--exclude", "label-fail")

    await buck.test(*cmd, "--include", "!label-fail")

    await buck.test(
        *cmd, "--include", "label-fail", "--exclude", "label-fail", "--always-exclude"
    )

    await buck.test(*cmd, "--include", "!label-fail", "label-fail")


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_name_filtering(buck: Buck) -> None:
        await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test/...", "--", "test_env"
        )

        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/test/...", "--", "test_fail"
            ),
            stderr_regex="1 TESTS FAILED",
        )


if fbcode_linux_only():

    @buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
    async def test_compile_error(buck: Buck) -> None:
        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/compile_error:cpp_test_compile_error",
            ),
            stderr_regex="#error Compile error",
        )


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
async def test_cwd(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_cwd",
    )


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
async def test_default_label_filtering(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_fail_extended",
            "--",
            "--extended-tests",
        ),
        stderr_regex="1 TESTS FAILED",
    )

    # Ignores it by default
    await buck.test(
        "fbcode//buck2/tests/targets/rules/sh_test:test_fail_extended",
    )


@buck_test(inplace=True, data_dir="../")  # cwd is fbcode, we want it to be fbsource
async def test_stress_runs(buck: Buck) -> None:
    await expect_failure(
        buck.test(
            "fbcode//buck2/tests/targets/rules/sh_test:test_fail",
            "--",
            "--stress-runs",
            "10",
        ),
        stderr_regex="10 TESTS FAILED",
    )


# Not-in-place tests cannot run with deployed buck2
if not is_deployed_buck2():

    @buck_test(inplace=False, data_dir="testsof")
    async def test_target_compatibility(buck: Buck) -> None:
        # This excludes some tests
        out = await buck.test(
            "//...",
            "--target-platforms",
            "//:platform_default_tests",
        )

        assert "target incompatible node" in out.stderr

        await expect_failure(
            buck.test(
                "//:foo_extra_test",
                "--target-platforms",
                "//:platform_default_tests",
            ),
            stderr_regex="incompatible",
        )


@buck_test(inplace=True, data_dir="..")
async def test_external_runner_test_info_options(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/external_runner_test_info/...",
    )


@buck_test(inplace=True, data_dir="..")
async def test_allow_tests_on_re(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/external_runner_test_info/...",
        "--unstable-allow-tests-on-re",
    )


if not is_deployed_buck2():

    @buck_test(inplace=True, data_dir="..")
    @env("TEST_MAKE_IT_FAIL", "1")
    async def test_env_var_filtering(buck: Buck) -> None:
        await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
        )

        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/test:test",
                "--",
                "--env",
                "TEST_MAKE_IT_FAIL=1",
            ),
            stderr_regex="1 TESTS FAILED",
        )

    @buck_test(inplace=True, data_dir="..")
    @env("EXTRA_VAR", "foo")
    async def test_prepare_for_local_execution_env(buck: Buck, tmpdir) -> None:
        out = Path(str(tmpdir)) / "out"
        await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            "--",
            "--no-run-output-test-commands-for-fdb",
            str(out),
        )

        with open(out) as f:
            config = json.load(f)

        # Expect python/test:test target to support debugging. Executable field is populated only when debugging is supported.
        assert "debuggers" in config
        assert len(config["debuggers"]) > 0
        assert "executable" in config
        env = config["executable"]["env"]
        assert "PWD" in env
        assert "EXTRA_VAR" not in env


@buck_test(inplace=True, data_dir="..")
@env("BUCK2_TEST_TPX_USE_TCP", "true")
async def test_tcp(buck: Buck) -> None:
    await buck.test(
        "fbcode//buck2/tests/targets/rules/python/test:test",
    )


if not is_deployed_buck2():

    @buck_test(inplace=True, data_dir="..")
    async def test_passing_test_names_are_not_shown(buck: Buck) -> None:
        # Passing test headers are not shown unless we pass --print-passing-details explicitly.
        tests = await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
        )
        assert (
            "Pass: buck2/tests/targets/rules/python/test:test - test"
            not in tests.stderr
        )

    @buck_test(inplace=True, data_dir="..")
    async def test_failing_test_names_are_shown(buck: Buck) -> None:
        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/test:test",
                "--",
                "--env",
                "TEST_ENV=fail",
            ),
            stderr_regex="Fail: buck2/tests/targets/rules/python/test:test - test",
        )

    @buck_test(inplace=True, data_dir="..")
    async def test_no_print_passing_details(buck: Buck) -> None:
        # Without --print-passing-details, test headers and stdout is NOT displayed.
        tests = await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
        )
        assert (
            "Pass: buck2/tests/targets/rules/python/test:test - test"
            not in tests.stderr
        )
        assert "TESTED!" not in tests.stderr

    @buck_test(inplace=True, data_dir="..")
    async def test_print_passing_details(buck: Buck) -> None:
        # With --print-passing-details, test headers and stdout is displayed.
        tests = await buck.test(
            "fbcode//buck2/tests/targets/rules/python/test:test",
            "--",
            "--print-passing-details",
        )
        assert "Pass: buck2/tests/targets/rules/python/test:test - test" in tests.stderr
        assert "TESTED!" in tests.stderr

    @buck_test(inplace=True, data_dir="..")
    async def test_no_no_print_details(buck: Buck) -> None:
        # Without --no-print-details the stack trace is displayed.
        await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/test:test",
                "--",
                "--env",
                "TEST_ENV=fail",
            ),
            stderr_regex="AssertionError: 41 != 42",
        )

    @buck_test(inplace=True, data_dir="..")
    async def test_no_print_details(buck: Buck) -> None:
        # With --no-print-details the stack trace is not displayed.
        tests = await expect_failure(
            buck.test(
                "fbcode//buck2/tests/targets/rules/python/test:test",
                "--",
                "--env",
                "TEST_ENV=fail",
                "--no-print-details",
            ),
        )
        assert "AssertionError: 41 != 42" not in tests.stderr
