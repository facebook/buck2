import sys

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test

# Currently Rust rules don't work on Mac
def rust_linux_only() -> bool:
    return sys.platform == "linux"


@buck_test(inplace=True)
async def test_executable_genrule(buck: Buck) -> None:
    result = await buck.run(
        "fbcode//buck2/tests/targets/rules/genrule:executable_helper"
    )
    assert result.stdout.strip() == "hello"


@buck_test(inplace=True)
async def test_non_executable_genrule(buck: Buck) -> None:
    await expect_failure(
        buck.run("fbcode//buck2/tests/targets/rules/genrule:executable"),
        stderr_regex=r"Target `[^`]+` is not a binary rule \(only binary rules can be `run`\)",
    )


@buck_test(inplace=True)
async def test_executable_genrule_with_extra_args(buck: Buck) -> None:
    args = ["val", "--long", "-s", "spa  ces"]
    result = await buck.run(
        "fbcode//buck2/tests/targets/rules/genrule:executable_echo_args",
        "--",
        *args,
    )
    assert result.stdout.strip() == " ".join(args)


@buck_test(inplace=True)
async def test_executable_fail_to_build(buck: Buck) -> None:
    await expect_failure(
        buck.run("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_3"),
        stderr_regex=r"Failed to build artifact",
    )


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_rust_cdylib(buck: Buck) -> None:
        # This test checks that when we build a Rust cdylib, we dynamically link to
        # any underlying C++ libraries that are being used there. If we don't, then
        # we'll e.g. duplicate statics and break things such as singletons.
        result = await buck.run("fbcode//buck2/tests/targets/rules/rust/cdylib:main")
        msgs = [m.strip() for m in result.stdout.split("\n")]
        msgs = [m for m in msgs if m]
        assert msgs == ["from main", "initialized", "done", "from lib", "done"]


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_rust_ffi(buck: Buck) -> None:
        # Check an ffi binding to a shared .so
        result = await buck.run("fbcode//buck2/tests/targets/rules/rust:ffi")
        msgs = [m.strip() for m in result.stdout.split("\n")]
        msgs = [m for m in msgs if m]
        assert msgs == ["Hello from C: 42!"]


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_rust_ffi_failure_filter(buck: Buck) -> None:
        # Check an ffi binding to a shared .so (with failure_filtering, which was a regression)
        result = await buck.run(
            "fbcode//buck2/tests/targets/rules/rust:ffi",
            "-c",
            "rust.failure_filter=true",
        )
        msgs = [m.strip() for m in result.stdout.split("\n")]
        msgs = [m for m in msgs if m]
        assert msgs == ["Hello from C: 42!"]
