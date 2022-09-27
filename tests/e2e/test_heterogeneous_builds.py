import sys

from typing import List

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


def running_on_linux() -> bool:
    return sys.platform == "linux"


def _apple_build_command_args(use_macos_toolchain: bool) -> List[str]:
    toolchain_mode_file = (
        "@fbsource//fbobjc/mode/buck2/toolchains/xcode-13.4"
        if use_macos_toolchain
        else "@fbsource//fbobjc/mode/buck2/toolchains/pika-14-linux"
    )
    args = [
        "fbsource//fbobjc/buck2/samples/app_with_watch:DemoApp",
        toolchain_mode_file,
    ]

    if running_on_linux():
        args += [
            "-c",
            # Allow usages of fb_xplat_* macros
            "xplat.available_platforms=APPLE,CXX",
        ]

    return args


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_if_windows=True)
async def test_heterogeneous_apple_builds(buck: Buck) -> None:
    # This test must be run from _both_ a macOS and a Linux host. The idea
    # is that we want to test all four combinations:
    #
    # - macOS host, macOS toolchain (non-heterogeneous)
    # - macOS host, Linux toolchain (heterogeneous)
    # - Linux host, Linux toolchain (non-heterogeneous)
    # - Linux host, macOS toolchain (heterogeneous)
    #
    # Note that we're also running the non-heterogeneous variants
    # because we want to ensure completeness of the toolchain tests.
    #
    # The test themselves are very fast, so it's all about making sure
    # the combinations work across all OS and toolchain combinations.
    await buck.build(*_apple_build_command_args(use_macos_toolchain=True))
    await buck.build(*_apple_build_command_args(use_macos_toolchain=False))


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
