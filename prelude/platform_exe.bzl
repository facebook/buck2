# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//os_lookup:defs.bzl", "Os", "OsLookup")

def resolve_platform_exe(
    exe: Artifact | Dependency | None,
    platform_exe: dict[str, Dependency],
    target_os: OsLookup,
) -> RunInfo | dict[str, RunInfo]:
    if target_os.os == Os("fat_mac_linux") and len(platform_exe) > 0:
        return {
            "Darwin": _os_base(exe, platform_exe, "macos"),
            "Linux": _os_base(exe, platform_exe, "linux"),
        }
    return _os_base(exe, platform_exe, target_os.os.value)

def _os_base(exe, platform_exe, os: str) -> RunInfo:
    value = platform_exe.get(os)
    if value == None:
        value = exe
    return _run_info(value)

def _run_info(exe: Artifact | Dependency | None) -> RunInfo:
    if exe == None:
        return RunInfo()

    if isinstance(exe, Artifact):
        return RunInfo(args = cmd_args(exe))

    run_info = exe.get(RunInfo)
    if run_info == None:
        run_info = RunInfo(
            args = exe[DefaultInfo].default_outputs,
        )

    return run_info
