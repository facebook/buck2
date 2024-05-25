# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//toolchains:cxx.bzl", "SystemCxxToolchainInfo")

def _get_current_os() -> str:
    os = host_info().os
    if os.is_macos:
        return "macos"
    elif os.is_windows:
        return "windows"
    else:
        return "linux"

def _path_clang_tools_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        SystemCxxToolchainInfo(
            compiler = "clang",
            compiler_type = "clang",
            cxx_compiler = "clang++",
            asm_compiler = "clang",
            asm_compiler_type = "clang",
            rc_compiler = None,
            cvtres_compiler = None,
            archiver = "ar",
            archiver_type = "gnu",
            linker = "clang++",
            linker_type = "gnu",
            os = ctx.attrs.os
        ),
    ]

path_clang_tools = rule(
    impl = _path_clang_tools_impl,
    attrs = {
        "os": attrs.string(default = _get_current_os()),
    },
)
