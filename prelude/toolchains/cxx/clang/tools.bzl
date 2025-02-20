# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "LinkerType")
load("@prelude//toolchains:cxx.bzl", "CxxToolsInfo")

def _path_clang_tools_impl(_ctx) -> list[Provider]:
    if host_info().os.is_windows:
        archiver = "llvm-ar"
    else:
        archiver = "ar"
    return [
        DefaultInfo(),
        CxxToolsInfo(
            compiler = "clang",
            compiler_type = "clang",
            cxx_compiler = "clang++",
            asm_compiler = "clang",
            asm_compiler_type = "clang",
            rc_compiler = None,
            cvtres_compiler = None,
            archiver = archiver,
            archiver_type = "gnu",
            linker = "clang++",
            linker_type = LinkerType("gnu"),
        ),
    ]

path_clang_tools = rule(
    impl = _path_clang_tools_impl,
    attrs = {},
)
