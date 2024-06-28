# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//decls/common.bzl", "buck")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//toolchains:cxx.bzl", "CxxToolsInfo")

def _path_clang_tools_impl(ctx: AnalysisContext) -> list[Provider]:
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
            archiver = "ar",
            archiver_type = "gnu",
            linker = "clang++",
            linker_type = "gnu",
            os = ctx.attrs._target_os_type[OsLookup].platform,
        ),
    ]

path_clang_tools = rule(
    impl = _path_clang_tools_impl,
    attrs = {
        "_target_os_type": buck.target_os_type_arg(),
    }
)
