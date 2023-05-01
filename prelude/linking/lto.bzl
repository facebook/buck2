# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")

# Styles of LTO.
LtoMode = enum(
    # No LTO
    "none",
    # Object files contain both LTO IR and native code to allow binaries to link
    # either via standard or LTO.
    "fat",
    # Traditional, monolithic LTO.
    "monolithic",
    # https://clang.llvm.org/docs/ThinLTO.html
    "thin",
)

def lto_compiler_flags(lto_mode: "LtoMode") -> [str.type]:
    if lto_mode == LtoMode("none"):
        return []
    elif lto_mode == LtoMode("fat") or lto_mode == LtoMode("monolithic"):
        return ["-flto=full"]
    elif lto_mode == LtoMode("thin"):
        return ["-flto=thin"]
    else:
        fail("Unhandled LTO mode: " + lto_mode)

def darwin_lto_linker_flags(ctx: "context") -> (["_arglike"], ["artifact"]):
    linker_info = get_cxx_toolchain_info(ctx).linker_info
    if linker_info.type != "darwin" or linker_info.lto_mode == LtoMode("none"):
        return [], []

    # https://releases.llvm.org/14.0.0/tools/clang/docs/CommandGuide/clang.html#cmdoption-flto
    # We need to pass -object_path_lto to keep the temporary LTO object files around to use
    # for dSYM generation.
    if linker_info.lto_mode == LtoMode("thin"):
        # For thin LTO the path is a folder that will contain the various object files.
        lto_object_path_artifact = ctx.actions.declare_output("lto_object_files", dir = True)
    else:
        # For monolithic LTO the path is a single object file.
        lto_object_path_artifact = ctx.actions.declare_output("lto_object_file.o", dir = False)

    linker_args = cmd_args([
        # Use -Xlinker in case the path has a ,
        "-Xlinker",
        "-object_path_lto",
        "-Xlinker",
        lto_object_path_artifact.as_output(),
    ])

    return [linker_args], [lto_object_path_artifact]
