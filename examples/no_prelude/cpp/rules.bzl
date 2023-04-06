# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@toolchains//:cpp_toolchain.bzl", "CxxCompilerInfo")

CxxLibraryInfo = provider(fields = ["headers", "objects", "include_folders"])

def _cpp_binary_impl(ctx: "context") -> ["provider"]:
    sources = ctx.attrs.srcs
    out = ctx.actions.declare_output("main")

    cmd = cmd_args([ctx.attrs.toolchain[CxxCompilerInfo].compiler_path, "-o", out.as_output()] + sources).hidden(ctx.attrs.headers)

    ctx.actions.run(cmd, category = "compile")

    return [
        DefaultInfo(default_output = out),
        RunInfo(args = cmd_args(out)),
    ]

cpp_binary = rule(
    impl = _cpp_binary_impl,
    attrs = {
        "deps": attrs.list(attrs.dep()),
        "headers": attrs.list(attrs.source()),
        "srcs": attrs.list(attrs.source()),
        "toolchain": attrs.toolchain_dep(),
    },
)

def _cpp_library_impl(ctx: "context") -> ["provider"]:
    sources = ctx.attrs.srcs
    headers = ctx.attrs.headers
    out = ctx.actions.declare_output("lib.so")

    cmd = cmd_args([ctx.attrs.toolchain[CxxCompilerInfo].compiler_path, "-shared", "-undefined", "dynamic_lookup", "-o", out.as_output()] + sources).hidden(ctx.attrs.headers)

    ctx.actions.run(cmd, category = "compile")

    return [DefaultInfo(default_output = out), CxxLibraryInfo(objects = [out], headers = headers)]

cpp_library = rule(
    impl = _cpp_library_impl,
    attrs = {
        "deps": attrs.list(attrs.dep()),
        "headers": attrs.list(attrs.source()),
        "srcs": attrs.list(attrs.source()),
        "toolchain": attrs.toolchain_dep(),
    },
)
