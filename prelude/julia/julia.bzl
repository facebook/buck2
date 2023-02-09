# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# -------------------------------------------------------- #
# Toolchain rules
# -------------------------------------------------------- #

JuliaToolchainInfo = provider(fields = [
    "julia",
    "env",
])

def _toolchain(lang: str.type, providers: [""]) -> "attribute":
    return attrs.default_only(attrs.toolchain_dep(default = "toolchains//:" + lang, providers = providers))

def get_toolchain_cmd_args(toolchain: "JuliaToolchainInfo") -> "cmd_args":
    cmd = cmd_args("/usr/bin/env")

    # set any relevant toolchain environment variables
    cmd.add(toolchain.env)
    cmd.add(toolchain.julia)

    return cmd

def _julia_toolchain():
    return _toolchain("julia", [JuliaToolchainInfo])

# -------------------------------------------------------- #
# Build rules
# -------------------------------------------------------- #

# run a command of the form:
# $ julia -flag_1 -flag_2 -- my_script.jl arg1 arg2
#
# https://docs.julialang.org/en/v1/manual/command-line-options/

def build_julia_command(ctx):
    # retrieve and add toolchain details
    julia_toolchain = ctx.attrs._julia_toolchain[JuliaToolchainInfo]

    cmd = get_toolchain_cmd_args(julia_toolchain)

    # add julia flags
    cmd.add(cmd_args(ctx.attrs.julia_flags))

    # The -- delimiter is used to separate command-line arguments intended
    # for the script file from arguments intended for Julia
    cmd.add("--")

    srcs_by_path = {f.short_path: f for f in ctx.attrs.srcs}
    srcs = ctx.actions.symlinked_dir("srcs_tree", srcs_by_path)

    if ctx.attrs.main not in srcs_by_path:
        fail("main should be in srcs!")

    # add the source file
    cmd.add(srcs.project(ctx.attrs.main))

    # add the command arguments
    cmd.add(cmd_args(ctx.attrs.julia_args))

    return cmd.hidden(srcs)

def julia_binary_impl(ctx: "context") -> ["provider"]:
    cmd = build_julia_command(ctx)
    return [DefaultInfo(), RunInfo(cmd)]

def julia_library_impl(_ctx: "context") -> ["provider"]:
    # TODO
    return [DefaultInfo()]

def julia_test_impl(_ctx: "context") -> ["provider"]:
    # TODO
    return [DefaultInfo()]

# -------------------------------------------------------- #
# Attributes
# -------------------------------------------------------- #

implemented_rules = {
    "julia_binary": julia_binary_impl,
    "julia_library": julia_library_impl,
    "julia_test": julia_test_impl,
}

extra_attributes = {
    "julia_binary": {
        "deps": attrs.list(attrs.dep(), default = []),
        "julia_args": attrs.list(attrs.string(), default = []),
        "julia_flags": attrs.list(attrs.string(), default = []),
        "main": attrs.string(),
        "srcs": attrs.list(attrs.source(), default = []),
        "_julia_toolchain": _julia_toolchain(),
    },
    "julia_library": {
        "deps": attrs.list(attrs.dep(), default = []),
        "_julia_toolchain": _julia_toolchain(),
    },
    "julia_test": {
        "deps": attrs.list(attrs.dep(), default = []),
        "julia_args": attrs.list(attrs.string(), default = []),
        "julia_flags": attrs.list(attrs.string(), default = []),
        "resources": attrs.list(attrs.source(allow_directory = True), default = []),
        "src": attrs.source(),
        "_julia_toolchain": _julia_toolchain(),
        # TODO: coverage
    },
}
