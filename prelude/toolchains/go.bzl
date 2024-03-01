# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//go:toolchain.bzl", "GoToolchainInfo")
load("@prelude//utils:cmd_script.bzl", "ScriptOs", "cmd_script")

def _system_go_toolchain_impl(ctx):
    arch = host_info().arch
    if arch.is_aarch64:
        go_arch = "arm64"
    elif arch.is_x86_64:
        go_arch = "amd64"
    else:
        fail("Unsupported go arch: {}".format(arch))

    os = host_info().os
    if os.is_macos:
        go_os = "darwin"
    elif os.is_linux:
        go_os = "linux"
    elif os.is_windows:
        go_os = "windows"
    else:
        fail("Unsupported go os: {}".format(os))

    script_os = ScriptOs("windows" if os.is_windows else "unix")
    go = "go.exe" if os.is_windows else "go"

    return [
        DefaultInfo(),
        GoToolchainInfo(
            assembler = RunInfo(cmd_script(ctx, "asm", cmd_args(go, "tool", "asm"), script_os)),
            cgo = RunInfo(cmd_script(ctx, "cgo", cmd_args(go, "tool", "cgo"), script_os)),
            cgo_wrapper = ctx.attrs.cgo_wrapper[RunInfo],
            compile_wrapper = ctx.attrs.compile_wrapper[RunInfo],
            concat_files = ctx.attrs.concat_files[RunInfo],
            compiler = RunInfo(cmd_script(ctx, "compile", cmd_args(go, "tool", "compile"), script_os)),
            cover = RunInfo(cmd_script(ctx, "cover", cmd_args(go, "tool", "cover"), script_os)),
            cover_srcs = ctx.attrs.cover_srcs[RunInfo],
            cxx_toolchain_for_linking = None,
            env_go_arch = go_arch,
            env_go_os = go_os,
            external_linker_flags = [],
            filter_srcs = ctx.attrs.filter_srcs[RunInfo],
            gen_stdlib_importcfg = ctx.attrs.gen_stdlib_importcfg[RunInfo],
            go = RunInfo(cmd_script(ctx, "go", cmd_args(go), script_os)),
            go_wrapper = ctx.attrs.go_wrapper[RunInfo],
            linker = RunInfo(cmd_script(ctx, "link", cmd_args(go, "tool", "link"), script_os)),
            packer = RunInfo(cmd_script(ctx, "pack", cmd_args(go, "tool", "pack"), script_os)),
            tags = [],
            linker_flags = [],
            assembler_flags = [],
            compiler_flags = [],
        ),
    ]

system_go_toolchain = rule(
    impl = _system_go_toolchain_impl,
    doc = """Example system go toolchain rules (WIP). Usage:
  system_go_toolchain(
      name = "go",
      visibility = ["PUBLIC"],
  )""",
    attrs = {
        "cgo_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:cgo_wrapper")),
        "compile_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:compile_wrapper")),
        "concat_files": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:concat_files")),
        "cover_srcs": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:cover_srcs")),
        "filter_srcs": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:filter_srcs")),
        "gen_stdlib_importcfg": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:gen_stdlib_importcfg")),
        "go_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:go_wrapper")),
    },
    is_toolchain_rule = True,
)
