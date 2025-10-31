# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//go:toolchain.bzl", "GoToolchainInfo")
load("@prelude//os_lookup:defs.bzl", "ScriptLanguage")
load("@prelude//utils:cmd_script.bzl", "cmd_script")

def go_platform() -> (str, str):
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

    return go_os, go_arch

def _system_go_toolchain_impl(ctx):
    go_os, go_arch = go_platform()

    script_language = ScriptLanguage("bat" if go_os == "windows" else "sh")
    go = "go.exe" if go_os == "windows" else "go"

    return [
        DefaultInfo(sub_targets = {"go": [
            RunInfo(go),
        ]}),
        GoToolchainInfo(
            assembler = RunInfo(cmd_script(ctx.actions, "asm", cmd_args(go, "tool", "asm"), script_language)),
            cgo = RunInfo(cmd_script(ctx.actions, "cgo", cmd_args(go, "tool", "cgo"), script_language)),
            concat_files = ctx.attrs.concat_files[RunInfo],
            compiler = RunInfo(cmd_script(ctx.actions, "compile", cmd_args(go, "tool", "compile"), script_language)),
            cover = RunInfo(cmd_script(ctx.actions, "cover", cmd_args(go, "tool", "cover"), script_language)),
            env_go_arch = go_arch,
            env_go_os = go_os,
            external_linker_flags = [],
            gen_stdlib_importcfg = ctx.attrs.gen_stdlib_importcfg[RunInfo],
            go = RunInfo(cmd_script(ctx.actions, "go", cmd_args(go), script_language)),
            go_wrapper = ctx.attrs.go_wrapper[RunInfo],
            linker = RunInfo(cmd_script(ctx.actions, "link", cmd_args(go, "tool", "link"), script_language)),
            packer = RunInfo(cmd_script(ctx.actions, "pack", cmd_args(go, "tool", "pack"), script_language)),
            build_tags = [],
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
        "concat_files": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go_bootstrap/tools:go_concat_files")),
        "gen_stdlib_importcfg": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:gen_stdlib_importcfg")),
        "go_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go_bootstrap/tools:go_go_wrapper")),
    },
    is_toolchain_rule = True,
)
