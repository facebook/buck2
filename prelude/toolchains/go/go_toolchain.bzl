# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//go:toolchain.bzl", "GoDistrInfo", "GoToolchainInfo")

def _go_toolchain_impl(ctx):
    # Note: It makes sense to make GoDirstrInfo an attribute of GoToolchainInfo.
    # That's a breaking change, so we'll need to notify oss users.
    go_distr = ctx.attrs.go_distr[GoDistrInfo]
    return [
        DefaultInfo(
            sub_targets = {"go": [
                RunInfo(cmd_args(
                    ctx.attrs.go_wrapper[RunInfo],
                    ["--go", go_distr.bin_go],
                    ["--goroot", go_distr.go_root],
                    "--",
                )),
            ]},
        ),
        GoToolchainInfo(
            assembler = go_distr.tool_asm,
            assembler_flags = ctx.attrs.assembler_flags,
            cxx_compiler_flags = ctx.attrs.cxx_compiler_flags,
            cgo = go_distr.tool_cgo,
            compiler = go_distr.tool_compile,
            compiler_flags = ctx.attrs.compiler_flags,
            concat_files = ctx.attrs.concat_files[RunInfo],
            external_linker_flags = ctx.attrs.external_linker_flags,
            gen_stdlib_importcfg = ctx.attrs.gen_stdlib_importcfg[RunInfo],
            go_wrapper = ctx.attrs.go_wrapper[RunInfo],
            cover = go_distr.tool_cover,
            go = go_distr.bin_go,
            env_go_arch = ctx.attrs.env_go_arch,
            env_go_os = ctx.attrs.env_go_os,
            env_go_arm = ctx.attrs.env_go_arm,
            env_go_root = go_distr.go_root,
            env_go_debug = ctx.attrs.env_go_debug,
            linker = go_distr.tool_link,
            linker_flags = ctx.attrs.linker_flags,
            packer = ctx.attrs.tool_pack[RunInfo],
            build_tags = ctx.attrs.build_tags,
        ),
    ]

go_toolchain = rule(
    impl = _go_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "assembler_flags": attrs.list(attrs.arg(), default = []),
        "build_tags": attrs.list(attrs.string(), default = []),
        "compiler_flags": attrs.list(attrs.arg(), default = []),
        "concat_files": attrs.exec_dep(providers = [RunInfo], default = "prelude//go_bootstrap/tools:go_concat_files"),
        "cxx_compiler_flags": attrs.list(attrs.arg(), default = []),
        "env_go_arch": attrs.string(),
        "env_go_arm": attrs.option(attrs.string(), default = None),
        "env_go_debug": attrs.dict(attrs.string(), attrs.string(), default = {}),
        "env_go_os": attrs.string(),
        "external_linker_flags": attrs.list(attrs.arg(), default = []),
        "gen_stdlib_importcfg": attrs.exec_dep(providers = [RunInfo], default = "prelude//go/tools:gen_stdlib_importcfg"),
        "go_distr": attrs.exec_dep(providers = [GoDistrInfo]),
        "go_wrapper": attrs.exec_dep(providers = [RunInfo], default = "prelude//go_bootstrap/tools:go_go_wrapper"),
        "linker_flags": attrs.list(attrs.arg(), default = []),
        "tool_pack": attrs.exec_dep(providers = [RunInfo], default = "prelude//go/tools:tool_pack"),
    },
)

def _go_distr_impl(ctx):
    go_root = ctx.attrs.go_root
    suffix = ".exe" if ctx.attrs.go_os == "windows" else ""
    tool_prefix = "pkg/tool/{}_{}".format(ctx.attrs.go_os, ctx.attrs.go_arch)
    return [
        DefaultInfo(),
        GoDistrInfo(
            bin_go = RunInfo(go_root.project("bin/go" + suffix)),
            go_root = go_root,
            tool_asm = RunInfo(go_root.project(tool_prefix + "/asm" + suffix)),
            tool_compile = RunInfo(go_root.project(tool_prefix + "/compile" + suffix)),
            tool_cover = RunInfo(go_root.project(tool_prefix + "/cover" + suffix)),
            tool_cgo = RunInfo(go_root.project(tool_prefix + "/cgo" + suffix)),
            tool_link = RunInfo(go_root.project(tool_prefix + "/link" + suffix)),
        ),
    ]

go_distr = rule(
    impl = _go_distr_impl,
    attrs = {
        "go_arch": attrs.string(),
        "go_os": attrs.string(),
        "go_root": attrs.source(allow_directory = True),
    },
)
