# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version_flags")
load(":packages.bzl", "GoStdlib")
load(":toolchain.bzl", "GoToolchainInfo", "evaluate_cgo_enabled", "get_toolchain_env_vars")

def go_stdlib_impl(ctx: AnalysisContext) -> list[Provider]:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    cgo_enabled = evaluate_cgo_enabled(cxx_toolchain_available, cxx_toolchain_available, ctx.attrs._cgo_enabled)
    build_tags = [] + go_toolchain.build_tags
    linker_flags = [] + go_toolchain.linker_flags
    assembler_flags = [] + go_toolchain.assembler_flags
    compiler_flags = [] + go_toolchain.compiler_flags
    compiler_flags += ["-buildid="]  # Make builds reproducible.

    if ctx.attrs._asan:
        compiler_flags += ["-asan"]
        build_tags += ["asan"]

    env = get_toolchain_env_vars(go_toolchain)
    env["GODEBUG"] = "installgoroot=all"
    env["CGO_ENABLED"] = "1" if cgo_enabled else "0"

    if go_toolchain.env_go_root != None:
        env["GOROOT"] = go_toolchain.env_go_root

    if cgo_enabled:
        cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]
        c_compiler = cxx_toolchain.c_compiler_info
        cflags = cmd_args(c_compiler.compiler_flags, delimiter = "\t", absolute_prefix = "%cwd%/")
        cflags.add(cmd_args(get_target_sdk_version_flags(ctx), delimiter = "\t"))
        env["CC"] = cmd_args(c_compiler.compiler, delimiter = "\t", absolute_prefix = "%cwd%/")
        env["CGO_CFLAGS"] = cflags
        env["CGO_CPPFLAGS"] = cmd_args(c_compiler.preprocessor_flags, delimiter = "\t", absolute_prefix = "%cwd%/")

    importcfg = ctx.actions.declare_output("stdlib.importcfg")
    importcfg_shared = ctx.actions.declare_output("stdlib_shared.importcfg")
    stdlib_pkgdir = ctx.actions.declare_output("stdlib_pkgdir", dir = True)
    stdlib_pkgdir_shared = ctx.actions.declare_output("stdlib_pkgdir_shared", dir = True)

    def build_variant(out: Artifact, shared: bool) -> cmd_args:
        local_assembler_flags = [] + assembler_flags
        local_compiler_flags = [] + compiler_flags
        if shared:
            local_assembler_flags += ["-shared"]
            local_compiler_flags += ["-shared"]
        return cmd_args([
            go_toolchain.go_wrapper,
            ["--go", go_toolchain.go],
            "install",
            "-pkgdir",
            out.as_output(),
            cmd_args(["-asmflags=", cmd_args(local_assembler_flags, delimiter = " ")], delimiter = "") if local_assembler_flags else [],
            cmd_args(["-gcflags=", cmd_args(local_compiler_flags, delimiter = " ")], delimiter = "") if local_compiler_flags else [],
            cmd_args(["-ldflags=", cmd_args(linker_flags, delimiter = " ")], delimiter = "") if linker_flags else [],
            ["-tags", ",".join(build_tags)] if build_tags else [],
            ["-race"] if ctx.attrs._race else [],
            "std",
        ])

    ctx.actions.run(build_variant(stdlib_pkgdir, False), env = env, category = "go_build_stdlib", identifier = "go_build_stdlib")
    ctx.actions.run(build_variant(stdlib_pkgdir_shared, True), env = env, category = "go_build_stdlib", identifier = "go_build_stdlib_shared")

    ctx.actions.run(
        [
            go_toolchain.gen_stdlib_importcfg,
            ["--stdlib", stdlib_pkgdir],
            ["--output", importcfg.as_output()],
        ],
        category = "go_gen_stdlib_importcfg",
        identifier = "go_gen_stdlib_importcfg",
    )

    ctx.actions.run(
        [
            go_toolchain.gen_stdlib_importcfg,
            ["--stdlib", stdlib_pkgdir_shared],
            ["--output", importcfg_shared.as_output()],
        ],
        category = "go_gen_stdlib_importcfg",
        identifier = "go_gen_stdlib_importcfg_shared",
    )

    return [
        DefaultInfo(default_output = stdlib_pkgdir),
        GoStdlib(pkgdir = stdlib_pkgdir, importcfg = importcfg, pkgdir_shared = stdlib_pkgdir_shared, importcfg_shared = importcfg_shared),
    ]
