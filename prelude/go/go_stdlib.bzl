# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":packages.bzl", "GoStdlib")
load(":toolchain.bzl", "GoToolchainInfo", "evaluate_cgo_enabled", "get_toolchain_env_vars")

def go_stdlib_impl(ctx: AnalysisContext) -> list[Provider]:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    stdlib_pkgdir = ctx.actions.declare_output("stdlib_pkgdir", dir = True)
    cgo_enabled = evaluate_cgo_enabled(go_toolchain, ctx.attrs._cgo_enabled)
    tags = [] + go_toolchain.tags
    linker_flags = [] + go_toolchain.linker_flags
    assembler_flags = [] + go_toolchain.assembler_flags
    compiler_flags = [] + go_toolchain.compiler_flags
    compiler_flags += ["-buildid="]  # Make builds reproducible.
    if ctx.attrs._compile_shared:
        assembler_flags += ["-shared"]
        compiler_flags += ["-shared"]

    if ctx.attrs._asan:
        compiler_flags += ["-asan"]
        tags += ["asan"]

    env = get_toolchain_env_vars(go_toolchain)
    env["GODEBUG"] = "installgoroot=all"
    env["CGO_ENABLED"] = "1" if cgo_enabled else "0"

    cxx_toolchain = go_toolchain.cxx_toolchain_for_linking
    if cxx_toolchain != None:
        c_compiler = cxx_toolchain.c_compiler_info

        env["CC"] = cmd_args(c_compiler.compiler, delimiter = " ", quote = "shell", absolute_prefix = "%cwd%/")
        env["CGO_CFLAGS"] = cmd_args(c_compiler.compiler_flags, delimiter = " ", quote = "shell", absolute_prefix = "%cwd%/")
        env["CGO_CPPFLAGS"] = cmd_args(c_compiler.preprocessor_flags, delimiter = " ", quote = "shell", absolute_prefix = "%cwd%/")

    cmd = cmd_args([
        go_toolchain.go_wrapper,
        go_toolchain.go,
        "install",
        "-pkgdir",
        stdlib_pkgdir.as_output(),
        cmd_args(["-asmflags=", cmd_args(assembler_flags, delimiter = " ")], delimiter = "") if assembler_flags else [],
        cmd_args(["-gcflags=", cmd_args(compiler_flags, delimiter = " ")], delimiter = "") if compiler_flags else [],
        cmd_args(["-ldflags=", cmd_args(linker_flags, delimiter = " ")], delimiter = "") if linker_flags else [],
        ["-tags", ",".join(tags)] if tags else [],
        ["-race"] if ctx.attrs._race else [],
        "std",
    ])

    ctx.actions.run(cmd, env = env, category = "go_build_stdlib", identifier = "go_build_stdlib")

    importcfg = ctx.actions.declare_output("stdlib.importcfg")
    ctx.actions.run(
        [
            go_toolchain.gen_stdlib_importcfg,
            "--stdlib",
            stdlib_pkgdir,
            "--output",
            importcfg.as_output(),
        ],
        category = "go_gen_stdlib_importcfg",
        identifier = "go_gen_stdlib_importcfg",
    )

    return [
        DefaultInfo(default_output = stdlib_pkgdir),
        GoStdlib(pkgdir = stdlib_pkgdir, importcfg = importcfg),
    ]
