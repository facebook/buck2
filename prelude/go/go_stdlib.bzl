# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:target_sdk_version.bzl", "get_target_sdk_version_flags")
load(":packages.bzl", "GoStdlib", "GoStdlibDynamicValue", "StdPkg")
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

    if go_toolchain.asan:
        compiler_flags += ["-asan"]
        build_tags += ["asan"]

    if go_toolchain.fuzz:
        # Note this will cover all packages including skipped https://fburl.com/etm81gfr
        compiler_flags += ["-d=libfuzzer"]
        # I've discovered a weird thing, runtime/libfuzzer* files are never used by `go build`
        # and we get "relocation target ... not defined error if include them",
        # so for now I assume this is broken.
        # build_tags += ["libfuzzer"]

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
        cflags.add(cmd_args(go_toolchain.cxx_compiler_flags, delimiter = "\t"))
        env["CC"] = cmd_args(c_compiler.compiler, delimiter = "\t", absolute_prefix = "%cwd%/")
        env["CGO_CFLAGS"] = cflags
        env["CGO_CPPFLAGS"] = cmd_args(c_compiler.preprocessor_flags, delimiter = "\t", absolute_prefix = "%cwd%/")

    stdlib_pkgdir = ctx.actions.declare_output("stdlib_pkgdir", dir = True, has_content_based_path = True)
    stdlib_pkgdir_shared = ctx.actions.declare_output("stdlib_pkgdir_shared", dir = True, has_content_based_path = True)

    def build_variant(out: Artifact, shared: bool) -> cmd_args:
        local_assembler_flags = [] + assembler_flags
        local_compiler_flags = [] + compiler_flags
        if shared:
            local_assembler_flags += ["-shared"]
            local_compiler_flags += ["-shared"]
        return cmd_args([
            go_toolchain.go_wrapper,
            ["--go", go_toolchain.go],
            "--use-tmp-workdir",  # cd to a tmp dir to avoid interference with procject's go.mod
            "install",
            cmd_args(out.as_output(), format = "-pkgdir=%cwd%/{}"),
            cmd_args(["-asmflags=", cmd_args(local_assembler_flags, delimiter = " ")], delimiter = "") if local_assembler_flags else [],
            cmd_args(["-gcflags=", cmd_args(local_compiler_flags, delimiter = " ")], delimiter = "") if local_compiler_flags else [],
            cmd_args(["-ldflags=", cmd_args(linker_flags, delimiter = " ")], delimiter = "") if linker_flags else [],
            ["-tags", ",".join(build_tags)] if build_tags else [],
            ["-race"] if go_toolchain.race else [],
            "std",
        ])

    ctx.actions.run(build_variant(stdlib_pkgdir, False), env = env, category = "go_build_stdlib", identifier = "go_build_stdlib")
    ctx.actions.run(build_variant(stdlib_pkgdir_shared, True), env = env, category = "go_build_stdlib", identifier = "go_build_stdlib_shared")

    go_list_stdlib_out = ctx.actions.declare_output("go_list_stdlib.txt", has_content_based_path = True)

    ctx.actions.run(
        [
            go_toolchain.go_wrapper,
            ["--go", go_toolchain.go],
            ["--output", go_list_stdlib_out.as_output()],
            "--convert-json-stream",
            "list",
            "-json",
            ["-tags", ",".join(build_tags)] if build_tags else [],
            ["-race"] if go_toolchain.race else [],
            "std",
        ],
        env = env,
        category = "go_list_stdlib",
        identifier = "go_list_stdlib",
    )

    go_stdlib_value = ctx.actions.dynamic_output_new(_produce_dynamic_value(
        go_list_stdlib_out = go_list_stdlib_out,
        pkgdir = stdlib_pkgdir,
        pkgdir_shared = stdlib_pkgdir_shared,
    ))

    return [
        DefaultInfo(default_output = stdlib_pkgdir),
        GoStdlib(dynamic_value = go_stdlib_value),
    ]

def _produce_dynamic_value_impl(actions: AnalysisActions, go_list_stdlib_out: ArtifactValue, pkgdir: Artifact, pkgdir_shared: Artifact) -> list[Provider]:
    _ignore = (actions,)  # buildifier: disable=unused-variable

    pkgs = {}
    for lib in go_list_stdlib_out.read_json():
        if "GoFiles" not in lib and "CgoFiles" not in lib:
            continue  # skip test packages

        import_path = lib["ImportPath"]
        if import_path in ["unsafe", "builtin"]:
            continue  # skip fake packages

        pkgs[import_path] = StdPkg(
            a_file = pkgdir.project(import_path + ".a"),
            a_file_shared = pkgdir_shared.project(import_path + ".a"),
        )

    return [
        GoStdlibDynamicValue(pkgs = pkgs),
    ]

_produce_dynamic_value = dynamic_actions(
    impl = _produce_dynamic_value_impl,
    attrs = {
        "go_list_stdlib_out": dynattrs.artifact_value(),
        "pkgdir": dynattrs.value(Artifact),
        "pkgdir_shared": dynattrs.value(Artifact),
    },
)
