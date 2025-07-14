# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:utils.bzl", "dedupe_by_value")
load(":cgo_builder.bzl", "build_cgo")
load(":compile.bzl", "get_inherited_compile_pkgs", "infer_package_root")
load(
    ":coverage.bzl",
    "GoCoverageMode",  # @Unused used as type
    "cover_srcs",
)
load(":go_list.bzl", "go_list", "parse_go_list_out")
load(":packages.bzl", "GoPackageInfo", "GoPkg", "make_importcfg", "merge_pkgs")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

def build_package(
        ctx: AnalysisContext,
        pkg_name: str,
        main: bool,
        srcs: list[Artifact],
        package_root: str | None,
        pkgs: dict[str, GoPkg] = {},
        deps: list[Dependency] = [],
        compiler_flags: list[str] = [],
        assembler_flags: list[str] = [],
        build_tags: list[str] = [],
        race: bool = False,
        asan: bool = False,
        cgo_enabled: bool = False,
        coverage_mode: GoCoverageMode | None = None,
        embedcfg: Artifact | None = None,
        with_tests: bool = False,
        cgo_gen_dir_name: str = "cgo_gen") -> (GoPkg, GoPackageInfo):
    if race and coverage_mode not in [None, GoCoverageMode("atomic")]:
        fail("`coverage_mode` must be `atomic` when `race=True`")

    out_x = ctx.actions.declare_output(paths.basename(pkg_name) + "_non-shared.x")
    out_a = ctx.actions.declare_output(paths.basename(pkg_name) + "_non-shared.a")

    out_shared_x = ctx.actions.declare_output(paths.basename(pkg_name) + "_shared.x")
    out_shared_a = ctx.actions.declare_output(paths.basename(pkg_name) + "_shared.a")

    cgo_gen_dir = ctx.actions.declare_output(cgo_gen_dir_name, dir = True)

    srcs = dedupe_by_value(srcs)

    package_root = package_root if package_root != None else infer_package_root(srcs)

    go_list_out = go_list(ctx, pkg_name, srcs, package_root, build_tags, cgo_enabled, with_tests = with_tests, asan = asan)

    test_go_files_argsfile = ctx.actions.declare_output(paths.basename(pkg_name) + "_test_go_files.go_package_argsfile")
    coverage_vars_argsfile = ctx.actions.declare_output(paths.basename(pkg_name) + "_coverage_vars.go_package_argsfile")
    dynamic_outputs = [out_x, out_a, out_shared_x, out_shared_a, test_go_files_argsfile, coverage_vars_argsfile, cgo_gen_dir]

    all_pkgs = merge_pkgs([
        pkgs,
        get_inherited_compile_pkgs(deps),
    ])

    def f(ctx: AnalysisContext, artifacts, outputs, go_list_out = go_list_out):
        go_list = parse_go_list_out(srcs, package_root, artifacts[go_list_out])

        # A go package can can contain CGo or Go ASM files, but not both.
        # If CGo and ASM files are present, we process ASM files together with C files with CxxToolchain.
        # The `go build` command does additional check here and throws an error if both CGo and Go-ASM files are present.
        c_files = go_list.c_files + go_list.cxx_files
        s_files = go_list.s_files
        if len(go_list.cgo_files) > 0:
            c_files += s_files
            s_files = []

        # Generate CGO and C sources.
        cgo_go_files, cgo_o_files, cgo_gen_tmp_dir = build_cgo(ctx, go_list.cgo_files, go_list.h_files, c_files, go_list.cgo_cflags, go_list.cgo_cppflags, anon_targets_allowed = False)
        ctx.actions.copy_dir(outputs[cgo_gen_dir], cgo_gen_tmp_dir)

        is_x_test_pkg = len(go_list.x_test_go_files) > 0
        if is_x_test_pkg:
            fail("External tests are not supported, remove suffix '_test' from package declaration '{}': {}", go_list.name, ctx.label)

        ctx.actions.write(outputs[test_go_files_argsfile], cmd_args((go_list.test_go_files if with_tests else []), ""))

        go_list_pkg_name = go_list.name
        go_files_to_cover = go_list.go_files + cgo_go_files + (go_list.test_go_files if with_tests else [])
        covered_go_files, coverage_vars_out, coveragecfg = cover_srcs(ctx, go_list_pkg_name, pkg_name, go_files_to_cover, coverage_mode)
        ctx.actions.write(outputs[coverage_vars_argsfile], coverage_vars_out)

        symabis = _symabis(ctx, pkg_name, main, s_files, go_list.h_files, assembler_flags)

        # Use -complete flag when compiling Go code only
        complete_flag = len(go_list.cgo_files) + len(s_files) + len(c_files) == 0

        def build_variant(shared: bool) -> (Artifact, Artifact):
            suffix = "_shared" if shared else "_non-shared"  # suffix to make artifacts unique
            go_files_to_compile = covered_go_files
            importcfg = make_importcfg(ctx, pkg_name, all_pkgs, shared, link = False)
            go_x_file, go_a_file, asmhdr = _compile(
                ctx = ctx,
                pkg_name = pkg_name,
                main = main,
                go_srcs = go_files_to_compile,
                importcfg = importcfg,
                compiler_flags = compiler_flags,
                shared = shared,
                race = race,
                asan = asan,
                suffix = suffix,
                complete = complete_flag,
                coveragecfg = coveragecfg,
                embedcfg = embedcfg,
                embed_files = go_list.embed_files,
                symabis = symabis,
                gen_asmhdr = len(s_files) > 0,
            )

            asm_o_files = _asssembly(ctx, pkg_name, main, s_files, go_list.h_files, asmhdr, assembler_flags, shared, suffix)

            return go_x_file, _pack(ctx, pkg_name, go_a_file, cgo_o_files + asm_o_files, suffix)

        non_shared_x, non_shared_a = build_variant(shared = False)
        ctx.actions.copy_file(outputs[out_x], non_shared_x)
        ctx.actions.copy_file(outputs[out_a], non_shared_a)

        shared_x, shared_a = build_variant(shared = True)
        ctx.actions.copy_file(outputs[out_shared_x], shared_x)
        ctx.actions.copy_file(outputs[out_shared_a], shared_a)

    ctx.actions.dynamic_output(dynamic = [go_list_out], inputs = [], outputs = [o.as_output() for o in dynamic_outputs], f = f)

    return GoPkg(
        pkg = out_a,
        pkg_shared = out_shared_a,
        export_file = out_x,
        export_file_shared = out_shared_x,
        coverage_vars = cmd_args(coverage_vars_argsfile, format = "@{}"),
        test_go_files = cmd_args(test_go_files_argsfile, format = "@{}", hidden = srcs),
    ), GoPackageInfo(
        build_out = out_x,
        cgo_gen_dir = cgo_gen_dir,
        package_name = pkg_name,
        package_root = package_root,
        go_list_out = go_list_out,
        srcs = srcs,
    )

def _compile(
        ctx: AnalysisContext,
        pkg_name: str,
        main: bool,
        go_srcs: list[Artifact],
        importcfg: cmd_args,
        compiler_flags: list[str],
        shared: bool,
        race: bool,
        asan: bool,
        suffix: str,
        complete: bool,
        coveragecfg: Artifact | None = None,
        embedcfg: Artifact | None = None,
        embed_files: list[Artifact] = [],
        symabis: Artifact | None = None,
        gen_asmhdr: bool = False) -> (Artifact, Artifact, Artifact | None):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    env = get_toolchain_env_vars(go_toolchain)
    out_a = ctx.actions.declare_output("go_compile_out{}.a".format(suffix))
    out_x = ctx.actions.declare_output("go_compile_out{}.x".format(suffix))

    if len(go_srcs) == 0:
        ctx.actions.write(out_a.as_output(), "")
        ctx.actions.write(out_x.as_output(), "")
        return out_x, out_a, None

    asmhdr = ctx.actions.declare_output("__asmhdr__{}/go_asm.h".format(suffix)) if gen_asmhdr else None

    # Use argsfile to avoid command length limit on Windows
    srcs_argsfile = ctx.actions.write(paths.basename(pkg_name) + suffix + "_srcs.go_package_argsfile", go_srcs)

    compile_cmd = cmd_args(
        [
            go_toolchain.go_wrapper,
            ["--go", go_toolchain.compiler],
            "--",
            go_toolchain.compiler_flags,
            compiler_flags,
            "-buildid=",
            "-nolocalimports",
            "-pack",
            ["-trimpath", "%cwd%"],
            ["-p", "main" if main else pkg_name],
            ["-importcfg", importcfg],
            ["-o", out_x.as_output()],
            ["-linkobj", out_a.as_output()],
            ["-race"] if race else [],
            ["-asan"] if asan else [],
            ["-shared"] if shared else [],
            ["-coveragecfg", coveragecfg] if coveragecfg else [],
            ["-embedcfg", embedcfg] if embedcfg else [],
            ["-symabis", symabis] if symabis else [],
            ["-asmhdr", asmhdr.as_output()] if asmhdr else [],
            ["-complete"] if complete else [],
            cmd_args(srcs_argsfile, format = "@{}", hidden = go_srcs),
        ],
        hidden = embed_files,  #  files and directories should be available for embedding
    )

    identifier = paths.basename(pkg_name)
    ctx.actions.run(compile_cmd, env = env, category = "go_compile", identifier = identifier + suffix)

    return (out_x, out_a, asmhdr)

def _symabis(
        ctx: AnalysisContext,
        pkg_name: str,
        main: bool,
        s_files: list[Artifact],
        h_files: list[Artifact],
        assembler_flags: list[str]) -> Artifact | None:
    if len(s_files) == 0:
        return None

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    # we have to supply "go_asm.h" with any content to make asm tool happy
    # its content doesn't matter if -gensymabis provided
    # https://github.com/golang/go/blob/3f8f929d60a90c4e4e2b07c8d1972166c1a783b1/src/cmd/go/internal/work/gc.go#L441-L443
    fake_asmhdr = ctx.actions.write("__fake_asmhdr__/go_asm.h", "")
    symabis = ctx.actions.declare_output("symabis")
    asm_cmd = [
        go_toolchain.go_wrapper,
        ["--go", go_toolchain.assembler],
        "--",
        go_toolchain.assembler_flags,
        assembler_flags,
        _asm_args(ctx, pkg_name, main, False),  # flag -shared doesn't matter for symabis
        "-gensymabis",
        ["-o", symabis.as_output()],
        ["-I", cmd_args(fake_asmhdr, parent = 1)],
        [cmd_args(h_files, parent = 1, prepend = "-I")] if h_files else [],
        ["-trimpath", "%cwd%"],
        s_files,
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(asm_cmd, env = env, category = "go_symabis", identifier = identifier)

    return symabis

def _asssembly(
        ctx: AnalysisContext,
        pkg_name: str,
        main: bool,
        s_files: list[Artifact],
        h_files: list[Artifact],
        asmhdr: Artifact | None,
        assembler_flags: list[str],
        shared: bool,
        suffix: str) -> list[Artifact]:
    if len(s_files) == 0:
        return []

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    o_files = []
    identifier = paths.basename(pkg_name)
    for s_file in s_files:
        o_file = ctx.actions.declare_output(s_file.short_path + suffix + ".o")
        o_files.append(o_file)

        asm_cmd = [
            go_toolchain.go_wrapper,
            ["--go", go_toolchain.assembler],
            "--",
            go_toolchain.assembler_flags,
            assembler_flags,
            _asm_args(ctx, pkg_name, main, shared),
            ["-o", o_file.as_output()],
            ["-I", cmd_args(asmhdr, parent = 1)] if asmhdr else [],  # can it actually be None?
            [cmd_args(h_files, parent = 1, prepend = "-I")] if h_files else [],
            ["-trimpath", "%cwd%"],
            s_file,
        ]

        ctx.actions.run(asm_cmd, env = env, category = "go_assembly", identifier = identifier + "/" + s_file.short_path + suffix)

    return o_files

def _pack(ctx: AnalysisContext, pkg_name: str, a_file: Artifact, o_files: list[Artifact], suffix: str) -> Artifact:
    if len(o_files) == 0:
        # no need to repack .a file, if there are no .o files
        return a_file

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    pkg_file = ctx.actions.declare_output("pkg{}.a".format(suffix))

    pack_cmd = [
        go_toolchain.packer,
        "c",
        pkg_file.as_output(),
        a_file,
        o_files,
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(pack_cmd, env = env, category = "go_pack", identifier = identifier + suffix)

    return pkg_file

def _asm_args(ctx: AnalysisContext, pkg_name: str, main: bool, shared: bool):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    return [
        ["-p", "main" if main else pkg_name],
        ["-I", go_toolchain.env_go_root.project("pkg/include")] if go_toolchain.env_go_root else [],
        ["-D", "GOOS_" + go_toolchain.env_go_os] if go_toolchain.env_go_os else [],
        ["-D", "GOARCH_" + go_toolchain.env_go_arch] if go_toolchain.env_go_arch else [],
        ["-shared"] if shared else [],
    ]
