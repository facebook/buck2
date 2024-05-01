# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:utils.bzl", "dedupe_by_value")
load(":cgo_builder.bzl", "build_cgo")
load(":compile.bzl", "get_inherited_compile_pkgs", "infer_package_root")
load(
    ":coverage.bzl",
    "GoCoverageMode",  # @Unused used as type
)
load(":go_list.bzl", "go_list", "parse_go_list_out")
load(":packages.bzl", "GoPkg", "make_importcfg", "merge_pkgs", "pkg_artifacts")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

def build_package(
        ctx: AnalysisContext,
        pkg_name: str,
        srcs: list[Artifact],
        package_root: str | None,
        pkgs: dict[str, Artifact] = {},
        deps: list[Dependency] = [],
        compiler_flags: list[str] = [],
        assembler_flags: list[str] = [],
        shared: bool = False,
        race: bool = False,
        asan: bool = False,
        coverage_mode: GoCoverageMode | None = None,
        embedcfg: Artifact | None = None,
        tests: bool = False,
        force_disable_cgo: bool = False,
        # If you change this dir or naming convention, please
        # update the corresponding logic in `fbgolist`.
        # Otherwise editing and linting for Go will break.
        cgo_gen_dir_name: str = "cgo_gen") -> GoPkg:
    if race and coverage_mode not in [None, GoCoverageMode("atomic")]:
        fail("`coverage_mode` must be `atomic` when `race=True`")

    out = ctx.actions.declare_output(paths.basename(pkg_name) + ".a")

    cgo_gen_dir = ctx.actions.declare_output(cgo_gen_dir_name, dir = True)

    srcs = dedupe_by_value(srcs)

    has_go_files = False
    for src in srcs:
        if src.extension == ".go":
            has_go_files = True
            break

    if not has_go_files:
        return GoPkg(
            pkg = ctx.actions.write(out.as_output(), ""),
            coverage_vars = cmd_args(),
            srcs_list = cmd_args(),
            cgo_gen_dir = ctx.actions.copied_dir(cgo_gen_dir.as_output(), {}),
        )

    package_root = package_root if package_root != None else infer_package_root(srcs)

    go_list_out = go_list(ctx, pkg_name, srcs, package_root, force_disable_cgo, with_tests = tests, asan = asan)

    srcs_list_argsfile = ctx.actions.declare_output(paths.basename(pkg_name) + "_srcs_list.argsfile")
    coverage_vars_argsfile = ctx.actions.declare_output(paths.basename(pkg_name) + "_coverage_vars.argsfile")
    dynamic_outputs = [out, srcs_list_argsfile, coverage_vars_argsfile, cgo_gen_dir]

    all_pkgs = merge_pkgs([
        pkgs,
        pkg_artifacts(get_inherited_compile_pkgs(deps)),
    ])
    importcfg = make_importcfg(ctx, pkg_name, all_pkgs, with_importmap = True)

    def f(ctx: AnalysisContext, artifacts, outputs, go_list_out = go_list_out):
        go_list = parse_go_list_out(srcs, package_root, artifacts[go_list_out])

        symabis = _symabis(ctx, pkg_name, go_list.s_files, assembler_flags, shared)

        # Generate CGO and C sources.
        cgo_go_files, cgo_o_files, cgo_gen_tmp_dir = build_cgo(ctx, go_list.cgo_files, go_list.h_files, go_list.c_files + go_list.cxx_files, go_list.cgo_cflags, go_list.cgo_cppflags)
        ctx.actions.copy_dir(outputs[cgo_gen_dir], cgo_gen_tmp_dir)

        go_files = go_list.go_files + cgo_go_files

        src_list_for_argsfile = go_files + (go_list.test_go_files + go_list.x_test_go_files if tests else [])
        ctx.actions.write(outputs[srcs_list_argsfile], cmd_args(src_list_for_argsfile, ""))

        covered_go_files, coverage_vars_out = _cover(ctx, pkg_name, go_files, coverage_mode)
        ctx.actions.write(outputs[coverage_vars_argsfile], coverage_vars_out)

        go_files_to_compile = covered_go_files + ((go_list.test_go_files + go_list.x_test_go_files) if tests else [])
        go_a_file, asmhdr = _compile(ctx, pkg_name, go_files_to_compile, importcfg, compiler_flags, shared, race, asan, embedcfg, go_list.embed_files, symabis, len(go_list.s_files) > 0)

        asm_o_files = _asssembly(ctx, pkg_name, go_list.s_files, asmhdr, assembler_flags, shared)

        pkg_file = _pack(ctx, pkg_name, go_a_file, cgo_o_files + asm_o_files)

        ctx.actions.copy_file(outputs[out], pkg_file)

    ctx.actions.dynamic_output(dynamic = [go_list_out], inputs = [], outputs = [o.as_output() for o in dynamic_outputs], f = f)

    return GoPkg(
        pkg = out,
        coverage_vars = cmd_args(coverage_vars_argsfile, format = "@{}"),
        srcs_list = cmd_args(srcs_list_argsfile, format = "@{}").hidden(srcs),
        cgo_gen_dir = cgo_gen_dir,
    )

def _compile(
        ctx: AnalysisContext,
        pkg_name: str,
        go_srcs: list[Artifact],
        importcfg: cmd_args,
        compiler_flags: list[str],
        shared: bool,
        race: bool,
        asan: bool,
        embedcfg: Artifact | None = None,
        embed_files: list[Artifact] = [],
        symabis: Artifact | None = None,
        gen_asmhdr: bool = False) -> (Artifact, Artifact | None):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    env = get_toolchain_env_vars(go_toolchain)
    out = ctx.actions.declare_output("go_compile_out.a")

    if len(go_srcs) == 0:
        ctx.actions.write(out.as_output(), "")
        return out, None

    asmhdr = ctx.actions.declare_output("__asmhdr__/go_asm.h") if gen_asmhdr else None

    compile_cmd = cmd_args([
        go_toolchain.compiler,
        go_toolchain.compiler_flags,
        compiler_flags,
        "-buildid=",
        "-nolocalimports",
        ["-p", pkg_name],
        ["-importcfg", importcfg],
        ["-o", out.as_output()],
        ["-race"] if race else [],
        ["-asan"] if asan else [],
        ["-shared"] if shared else [],
        ["-embedcfg", embedcfg] if embedcfg else [],
        ["-symabis", symabis] if symabis else [],
        ["-asmhdr", asmhdr.as_output()] if asmhdr else [],
        go_srcs,
    ])
    compile_cmd.hidden(embed_files)  #  files and directories should be available for embedding

    identifier = paths.basename(pkg_name)
    ctx.actions.run(compile_cmd, env = env, category = "go_compile", identifier = identifier)

    return (out, asmhdr)

def _symabis(ctx: AnalysisContext, pkg_name: str, s_files: list[Artifact], assembler_flags: list[str], shared: bool) -> Artifact | None:
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
        go_toolchain.assembler,
        go_toolchain.assembler_flags,
        assembler_flags,
        _asm_args(ctx, pkg_name, shared),
        "-gensymabis",
        ["-o", symabis.as_output()],
        ["-I", cmd_args(fake_asmhdr).parent(1)],
        s_files,
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(asm_cmd, env = env, category = "go_symabis", identifier = identifier)

    return symabis

def _asssembly(ctx: AnalysisContext, pkg_name: str, s_files: list[Artifact], asmhdr: Artifact | None, assembler_flags: list[str], shared: bool) -> list[Artifact]:
    if len(s_files) == 0:
        return []

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    o_files = []
    identifier = paths.basename(pkg_name)
    for s_file in s_files:
        o_file = ctx.actions.declare_output(s_file.short_path + ".o")
        o_files.append(o_file)

        asm_cmd = [
            go_toolchain.assembler,
            go_toolchain.assembler_flags,
            assembler_flags,
            _asm_args(ctx, pkg_name, shared),
            ["-o", o_file.as_output()],
            ["-I", cmd_args(asmhdr).parent(1)] if asmhdr else [],  # can it actually be None?
            s_file,
        ]

        ctx.actions.run(asm_cmd, env = env, category = "go_assembly", identifier = identifier + "/" + s_file.short_path)

    return o_files

def _pack(ctx: AnalysisContext, pkg_name: str, a_file: Artifact, o_files: list[Artifact]) -> Artifact:
    if len(o_files) == 0:
        # no need to repack .a file, if there are no .o files
        return a_file

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    pkg_file = ctx.actions.declare_output("pkg.a")

    pack_cmd = [
        go_toolchain.packer,
        "c",
        pkg_file.as_output(),
        a_file,
        o_files,
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(pack_cmd, env = env, category = "go_pack", identifier = identifier)

    return pkg_file

def _asm_args(ctx: AnalysisContext, pkg_name: str, shared: bool):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    return [
        ["-p", pkg_name],
        ["-I", cmd_args(go_toolchain.env_go_root).absolute_suffix("/pkg/include")],
        ["-D", "GOOS_" + go_toolchain.env_go_os] if go_toolchain.env_go_os else [],
        ["-D", "GOARCH_" + go_toolchain.env_go_arch] if go_toolchain.env_go_arch else [],
        ["-shared"] if shared else [],
    ]

def _cover(ctx: AnalysisContext, pkg_name: str, go_files: list[Artifact], coverage_mode: GoCoverageMode | None) -> (list[Artifact], str | cmd_args):
    if coverage_mode == None:
        return go_files, ""

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)
    covered_files = []
    coverage_vars = {}
    for go_file in go_files:
        covered_file = ctx.actions.declare_output("with_coverage", go_file.short_path)
        covered_files.append(covered_file)

        var = "Var_" + sha256(pkg_name + "::" + go_file.short_path)
        coverage_vars[var] = go_file.short_path

        cover_cmd = [
            go_toolchain.cover,
            ["-mode", coverage_mode.value],
            ["-var", var],
            ["-o", covered_file.as_output()],
            go_file,
        ]

        ctx.actions.run(cover_cmd, env = env, category = "go_cover", identifier = paths.basename(pkg_name) + "/" + go_file.short_path)

    coverage_vars_out = ""
    if len(coverage_vars) > 0:
        # convert coverage_vars to argsfile for compatibility with python implementation
        cover_pkg = "{}:{}".format(pkg_name, ",".join(["{}={}".format(var, name) for var, name in coverage_vars.items()]))
        coverage_vars_out = cmd_args("--cover-pkgs", cover_pkg)

    return covered_files, coverage_vars_out
