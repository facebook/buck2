# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:utils.bzl", "dedupe_by_value")
load(":compile.bzl", "get_inherited_compile_pkgs", "infer_package_root")
load(
    ":coverage.bzl",
    "GoCoverageMode",  # @Unused used as type
)
load(":packages.bzl", "GoPkg", "make_importcfg", "merge_pkgs", "pkg_artifacts")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_cmd_args")

def build_package(
        ctx: AnalysisContext,
        pkg_name: str,
        srcs: list[Artifact],
        package_root: str | None,
        pkgs: dict[str, Artifact] = {},
        deps: list[Dependency] = [],
        compiled_objects: list[Artifact] = [],
        # hack: extra go files will bypass filtration to enable cgo_library migration
        extra_go_files: list[Artifact] = [],
        compiler_flags: list[str] = [],
        assembler_flags: list[str] = [],
        shared: bool = False,
        race: bool = False,
        coverage_mode: GoCoverageMode | None = None,
        embedcfg: Artifact | None = None,
        tests: bool = False,
        force_disable_cgo: bool = False) -> GoPkg:
    if race and coverage_mode not in [None, GoCoverageMode("atomic")]:
        fail("`coverage_mode` must be `atomic` when `race=True`")

    out = ctx.actions.declare_output(paths.basename(pkg_name) + ".a")

    srcs = dedupe_by_value(srcs)

    has_go_files = False
    for src in (srcs + extra_go_files):
        if src.extension == ".go":
            has_go_files = True
            break

    if not has_go_files:
        return GoPkg(
            pkg = ctx.actions.write(out.as_output(), ""),
            coverage_vars = cmd_args(),
            srcs_list = cmd_args(),
        )

    package_root = package_root if package_root != None else infer_package_root(srcs)

    go_list_out = _go_list(ctx, pkg_name, srcs, package_root, force_disable_cgo)

    srcs_list_argsfile = ctx.actions.declare_output(paths.basename(pkg_name) + "_srcs_list.argsfile")
    coverage_vars_argsfile = ctx.actions.declare_output(paths.basename(pkg_name) + "_coverage_vars.argsfile")
    dynamic_outputs = [out, srcs_list_argsfile, coverage_vars_argsfile]

    all_pkgs = merge_pkgs([
        pkgs,
        pkg_artifacts(get_inherited_compile_pkgs(deps)),
    ])
    importcfg = make_importcfg(ctx, pkg_name, all_pkgs, with_importmap = True)

    def f(ctx: AnalysisContext, artifacts, outputs, go_list_out = go_list_out):
        go_list = _parse_go_list_out(srcs, package_root, artifacts[go_list_out])

        symabis = _symabis(ctx, pkg_name, go_list.s_files, assembler_flags, shared)

        go_files = go_list.go_files + extra_go_files

        src_list_for_argsfile = go_files + (go_list.test_go_files + go_list.x_test_go_files if tests else [])
        ctx.actions.write(outputs[srcs_list_argsfile], cmd_args(src_list_for_argsfile, ""))

        covered_go_files, coverage_vars_out = _cover(ctx, pkg_name, go_files, coverage_mode)
        ctx.actions.write(outputs[coverage_vars_argsfile], coverage_vars_out)

        go_files_to_compile = covered_go_files + ((go_list.test_go_files + go_list.x_test_go_files) if tests else [])
        go_a_file, asmhdr = _compile(ctx, pkg_name, go_files_to_compile, importcfg, compiler_flags, shared, race, embedcfg, go_list.embed_files, symabis, len(go_list.s_files) > 0)

        asm_o_files = _asssembly(ctx, pkg_name, go_list.s_files, asmhdr, assembler_flags, shared)

        pkg_file = _pack(ctx, pkg_name, go_a_file, compiled_objects + asm_o_files)

        ctx.actions.copy_file(outputs[out], pkg_file)

    ctx.actions.dynamic_output(dynamic = [go_list_out], inputs = [], outputs = [o.as_output() for o in dynamic_outputs], f = f)

    return GoPkg(
        pkg = out,
        coverage_vars = cmd_args(coverage_vars_argsfile, format = "@{}"),
        srcs_list = cmd_args(srcs_list_argsfile, format = "@{}").hidden(srcs),
    )

def _go_list(ctx: AnalysisContext, pkg_name: str, srcs: list[Artifact], package_root: str, force_disable_cgo: bool):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env_args = get_toolchain_cmd_args(go_toolchain, go_root = True, force_disable_cgo = force_disable_cgo)
    go_list_out = ctx.actions.declare_output(paths.basename(pkg_name) + "_go_list.json")

    # Create file sructure that `go list` can recognize
    # Use copied_dir, because embed doesn't work with symlinks
    srcs_dir = ctx.actions.copied_dir(
        "__{}_srcs_dir__".format(paths.basename(pkg_name)),
        {src.short_path.removeprefix(package_root).lstrip("/"): src for src in srcs},
    )
    tags = go_toolchain.tags + ctx.attrs._tags
    go_list_args = [
        env_args,
        "GO111MODULE=off",
        go_toolchain.go_list_wrapper,
        "-e",
        ["--go", go_toolchain.go],
        ["--workdir", srcs_dir],
        ["--output", go_list_out.as_output()],
        "-json=GoFiles,CgoFiles,SFiles,TestGoFiles,XTestGoFiles,EmbedFiles",
        ["-tags", ",".join(tags) if tags else []],
        ".",
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(go_list_args, category = "go_list", identifier = identifier)

    return go_list_out

GoListOut = record(
    go_files = field(list[Artifact], default = []),
    cgo_files = field(list[Artifact], default = []),
    s_files = field(list[Artifact], default = []),
    test_go_files = field(list[Artifact], default = []),
    x_test_go_files = field(list[Artifact], default = []),
    embed_files = field(list[Artifact], default = []),
)

def _parse_go_list_out(srcs: list[Artifact], package_root: str, go_list_out) -> GoListOut:
    go_list = go_list_out.read_json()
    go_files, cgo_files, s_files, test_go_files, x_test_go_files, embed_files = [], [], [], [], [], []

    for src in srcs:
        # remove package_root prefix from src artifact path to match `go list` outout format
        src_path = src.short_path.removeprefix(package_root).lstrip("/")
        if src_path in go_list.get("GoFiles", []):
            go_files.append(src)
        if src_path in go_list.get("CgoFiles", []):
            cgo_files.append(src)
        if src_path in go_list.get("SFiles", []):
            s_files.append(src)
        if src_path in go_list.get("TestGoFiles", []):
            test_go_files.append(src)
        if src_path in go_list.get("XTestGoFiles", []):
            x_test_go_files.append(src)
        if _any_starts_with(go_list.get("EmbedFiles", []), src_path):
            embed_files.append(src)

    return GoListOut(
        go_files = go_files,
        cgo_files = cgo_files,
        s_files = s_files,
        test_go_files = test_go_files,
        x_test_go_files = x_test_go_files,
        embed_files = embed_files,
    )

def _any_starts_with(files: list[str], path: str):
    for file in files:
        if paths.starts_with(file, path):
            return True

    return False

def _compile(
        ctx: AnalysisContext,
        pkg_name: str,
        go_srcs: list[Artifact],
        importcfg: cmd_args,
        compiler_flags: list[str],
        shared: bool,
        race: bool,
        embedcfg: Artifact | None = None,
        embed_files: list[Artifact] = [],
        symabis: Artifact | None = None,
        gen_asmhdr: bool = False) -> (Artifact, Artifact | None):
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    env_args = get_toolchain_cmd_args(go_toolchain, go_root = True)
    out = ctx.actions.declare_output("go_compile_out.a")

    if len(go_srcs) == 0:
        ctx.actions.write(out.as_output(), "")
        return out, None

    asmhdr = ctx.actions.declare_output("__asmhdr__/go_asm.h") if gen_asmhdr else None

    compile_cmd = cmd_args([
        env_args,
        go_toolchain.compiler,
        go_toolchain.compiler_flags,
        compiler_flags,
        "-buildid=",
        "-nolocalimports",
        ["-p", pkg_name],
        ["-importcfg", importcfg],
        ["-o", out.as_output()],
        ["-race"] if race else [],
        ["-shared"] if shared else [],
        ["-embedcfg", embedcfg] if embedcfg else [],
        ["-symabis", symabis] if symabis else [],
        ["-asmhdr", asmhdr.as_output()] if asmhdr else [],
        go_srcs,
    ])
    compile_cmd.hidden(embed_files)  #  files and directories should be available for embedding

    identifier = paths.basename(pkg_name)
    ctx.actions.run(compile_cmd, category = "go_compile", identifier = identifier)

    return (out, asmhdr)

def _symabis(ctx: AnalysisContext, pkg_name: str, s_files: list[Artifact], assembler_flags: list[str], shared: bool) -> Artifact | None:
    if len(s_files) == 0:
        return None

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env_args = get_toolchain_cmd_args(go_toolchain, go_root = True)

    # we have to supply "go_asm.h" with any content to make asm tool happy
    # its content doesn't matter if -gensymabis provided
    # https://github.com/golang/go/blob/3f8f929d60a90c4e4e2b07c8d1972166c1a783b1/src/cmd/go/internal/work/gc.go#L441-L443
    fake_asmhdr = ctx.actions.write("__fake_asmhdr__/go_asm.h", "")
    symabis = ctx.actions.declare_output("symabis")
    asm_cmd = [
        env_args,
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
    ctx.actions.run(asm_cmd, category = "go_symabis", identifier = identifier)

    return symabis

def _asssembly(ctx: AnalysisContext, pkg_name: str, s_files: list[Artifact], asmhdr: Artifact | None, assembler_flags: list[str], shared: bool) -> list[Artifact]:
    if len(s_files) == 0:
        return []

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env_args = get_toolchain_cmd_args(go_toolchain, go_root = True)

    o_files = []
    identifier = paths.basename(pkg_name)
    for s_file in s_files:
        o_file = ctx.actions.declare_output(s_file.short_path + ".o")
        o_files.append(o_file)

        asm_cmd = [
            env_args,
            go_toolchain.assembler,
            go_toolchain.assembler_flags,
            assembler_flags,
            _asm_args(ctx, pkg_name, shared),
            ["-o", o_file.as_output()],
            ["-I", cmd_args(asmhdr).parent(1)] if asmhdr else [],  # can it actually be None?
            s_file,
        ]

        ctx.actions.run(asm_cmd, category = "go_assembly", identifier = identifier + "/" + s_file.short_path)

    return o_files

def _pack(ctx: AnalysisContext, pkg_name: str, a_file: Artifact, o_files: list[Artifact]) -> Artifact:
    if len(o_files) == 0:
        # no need to repack .a file, if there are no .o files
        return a_file

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env_args = get_toolchain_cmd_args(go_toolchain, go_root = True)

    pkg_file = ctx.actions.declare_output("pkg.a")

    pack_cmd = [
        env_args,
        go_toolchain.packer,
        "c",
        pkg_file.as_output(),
        a_file,
        o_files,
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(pack_cmd, category = "go_pack", identifier = identifier)

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
    env_args = get_toolchain_cmd_args(go_toolchain, go_root = True)
    covered_files = []
    coverage_vars = {}
    for go_file in go_files:
        covered_file = ctx.actions.declare_output("with_coverage", go_file.short_path)
        covered_files.append(covered_file)

        var = "Var_" + sha256(pkg_name + "::" + go_file.short_path)
        coverage_vars[var] = go_file.short_path

        cover_cmd = [
            env_args,
            go_toolchain.cover,
            ["-mode", coverage_mode.value],
            ["-var", var],
            ["-o", covered_file.as_output()],
            go_file,
        ]

        ctx.actions.run(cover_cmd, category = "go_cover", identifier = paths.basename(pkg_name) + "/" + go_file.short_path)

    coverage_vars_out = ""
    if len(coverage_vars) > 0:
        # convert coverage_vars to argsfile for compatibility with python implementation
        cover_pkg = "{}:{}".format(pkg_name, ",".join(["{}={}".format(var, name) for var, name in coverage_vars.items()]))
        coverage_vars_out = cmd_args("--cover-pkgs", cover_pkg)

    return covered_files, coverage_vars_out
