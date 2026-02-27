# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//utils:utils.bzl", "dedupe_by_value")
load(
    ":cgo_builder.bzl",
    "CGoBuildContext",  # @Unused used as type
    "build_cgo",
)
load(":compile.bzl", "get_inherited_compile_pkgs", "infer_package_root")
load(
    ":coverage.bzl",
    "GoCoverageMode",  # @Unused used as type
    "cover_srcs",
)
load(":go_error_handler.bzl", "go_build_error_handler")
load(":go_list.bzl", "go_list", "parse_go_list_out")
load(":packages.bzl", "GoPackageInfo", "GoPkg", "GoStdlib", "GoStdlibDynamicValue", "StdPkg", "make_compile_importcfg", "merge_pkgs")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

def build_package_wrapper(
        ctx: AnalysisContext,
        pkg_import_path: str,
        main: bool,
        srcs: list[Artifact],
        package_root: str | None,
        cgo_build_context: CGoBuildContext | None,
        pkgs: dict[str, GoPkg] = {},
        deps: list[Dependency] = [],
        compiler_flags: list[str] = [],
        assembler_flags: list[str] = [],
        build_tags: list[str] = [],
        embed_srcs: list[Artifact] = [],
        cgo_enabled: bool = False,
        coverage_mode: GoCoverageMode | None = None,
        with_tests: bool = False,
        cgo_gen_dir_name: str = "cgo_gen") -> (GoPkg, GoPackageInfo):
    actions = ctx.actions
    target_label = ctx.label
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    go_stdlib = ctx.attrs._go_stdlib[GoStdlib]

    asan = go_toolchain.asan
    race = go_toolchain.race
    if race and asan:
        fail("`race=True` and `asan=True` are mutually exclusive")

    if race and coverage_mode not in [None, GoCoverageMode("atomic")]:
        fail("`coverage_mode` must be `atomic` when `race=True`")

    out_x = actions.declare_output(paths.basename(pkg_import_path) + "_non-shared.x", has_content_based_path = True)
    out_a = actions.declare_output(paths.basename(pkg_import_path) + "_non-shared.a", has_content_based_path = True)

    out_shared_x = actions.declare_output(paths.basename(pkg_import_path) + "_shared.x", has_content_based_path = True)
    out_shared_a = actions.declare_output(paths.basename(pkg_import_path) + "_shared.a", has_content_based_path = True)

    cgo_gen_dir = actions.declare_output(cgo_gen_dir_name, dir = True, has_content_based_path = True)

    srcs = dedupe_by_value(srcs)

    package_root = package_root if package_root != None else infer_package_root(srcs)

    go_list_out = go_list(actions, go_toolchain, pkg_import_path, srcs, package_root, build_tags, cgo_enabled, with_tests = with_tests)

    test_go_files_argsfile = actions.declare_output(paths.basename(pkg_import_path) + "_test_go_files.go_package_argsfile", has_content_based_path = True)

    all_pkgs = merge_pkgs([
        pkgs,
        get_inherited_compile_pkgs(deps),
    ])

    actions.dynamic_output_new(_build_package_action(
        target_label = target_label,
        pkg_import_path = pkg_import_path,
        main = main,
        srcs = srcs,
        package_root = package_root,
        cgo_build_context = cgo_build_context,
        deps_pkgs = all_pkgs,
        compiler_flags = compiler_flags,
        assembler_flags = assembler_flags,
        coverage_mode = coverage_mode,
        embed_srcs = embed_srcs,
        with_tests = with_tests,
        go_toolchain = go_toolchain,
        go_stdlib_value = go_stdlib.dynamic_value,
        go_list_out = go_list_out,
        cgo_gen_dir = cgo_gen_dir.as_output(),
        out_a = out_a.as_output(),
        out_shared_a = out_shared_a.as_output(),
        out_shared_x = out_shared_x.as_output(),
        out_x = out_x.as_output(),
        test_go_files_argsfile = test_go_files_argsfile.as_output(),
    ))
    return GoPkg(
        pkg = out_a,
        pkg_shared = out_shared_a,
        export_file = out_x,
        export_file_shared = out_shared_x,
        test_go_files = cmd_args(test_go_files_argsfile, format = "@{}", hidden = srcs),
    ), GoPackageInfo(
        build_out = out_x,
        cgo_gen_dir = cgo_gen_dir,
        pkg_import_path = pkg_import_path,
        package_root = package_root,
        go_list_out = go_list_out,
        srcs = srcs,
    )

def _build_package_action_impl(
        actions: AnalysisActions,
        target_label: Label,
        pkg_import_path: str,
        main: bool,
        srcs: list[Artifact],
        package_root: None | str,
        cgo_build_context: None | CGoBuildContext,
        deps_pkgs: dict[str, GoPkg],
        compiler_flags: list[str],
        assembler_flags: list[str],
        coverage_mode: None | GoCoverageMode,
        embed_srcs: list[Artifact],
        with_tests: bool,
        go_toolchain: GoToolchainInfo,
        go_list_out: ArtifactValue,
        go_stdlib_value: ResolvedDynamicValue,
        cgo_gen_dir: OutputArtifact,
        out_a: OutputArtifact,
        out_shared_a: OutputArtifact,
        out_shared_x: OutputArtifact,
        out_x: OutputArtifact,
        test_go_files_argsfile: OutputArtifact):
    go_list = parse_go_list_out(srcs, package_root, go_list_out)

    if go_list.error != None:
        fail("Invalid go package: {}", go_list.error.err)

    if len(go_list.x_test_go_files) > 0:
        fail("External tests are not supported, remove suffix '_test' from package declaration '{}': {}", go_list.name, target_label)

    go_stdlib_value = go_stdlib_value.providers[GoStdlibDynamicValue]
    imports = set([] + go_list.imports + (go_list.test_imports if with_tests else []))
    embed_patterns = [] + go_list.embed_patterns + (go_list.test_embed_patterns if with_tests else [])
    go_files = [] + go_list.go_files + (go_list.test_go_files if with_tests else [])

    result = build_package(
        actions = actions,
        target_label = target_label,
        go_toolchain = go_toolchain,
        cgo_build_context = cgo_build_context,
        params = BuildPackageParams(
            main = main,
            pkg_name = go_list.name,
            pkg_import_path = pkg_import_path,
            package_root = package_root,
            go_files = go_files,
            cgo_files = go_list.cgo_files,
            s_files = go_list.s_files,
            h_files = go_list.h_files,
            c_files = go_list.c_files,
            cxx_files = go_list.cxx_files,
            imports = imports,
            embed_patterns = embed_patterns,
            embed_srcs = embed_srcs,
            compiler_flags = compiler_flags,
            assembler_flags = assembler_flags,
            cgo_cflags = go_list.cgo_cflags,
            cgo_cppflags = go_list.cgo_cppflags,
            coverage_mode = coverage_mode,
            deps = deps_pkgs,
            stdlib_deps = go_stdlib_value.pkgs,
        ),
    )

    actions.write(test_go_files_argsfile, cmd_args((go_list.test_go_files if with_tests else []), ""))
    actions.copy_dir(cgo_gen_dir, result.cgo_gen_dir)

    actions.copy_file(out_x, result.x_file)
    actions.copy_file(out_a, result.a_file)

    actions.copy_file(out_shared_x, result.x_file_shared)
    actions.copy_file(out_shared_a, result.a_file_shared)

    return []

_build_package_action = dynamic_actions(
    impl = _build_package_action_impl,
    # @unsorted-dict-items
    attrs = {
        # Input Parameters
        "target_label": dynattrs.value(Label),
        "pkg_import_path": dynattrs.value(str),
        "main": dynattrs.value(bool),
        "srcs": dynattrs.value(list[Artifact]),
        "package_root": dynattrs.value(str | None),
        "cgo_build_context": dynattrs.value(CGoBuildContext | None),
        "deps_pkgs": dynattrs.value(dict[str, GoPkg]),
        "compiler_flags": dynattrs.value(list[str]),
        "assembler_flags": dynattrs.value(list[str]),
        "coverage_mode": dynattrs.value(GoCoverageMode | None),
        "embed_srcs": dynattrs.value(list[Artifact]),
        "with_tests": dynattrs.value(bool),
        "go_toolchain": dynattrs.value(GoToolchainInfo),
        # Readable Artifacts
        "go_list_out": dynattrs.artifact_value(),
        # Dynamic Values
        "go_stdlib_value": dynattrs.dynamic_value(),  # GoStdlibDynamicValue
        # Outputs
        "cgo_gen_dir": dynattrs.output(),
        "out_a": dynattrs.output(),
        "out_shared_a": dynattrs.output(),
        "out_shared_x": dynattrs.output(),
        "out_x": dynattrs.output(),
        "test_go_files_argsfile": dynattrs.output(),
    },
)

BuildPackageParams = record(
    main = field(bool),
    pkg_name = field(str),
    pkg_import_path = field(str),
    package_root = field(str),
    go_files = field(list[Artifact]),
    cgo_files = field(list[Artifact]),
    s_files = field(list[Artifact]),
    h_files = field(list[Artifact]),
    c_files = field(list[Artifact]),
    cxx_files = field(list[Artifact]),
    imports = field(set[str]),
    embed_patterns = field(list[str]),
    embed_srcs = field(list[Artifact]),
    compiler_flags = field(list[str]),
    assembler_flags = field(list[str]),
    cgo_cflags = field(list[str]),
    cgo_cppflags = field(list[str]),
    coverage_mode = field(GoCoverageMode | None),
    deps = field(dict[str, GoPkg]),
    stdlib_deps = field(dict[str, StdPkg]),
)
BuildPackageResult = record(
    a_file = field(Artifact),
    x_file = field(Artifact),
    a_file_shared = field(Artifact),
    x_file_shared = field(Artifact),
    cgo_gen_dir = field(Artifact),
)

def build_package(
        actions: AnalysisActions,
        target_label: Label,
        go_toolchain: GoToolchainInfo,
        cgo_build_context: CGoBuildContext | None,
        params: BuildPackageParams) -> BuildPackageResult:
    covered_go_files, covered_cgo_files, coveragecfg = cover_srcs(
        actions = actions,
        go_toolchain = go_toolchain,
        pkg_name = params.pkg_name,
        pkg_import_path = params.pkg_import_path,
        go_files = params.go_files,
        cgo_files = params.cgo_files,
        coverage_mode = params.coverage_mode,
    )

    # A go package can can contain CGo or Go ASM files, but not both.
    # If CGo and ASM files are present, we process ASM files together with C files with CxxToolchain.
    # The `go build` command does additional check here and throws an error if both CGo and Go-ASM files are present.
    c_files = params.c_files + params.cxx_files
    s_files = params.s_files
    if len(params.cgo_files) > 0:
        c_files += s_files
        s_files = []

    # Generate CGO and C sources.
    transformed_cgo_files, cgo_o_files, cgo_gen_dir = build_cgo(
        actions = actions,
        target_label = target_label,
        go_toolchain_info = go_toolchain,
        cgo_build_context = cgo_build_context,
        cgo_files = covered_cgo_files,
        h_files = params.h_files,
        c_files = c_files,
        c_flags = params.cgo_cflags,
        cpp_flags = params.cgo_cppflags,
        anon_targets_allowed = False,
    )

    symabis = _symabis(actions, go_toolchain, params.pkg_import_path, params.main, s_files, params.h_files, params.assembler_flags)

    embedcfg = _embedcfg(actions, go_toolchain, params.pkg_import_path, params.package_root, params.embed_srcs, params.embed_patterns)

    # Use -complete flag when compiling Go code only
    complete_flag = len(params.cgo_files) + len(s_files) + len(c_files) == 0

    def build_variant(shared: bool) -> (Artifact, Artifact):
        suffix = "_shared" if shared else "_non-shared"  # suffix to make artifacts unique
        go_files_to_compile = covered_go_files + transformed_cgo_files

        importcfg = make_compile_importcfg(
            actions = actions,
            stdlib_deps = params.stdlib_deps,
            deps = params.deps,
            imports = params.imports,
            has_cgo_files = len(params.cgo_files) > 0,
            coverage_enabled = params.coverage_mode != None,
            shared = shared,
        )
        go_x_file, go_a_file, asmhdr = _compile(
            actions = actions,
            go_toolchain = go_toolchain,
            pkg_import_path = params.pkg_import_path,
            main = params.main,
            go_srcs = go_files_to_compile,
            importcfg = importcfg,
            compiler_flags = params.compiler_flags,
            shared = shared,
            race = go_toolchain.race,
            asan = go_toolchain.asan,
            suffix = suffix,
            complete = complete_flag,
            coveragecfg = coveragecfg,
            embedcfg = embedcfg,
            symabis = symabis,
            gen_asmhdr = len(s_files) > 0,
        )

        asm_o_files = _asssembly(actions, go_toolchain, params.pkg_import_path, params.main, s_files, params.h_files, asmhdr, params.assembler_flags, shared, suffix)

        return go_x_file, _pack(actions, go_toolchain, params.pkg_import_path, go_a_file, cgo_o_files + asm_o_files, suffix)

    non_shared_x, non_shared_a = build_variant(shared = False)

    shared_x, shared_a = build_variant(shared = True)

    return BuildPackageResult(
        a_file = non_shared_a,
        x_file = non_shared_x,
        a_file_shared = shared_a,
        x_file_shared = shared_x,
        cgo_gen_dir = cgo_gen_dir,
    )

def _compile(
        actions: AnalysisActions,
        go_toolchain: GoToolchainInfo,
        pkg_import_path: str,
        main: bool,
        go_srcs: list[Artifact],
        importcfg: Artifact,
        compiler_flags: list[str],
        shared: bool,
        race: bool,
        asan: bool,
        suffix: str,
        complete: bool,
        coveragecfg: Artifact | None = None,
        embedcfg: Artifact | None = None,
        symabis: Artifact | None = None,
        gen_asmhdr: bool = False) -> (Artifact, Artifact, Artifact | None):
    env = get_toolchain_env_vars(go_toolchain)
    out_a = actions.declare_output("go_compile_out{}.a".format(suffix), has_content_based_path = True)
    out_x = actions.declare_output("go_compile_out{}.x".format(suffix), has_content_based_path = True)

    if len(go_srcs) == 0:
        actions.write(out_a.as_output(), "")
        actions.write(out_x.as_output(), "")
        return out_x, out_a, None

    asmhdr = actions.declare_output("__asmhdr__{}/go_asm.h".format(suffix), has_content_based_path = True) if gen_asmhdr else None

    # Use argsfile to avoid command length limit on Windows
    srcs_argsfile = actions.write(paths.basename(pkg_import_path) + suffix + "_srcs.go_package_argsfile", go_srcs, has_content_based_path = True)

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
            ["-p", "main" if main else pkg_import_path],
            ["-importcfg", importcfg],
            ["-o", out_x.as_output()],
            ["-linkobj", out_a.as_output()],
            ["-race"] if race else [],
            ["-asan"] if asan else [],
            ["-d=libfuzzer"] if go_toolchain.fuzz else [],
            ["-shared"] if shared else [],
            ["-coveragecfg", coveragecfg] if coveragecfg else [],
            ["-embedcfg", embedcfg] if embedcfg else [],
            ["-symabis", symabis] if symabis else [],
            ["-asmhdr", asmhdr.as_output()] if asmhdr else [],
            ["-complete"] if complete else [],
            cmd_args(srcs_argsfile, format = "@{}", hidden = go_srcs),
        ],
    )

    identifier = paths.basename(pkg_import_path)
    actions.run(compile_cmd, env = env, category = "go_compile", identifier = identifier + suffix, error_handler = go_build_error_handler)

    return (out_x, out_a, asmhdr)

def _symabis(
        actions: AnalysisActions,
        go_toolchain: GoToolchainInfo,
        pkg_import_path: str,
        main: bool,
        s_files: list[Artifact],
        h_files: list[Artifact],
        assembler_flags: list[str]) -> Artifact | None:
    if len(s_files) == 0:
        return None

    env = get_toolchain_env_vars(go_toolchain)

    # we have to supply "go_asm.h" with any content to make asm tool happy
    # its content doesn't matter if -gensymabis provided
    # https://github.com/golang/go/blob/3f8f929d60a90c4e4e2b07c8d1972166c1a783b1/src/cmd/go/internal/work/gc.go#L441-L443
    fake_asmhdr = actions.write("__fake_asmhdr__/go_asm.h", "", has_content_based_path = True)
    symabis = actions.declare_output("symabis", has_content_based_path = True)
    asm_cmd = [
        go_toolchain.go_wrapper,
        ["--go", go_toolchain.assembler],
        "--",
        go_toolchain.assembler_flags,
        assembler_flags,
        _asm_args(go_toolchain, pkg_import_path, main, False),  # flag -shared doesn't matter for symabis
        "-gensymabis",
        ["-o", symabis.as_output()],
        ["-I", cmd_args(fake_asmhdr, parent = 1)],
        [cmd_args(h_files, parent = 1, prepend = "-I")] if h_files else [],
        [cmd_args(s_files, parent = 1, prepend = "-I")],  # some .s files can include other .s files
        ["-trimpath", "%cwd%"],
        s_files,
    ]

    identifier = paths.basename(pkg_import_path)
    actions.run(asm_cmd, env = env, category = "go_symabis", identifier = identifier)

    return symabis

def _asssembly(
        actions: AnalysisActions,
        go_toolchain: GoToolchainInfo,
        pkg_import_path: str,
        main: bool,
        s_files: list[Artifact],
        h_files: list[Artifact],
        asmhdr: Artifact | None,
        assembler_flags: list[str],
        shared: bool,
        suffix: str) -> list[Artifact]:
    if len(s_files) == 0:
        return []

    env = get_toolchain_env_vars(go_toolchain)

    o_files = []
    identifier = paths.basename(pkg_import_path)
    for s_file in s_files:
        o_file = actions.declare_output(s_file.short_path + suffix + ".o", has_content_based_path = True)
        o_files.append(o_file)

        asm_cmd = [
            go_toolchain.go_wrapper,
            ["--go", go_toolchain.assembler],
            "--",
            go_toolchain.assembler_flags,
            assembler_flags,
            _asm_args(go_toolchain, pkg_import_path, main, shared),
            ["-o", o_file.as_output()],
            ["-I", cmd_args(asmhdr, parent = 1)] if asmhdr else [],  # can it actually be None?
            [cmd_args(h_files, parent = 1, prepend = "-I")] if h_files else [],
            [cmd_args(s_files, parent = 1, prepend = "-I")],  # some .s files can include other .s files
            ["-trimpath", "%cwd%"],
            s_file,
        ]

        actions.run(asm_cmd, env = env, category = "go_assembly", identifier = identifier + "/" + s_file.short_path + suffix)

    return o_files

def _pack(actions: AnalysisActions, go_toolchain: GoToolchainInfo, pkg_import_path: str, a_file: Artifact, o_files: list[Artifact], suffix: str) -> Artifact:
    if len(o_files) == 0:
        # no need to repack .a file, if there are no .o files
        return a_file

    env = get_toolchain_env_vars(go_toolchain)

    pkg_file = actions.declare_output("pkg{}.a".format(suffix), has_content_based_path = True)

    pack_cmd = [
        go_toolchain.packer,
        "c",
        pkg_file.as_output(),
        a_file,
        o_files,
    ]

    identifier = paths.basename(pkg_import_path)
    actions.run(pack_cmd, env = env, category = "go_pack", identifier = identifier + suffix)

    return pkg_file

def _embedcfg(actions: AnalysisActions, go_toolchain: GoToolchainInfo, pkg_import_path: str, package_root: str, embed_srcs: list[Artifact], embed_patterns: list[str]) -> Artifact | None:
    if len(embed_patterns) == 0:
        return None

    embedcfg = actions.declare_output("embedcfg", has_content_based_path = True)

    srcs_dir = actions.copied_dir(
        "__embed_srcs_dir__",
        {src.short_path.removeprefix(package_root).lstrip("/"): src for src in embed_srcs},
        has_content_based_path = True,
    )

    embed_cmd = [
        go_toolchain.gen_embedcfg,
        ["-o", embedcfg.as_output()],
        ["-pkgdir", srcs_dir],
        embed_patterns,
    ]

    identifier = paths.basename(pkg_import_path)
    actions.run(embed_cmd, category = "go_embedcfg", identifier = identifier)

    return embedcfg.with_associated_artifacts([srcs_dir])

def _asm_args(go_toolchain: GoToolchainInfo, pkg_import_path: str, main: bool, shared: bool):
    return [
        ["-p", "main" if main else pkg_import_path],
        ["-I", go_toolchain.env_go_root.project("pkg/include")] if go_toolchain.env_go_root else [],
        ["-D", "GOOS_" + go_toolchain.env_go_os] if go_toolchain.env_go_os else [],
        ["-D", "GOARCH_" + go_toolchain.env_go_arch] if go_toolchain.env_go_arch else [],
        ["-shared"] if shared else [],
    ]
