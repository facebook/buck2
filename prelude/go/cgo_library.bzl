# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//apple:xcode.bzl",
    "get_project_root_file",
)
load(
    "@prelude//cxx:compile.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@prelude//cxx:cxx_library.bzl", "cxx_compile_srcs")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//cxx:cxx_types.bzl",
    "CxxRuleConstructorParams",  # @unused Used as a type
)
load("@prelude//cxx:headers.bzl", "cxx_get_regular_cxx_headers_layout", "prepare_headers")
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
    "CPreprocessorInfo",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
    "cxx_private_preprocessor_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
    "MergedLinkInfo",
    "create_merged_link_info_for_propagation",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
)
load("@prelude//linking:types.bzl", "Linkage")
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:cmd_script.bzl", "ScriptOs", "cmd_script")
load("@prelude//utils:expect.bzl", "expect")
load(
    "@prelude//utils:utils.bzl",
    "map_idx",
)
load(":compile.bzl", "GoPkgCompileInfo", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "GoPkg", "go_attr_pkg_name", "merge_pkgs")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

# A map of expected linkages for provided link style
_LINKAGE_FOR_LINK_STYLE = {
    LinkStyle("static"): Linkage("static"),
    LinkStyle("static_pic"): Linkage("static"),
    LinkStyle("shared"): Linkage("shared"),
}

def _cgo(
        ctx: AnalysisContext,
        srcs: list[Artifact],
        own_pre: list[CPreprocessor],
        inherited_pre: list[CPreprocessorInfo]) -> (list[Artifact], list[Artifact], list[Artifact]):
    """
    Run `cgo` on `.go` sources to generate Go, C, and C-Header sources.
    """

    # If you change this dir or naming convention, please
    # update the corresponding logic in `fbgolist`.
    # Otherwise editing and linting for Go will break.
    gen_dir = "cgo_gen"

    go_srcs = []
    c_headers = []
    c_srcs = []
    go_srcs.append(ctx.actions.declare_output(paths.join(gen_dir, "_cgo_gotypes.go")))
    c_srcs.append(ctx.actions.declare_output(paths.join(gen_dir, "_cgo_export.c")))
    c_headers.append(ctx.actions.declare_output(paths.join(gen_dir, "_cgo_export.h")))
    for src in srcs:
        go_srcs.append(ctx.actions.declare_output(paths.join(gen_dir, paths.replace_extension(src.basename, ".cgo1.go"))))
        c_srcs.append(ctx.actions.declare_output(paths.join(gen_dir, paths.replace_extension(src.basename, ".cgo2.c"))))

    # Return a `cmd_args` to use as the generated sources.
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]

    cmd = cmd_args()
    cmd.add(go_toolchain.cgo_wrapper)

    args = cmd_args()
    args.add(cmd_args(go_toolchain.cgo, format = "--cgo={}"))

    # TODO(agallagher): cgo outputs a dir with generated sources, but I'm not
    # sure how to pass in an output dir *and* enumerate the sources we know will
    # generated w/o v2 complaining that the output dir conflicts with the nested
    # artifacts.
    args.add(cmd_args(go_srcs[0].as_output(), format = "--output={}/.."))

    args.add(srcs)

    argsfile = ctx.actions.declare_output(paths.join(gen_dir, ".cgo.argsfile"))
    ctx.actions.write(argsfile.as_output(), args, allow_args = True)

    cmd.add(cmd_args(argsfile, format = "@{}").hidden([args]))

    for src in go_srcs + c_headers + c_srcs:
        cmd.hidden(src.as_output())

    env = get_toolchain_env_vars(go_toolchain)
    env["CC"] = _cxx_wrapper(ctx, own_pre, inherited_pre)

    ctx.actions.run(cmd, env = env, category = "cgo")

    return go_srcs, c_headers, c_srcs

def _cxx_wrapper(ctx: AnalysisContext, own_pre: list[CPreprocessor], inherited_pre: list[CPreprocessorInfo]) -> cmd_args:
    pre = cxx_merge_cpreprocessors(ctx, own_pre, inherited_pre)
    pre_args = pre.set.project_as_args("args")
    pre_include_dirs = pre.set.project_as_args("include_dirs")

    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    expect(CxxToolchainInfo in ctx.attrs._cxx_toolchain)
    cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]

    c_compiler = cxx_toolchain.c_compiler_info

    # Construct the full C/C++ command needed to preprocess/compile sources.
    cxx_cmd = cmd_args(
        c_compiler.compiler,
        c_compiler.preprocessor_flags,
        c_compiler.compiler_flags,
        pre_args,
        pre_include_dirs,
        go_toolchain.c_compiler_flags,
    )

    # Wrap the C/C++ command in a wrapper script to avoid arg length limits.
    return cmd_script(
        ctx = ctx,
        name = "cxx_wrapper",
        cmd = cxx_cmd,
        os = ScriptOs("windows" if ctx.attrs._exec_os_type[OsLookup].platform == "windows" else "unix"),
    )

def cgo_library_impl(ctx: AnalysisContext) -> list[Provider]:
    pkg_name = go_attr_pkg_name(ctx)

    project_root_file = get_project_root_file(ctx)

    # Gather preprocessor inputs.
    (own_pre, _) = cxx_private_preprocessor_info(
        ctx,
        cxx_get_regular_cxx_headers_layout(ctx),
        project_root_file = project_root_file,
    )
    inherited_pre = cxx_inherited_preprocessor_infos(ctx.attrs.deps)

    # Separate sources into C++ and CGO sources.
    cgo_srcs = []
    cxx_srcs = []
    for src in ctx.attrs.srcs:
        if src.extension == ".go":
            cgo_srcs.append(src)
        elif src.extension in (".c", ".cpp"):
            cxx_srcs.append(src)
        else:
            fail("unexpected extension: {}".format(src))

    # Generate CGO and C sources.
    go_srcs, c_headers, c_srcs = _cgo(ctx, cgo_srcs, [own_pre], inherited_pre)
    cxx_srcs.extend(c_srcs)

    # Wrap the generated CGO C headers in a CPreprocessor object for compiling.
    cgo_headers_pre = CPreprocessor(relative_args = CPreprocessorArgs(args = [
        "-I",
        prepare_headers(
            ctx,
            {h.basename: h for h in c_headers},
            "cgo-private-headers",
            None,
        ).include_path,
    ]))

    link_style = ctx.attrs.link_style
    if link_style == None:
        link_style = "static"
    linkage = _LINKAGE_FOR_LINK_STYLE[LinkStyle(link_style)]

    # Copmile C++ sources into object files.
    c_compile_cmds = cxx_compile_srcs(
        ctx,
        CxxRuleConstructorParams(
            rule_type = "cgo_library",
            headers_layout = cxx_get_regular_cxx_headers_layout(ctx),
            srcs = [CxxSrcWithFlags(file = src) for src in cxx_srcs],
        ),
        # Create private header tree and propagate via args.
        [own_pre, cgo_headers_pre],
        inherited_pre,
        [],
        linkage,
    )

    compiled_objects = c_compile_cmds.pic.objects

    shared = ctx.attrs._compile_shared
    race = ctx.attrs._race
    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None

    # Build Go library.
    compiled_pkg = build_package(
        ctx,
        pkg_name,
        ctx.attrs.go_srcs,
        package_root = ctx.attrs.package_root,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        compiled_objects = compiled_objects,
        extra_go_files = go_srcs,
        shared = shared,
        race = race,
        coverage_mode = coverage_mode,
        embedcfg = ctx.attrs.embedcfg,
    )

    # Temporarily hack, it seems like we can update record, so create new one
    compiled_pkg = GoPkg(
        pkg = compiled_pkg.pkg,
        coverage_vars = compiled_pkg.coverage_vars,
    )

    pkgs = {
        pkg_name: compiled_pkg,
    }

    # We need to keep pre-processed cgo source files,
    # because they are required for any editing and linting (like VSCode+gopls)
    # to work with cgo. And when nearly every FB service client is cgo,
    # we need to support it well.
    return [
        DefaultInfo(default_output = compiled_pkg.pkg, other_outputs = go_srcs),
        GoPkgCompileInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_compile_pkgs(ctx.attrs.exported_deps),
        ])),
        GoPkgLinkInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_link_pkgs(ctx.attrs.deps + ctx.attrs.exported_deps),
        ])),
        create_merged_link_info_for_propagation(ctx, filter(None, [d.get(MergedLinkInfo) for d in ctx.attrs.deps])),
        merge_shared_libraries(
            ctx.actions,
            deps = filter(None, map_idx(SharedLibraryInfo, ctx.attrs.deps)),
        ),
    ]
