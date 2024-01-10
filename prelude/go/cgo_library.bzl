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
    "Linkage",
    "MergedLinkInfo",
    "create_merged_link_info_for_propagation",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
)
load("@prelude//os_lookup:defs.bzl", "OsLookup")
load("@prelude//utils:expect.bzl", "expect")
load(
    "@prelude//utils:utils.bzl",
    "map_idx",
)
load(":compile.bzl", "GoPkgCompileInfo", "compile", "get_filtered_srcs", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode", "cover_srcs")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":packages.bzl", "GoPkg", "go_attr_pkg_name", "merge_pkgs")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_cmd_args")

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

    pre = cxx_merge_cpreprocessors(ctx, own_pre, inherited_pre)
    pre_args = pre.set.project_as_args("args")
    pre_include_dirs = pre.set.project_as_args("include_dirs")

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
    expect(go_toolchain.cgo != None)
    expect(CxxToolchainInfo in ctx.attrs._cxx_toolchain)
    cxx_toolchain = ctx.attrs._cxx_toolchain[CxxToolchainInfo]

    cmd = get_toolchain_cmd_args(go_toolchain, go_root = False)
    cmd.add(go_toolchain.cgo_wrapper[RunInfo])

    args = cmd_args()
    args.add(cmd_args(go_toolchain.cgo, format = "--cgo={}"))

    c_compiler = cxx_toolchain.c_compiler_info
    # linker = cxx_toolchain.linker_info

    # Passing fbcode-platform ldflags may create S365277, so I would
    # comment this change until we really need to do it.
    # ldflags = cmd_args(
    #     linker.linker_flags,
    #     go_toolchain.external_linker_flags,
    # )

    # Construct the full C/C++ command needed to preprocess/compile sources.
    cxx_cmd = cmd_args()
    cxx_cmd.add(c_compiler.compiler)
    cxx_cmd.add(c_compiler.preprocessor_flags)
    cxx_cmd.add(c_compiler.compiler_flags)
    cxx_cmd.add(pre_args)
    cxx_cmd.add(pre_include_dirs)

    # Passing the same value as go-build, because our -g flags break cgo
    # in some buck modes
    cxx_cmd.add("-g")

    # Wrap the C/C++ command in a wrapper script to avoid arg length limits.
    is_win = ctx.attrs._exec_os_type[OsLookup].platform == "windows"
    cxx_sh = cmd_args(
        [
            cmd_args(cxx_cmd, quote = "shell"),
            "%*" if is_win else "\"$@\"",
        ],
        delimiter = " ",
    )
    cxx_wrapper, _ = ctx.actions.write(
        "__{}_cxx__.{}".format(ctx.label.name, "bat" if is_win else "sh"),
        ([] if is_win else ["#!/bin/sh"]) + [cxx_sh],
        allow_args = True,
        is_executable = True,
    )
    args.add(cmd_args(cxx_wrapper, format = "--env-cc={}"))
    args.hidden(cxx_cmd)

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
    ctx.actions.run(cmd, category = "cgo")

    return go_srcs, c_headers, c_srcs

def _compile_with_coverage(ctx: AnalysisContext, pkg_name: str, srcs: cmd_args, coverage_mode: GoCoverageMode, shared: bool = False) -> (Artifact, cmd_args):
    cov_res = cover_srcs(ctx, pkg_name, coverage_mode, srcs, shared)
    srcs = cov_res.srcs
    coverage_vars = cov_res.variables
    coverage_pkg = compile(
        ctx,
        pkg_name,
        srcs = srcs,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        coverage_mode = coverage_mode,
        shared = shared,
    )
    return (coverage_pkg, coverage_vars)

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

    # Merge all sources together to pass to the Go compile step.
    all_srcs = cmd_args(go_srcs + compiled_objects)
    if ctx.attrs.go_srcs:
        all_srcs.add(get_filtered_srcs(ctx, ctx.attrs.go_srcs))

    # Build Go library.
    static_pkg = compile(
        ctx,
        pkg_name,
        all_srcs,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        shared = False,
    )
    shared_pkg = compile(
        ctx,
        pkg_name,
        all_srcs,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        shared = True,
    )
    coverage_shared = {mode: _compile_with_coverage(ctx, pkg_name, all_srcs, mode, True) for mode in GoCoverageMode}
    coverage_static = {mode: _compile_with_coverage(ctx, pkg_name, all_srcs, mode, False) for mode in GoCoverageMode}
    pkgs = {
        pkg_name: GoPkg(
            shared = shared_pkg,
            static = static_pkg,
            cgo = True,
            coverage_shared = coverage_shared,
            coverage_static = coverage_static,
        ),
    }

    # We need to keep pre-processed cgo source files,
    # because they are required for any editing and linting (like VSCode+gopls)
    # to work with cgo. And when nearly every FB service client is cgo,
    # we need to support it well.
    return [
        DefaultInfo(default_output = static_pkg, other_outputs = go_srcs),
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
