# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_groups.bzl",
    "LinkGroupLibInfo",
)
load(
    "@prelude//linking:link_info.bzl",
    "MergedLinkInfo",
    "create_merged_link_info_for_propagation",
)
load(
    "@prelude//linking:shared_libraries.bzl",
    "SharedLibraryInfo",
    "merge_shared_libraries",
)
load(
    "@prelude//utils:utils.bzl",
    "map_idx",
)
load(":compile.bzl", "GoPkgCompileInfo", "GoTestInfo", "compile", "get_filtered_srcs", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode", "cover_srcs")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":packages.bzl", "GoPkg", "go_attr_pkg_name", "merge_pkgs")

def _compile_with_coverage(ctx: AnalysisContext, pkg_name: str, srcs: cmd_args, coverage_mode: GoCoverageMode, shared: bool, race: bool) -> (Artifact, cmd_args):
    cov_res = cover_srcs(ctx, pkg_name, coverage_mode, srcs, shared)
    srcs = cov_res.srcs
    coverage_vars = cov_res.variables
    coverage_pkg = compile(
        ctx,
        pkg_name,
        srcs = srcs,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        compile_flags = ctx.attrs.compiler_flags,
        coverage_mode = coverage_mode,
        shared = shared,
        race = race,
    )
    return (coverage_pkg, coverage_vars)

def go_library_impl(ctx: AnalysisContext) -> list[Provider]:
    pkgs = {}
    default_output = None
    pkg_name = None
    if ctx.attrs.srcs:
        pkg_name = go_attr_pkg_name(ctx)

        # We need to set CGO_DESABLED for "pure" Go libraries, otherwise CGo files may be selected for compilation.
        srcs = get_filtered_srcs(ctx, ctx.attrs.srcs, force_disable_cgo = True)
        shared = ctx.attrs._compile_shared
        race = ctx.attrs._race

        compiled_pkg = compile(
            ctx,
            pkg_name,
            srcs = srcs,
            deps = ctx.attrs.deps + ctx.attrs.exported_deps,
            compile_flags = ctx.attrs.compiler_flags,
            assemble_flags = ctx.attrs.assembler_flags,
            shared = shared,
            race = race,
        )

        pkg_with_coverage = {mode: _compile_with_coverage(ctx, pkg_name, srcs, mode, shared, race = race) for mode in GoCoverageMode}

        default_output = compiled_pkg
        pkgs[pkg_name] = GoPkg(
            pkg = compiled_pkg,
            pkg_with_coverage = pkg_with_coverage,
        )

    return [
        DefaultInfo(default_output = default_output),
        LinkGroupLibInfo(libs = {}),
        GoPkgCompileInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_compile_pkgs(ctx.attrs.exported_deps),
        ])),
        GoPkgLinkInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_link_pkgs(ctx.attrs.deps + ctx.attrs.exported_deps),
        ])),
        GoTestInfo(
            deps = ctx.attrs.deps,
            srcs = ctx.attrs.srcs,
            pkg_name = pkg_name,
        ),
        create_merged_link_info_for_propagation(ctx, filter(None, [d.get(MergedLinkInfo) for d in ctx.attrs.deps])),
        merge_shared_libraries(
            ctx.actions,
            deps = filter(None, map_idx(SharedLibraryInfo, ctx.attrs.deps)),
        ),
    ]
