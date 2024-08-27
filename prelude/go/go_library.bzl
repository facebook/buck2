# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//cxx:preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//linking:link_info.bzl",
    "MergedLinkInfo",
    "create_merged_link_info_for_propagation",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
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
load(":compile.bzl", "GoPkgCompileInfo", "GoTestInfo", "get_inherited_compile_pkgs")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "go_attr_pkg_name", "merge_pkgs")

def go_library_impl(ctx: AnalysisContext) -> list[Provider]:
    pkgs = {}
    pkg_name = go_attr_pkg_name(ctx)

    race = ctx.attrs._race
    asan = ctx.attrs._asan
    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None

    pkg, pkg_info = build_package(
        ctx,
        pkg_name,
        srcs = ctx.attrs.srcs,
        package_root = ctx.attrs.package_root,
        deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        compiler_flags = ctx.attrs.compiler_flags,
        assembler_flags = ctx.attrs.assembler_flags,
        race = race,
        asan = asan,
        coverage_mode = coverage_mode,
        embedcfg = ctx.attrs.embedcfg,
        # We need to set CGO_DESABLED for "pure" Go libraries, otherwise CGo files may be selected for compilation.
        force_disable_cgo = True,
    )

    default_output = pkg.pkg
    pkgs[pkg_name] = pkg

    return [
        DefaultInfo(default_output = default_output),
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
        create_linkable_graph(
            ctx,
            deps = ctx.attrs.deps,
        ),
        cxx_merge_cpreprocessors(ctx, [], cxx_inherited_preprocessor_infos(ctx.attrs.deps)),
        pkg_info,
    ]
