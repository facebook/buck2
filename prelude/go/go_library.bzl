# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//cxx:preprocessor.bzl",
    "cxx_inherited_preprocessor_infos",
    "cxx_merge_cpreprocessors",
)
load(
    "@prelude//linking:link_groups.bzl",
    "merge_link_group_lib_info",
)
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "MergedLinkInfo",
    "create_merged_link_info_for_propagation",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
    "create_linkable_graph_node",
    "create_linkable_node",
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
load(":compile.bzl", "GoPkgCompileInfo", "GoTestInfo")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "cgo_exported_preprocessor", "go_attr_pkg_name", "merge_pkgs")
load(":toolchain.bzl", "evaluate_cgo_enabled")

def go_library_impl(ctx: AnalysisContext) -> list[Provider]:
    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    pkg_name = go_attr_pkg_name(ctx)

    race = ctx.attrs._race
    asan = ctx.attrs._asan
    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None

    pkg, pkg_info = build_package(
        ctx = ctx,
        pkg_name = pkg_name,
        main = False,
        srcs = ctx.attrs.srcs + ctx.attrs.headers,
        package_root = ctx.attrs.package_root,
        deps = ctx.attrs.deps,
        compiler_flags = ctx.attrs.compiler_flags,
        assembler_flags = ctx.attrs.assembler_flags,
        build_tags = ctx.attrs._build_tags,
        race = race,
        asan = asan,
        coverage_mode = coverage_mode,
        embedcfg = ctx.attrs.embedcfg,
        cgo_enabled = evaluate_cgo_enabled(cxx_toolchain_available, ctx.attrs._cgo_enabled, ctx.attrs.override_cgo_enabled),
    )

    default_output = pkg.pkg
    pkgs = {
        pkg_name: pkg,
    }

    own_exported_preprocessors = [cgo_exported_preprocessor(ctx, pkg_info)] if ctx.attrs.generate_exported_header else []

    return [
        DefaultInfo(default_output = default_output),
        GoPkgCompileInfo(pkgs = pkgs),
        GoPkgLinkInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_link_pkgs(ctx.attrs.deps),
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
        merge_link_group_lib_info(deps = ctx.attrs.deps),
        create_linkable_graph(
            ctx,
            # Linkable graph nodes must be present for link_groups operation (even if there are nothing to provide).
            # Borrowing the this approach from Ocaml.
            # Though his doesn't look entirely right.
            node = create_linkable_graph_node(
                ctx,
                linkable_node = create_linkable_node(
                    ctx,
                    default_soname = None,
                    exported_deps = ctx.attrs.deps,
                    link_infos = _get_empty_link_infos(),
                ),
            ),
            deps = ctx.attrs.deps,
        ),
        cxx_merge_cpreprocessors(ctx, own_exported_preprocessors, cxx_inherited_preprocessor_infos(ctx.attrs.deps)),
        pkg_info,
    ]

def _get_empty_link_infos() -> dict[LibOutputStyle, LinkInfos]:
    infos = {}
    for output_style in LibOutputStyle:
        infos[output_style] = LinkInfos(default = LinkInfo())
    return infos
