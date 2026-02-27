# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
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
load(":cgo_builder.bzl", "get_cgo_build_context")
load(":compile.bzl", "GoPkgCompileInfo", "GoTestInfo")
load(":coverage.bzl", "GoCoverageMode")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":package_builder.bzl", "build_package")
load(":packages.bzl", "cgo_exported_preprocessor", "go_attr_pkg_name", "merge_pkgs")
load(":toolchain.bzl", "GoToolchainInfo", "evaluate_cgo_enabled", "get_toolchain_env_vars")

def go_library_impl(ctx: AnalysisContext) -> list[Provider]:
    cxx_toolchain_available = CxxToolchainInfo in ctx.attrs._cxx_toolchain
    pkg_import_path = go_attr_pkg_name(ctx)

    coverage_mode = GoCoverageMode(ctx.attrs._coverage_mode) if ctx.attrs._coverage_mode else None
    cgo_build_context = get_cgo_build_context(ctx)

    pkg, pkg_info = build_package(
        ctx = ctx,
        pkg_import_path = pkg_import_path,
        main = False,
        srcs = ctx.attrs.srcs + ctx.attrs.headers,
        package_root = ctx.attrs.package_root,
        cgo_build_context = cgo_build_context,
        deps = ctx.attrs.deps,
        compiler_flags = ctx.attrs.compiler_flags,
        assembler_flags = ctx.attrs.assembler_flags,
        build_tags = ctx.attrs._build_tags,
        coverage_mode = coverage_mode,
        embed_srcs = ctx.attrs.embed_srcs,
        cgo_enabled = evaluate_cgo_enabled(cxx_toolchain_available, ctx.attrs._cgo_enabled, ctx.attrs.override_cgo_enabled),
    )

    default_output = _combine_package(ctx, pkg_import_path, pkg.pkg, pkg.export_file)
    pkgs = {
        pkg_import_path: pkg,
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
            pkg_import_path = pkg_import_path,
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
        cxx_merge_cpreprocessors(ctx.actions, own_exported_preprocessors, cxx_inherited_preprocessor_infos(ctx.attrs.deps)),
        pkg_info,
    ]

def _get_empty_link_infos() -> dict[LibOutputStyle, LinkInfos]:
    infos = {}
    for output_style in LibOutputStyle:
        infos[output_style] = LinkInfos(default = LinkInfo())
    return infos

# The combined package is convinient for debugging purposes, but for actual builds we use separate objects.
def _combine_package(ctx: AnalysisContext, pkg_import_path: str, a_file: Artifact, x_file: Artifact) -> Artifact:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain)

    pkg_file = ctx.actions.declare_output(paths.basename(pkg_import_path) + "-combined.a", has_content_based_path = True)

    pack_cmd = [
        go_toolchain.packer,
        "c",
        pkg_file.as_output(),
        a_file,
        x_file,
    ]

    identifier = paths.basename(pkg_import_path) + "-combined"
    ctx.actions.run(pack_cmd, env = env, category = "go_pack", identifier = identifier)

    return pkg_file
