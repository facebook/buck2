load(
    "@prelude//linking:link_info.bzl",
    "MergedLinkInfo",
    "merge_link_infos",
)
load(":compile.bzl", "GoPkgCompileInfo", "GoTestInfo", "compile", "get_filtered_srcs", "get_inherited_compile_pkgs")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":packages.bzl", "go_attr_pkg_name", "merge_pkgs")

def go_library_impl(ctx: "context") -> ["provider"]:
    pkgs = {}
    default_outputs = []
    pkg_name = None
    if ctx.attrs.srcs:
        pkg_name = go_attr_pkg_name(ctx)
        lib = compile(
            ctx,
            pkg_name,
            get_filtered_srcs(ctx, ctx.attrs.srcs),
            deps = ctx.attrs.deps + ctx.attrs.exported_deps,
        )
        default_outputs.append(lib)
        pkgs[pkg_name] = lib

    return [
        DefaultInfo(default_outputs = default_outputs),
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
        merge_link_infos(ctx, filter(None, [d[MergedLinkInfo] for d in ctx.attrs.deps])),
    ]
