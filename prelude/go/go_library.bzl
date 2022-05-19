load(":compile.bzl", "GoPkgCompileInfo", "GoTestInfo", "compile", "get_filtered_srcs", "get_inherited_compile_pkgs")
load(":link.bzl", "GoPkgLinkInfo", "get_inherited_link_pkgs")
load(":packages.bzl", "go_attr_pkg_name", "merge_pkgs")

def go_library_impl(ctx: "context") -> ["provider"]:
    pkgs = {}
    default_outputs = []
    pkg_name = None
    if ctx.attr.srcs:
        pkg_name = go_attr_pkg_name(ctx)
        lib = compile(
            ctx,
            pkg_name,
            get_filtered_srcs(ctx, ctx.attr.srcs),
            deps = ctx.attr.deps + ctx.attr.exported_deps,
        )
        default_outputs.append(lib)
        pkgs[pkg_name] = lib

    return [
        DefaultInfo(default_outputs = default_outputs),
        GoPkgCompileInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_compile_pkgs(ctx.attr.exported_deps),
        ])),
        GoPkgLinkInfo(pkgs = merge_pkgs([
            pkgs,
            get_inherited_link_pkgs(ctx.attr.deps + ctx.attr.exported_deps),
        ])),
        GoTestInfo(
            deps = ctx.attr.deps,
            srcs = ctx.attr.srcs,
            pkg_name = pkg_name,
        ),
    ]
