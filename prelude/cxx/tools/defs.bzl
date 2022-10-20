load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")
load(
    "@prelude//cxx:omnibus.bzl",
    "OmnibusEnvironment",
    "all_deps",
    "create_dummy_omnibus",
    "do_not_inject_omnibus_environment_transition",
    "get_excluded",
    "get_omnibus_graph",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "create_linkable_graph",
)

def _omnibus_environment_impl(ctx: "context"):
    omnibus = create_dummy_omnibus(ctx)

    # Figure out what transitive exclusions are induced by our dynamic_exclusions.
    linkable_graph = create_linkable_graph(
        ctx,
        deps = ctx.attrs.dynamic_exclusions,
    )

    omnibus_graph = get_omnibus_graph(
        graph = linkable_graph,
        roots = {},
        excluded = get_excluded(deps = ctx.attrs.dynamic_exclusions),
    )

    dynamic_exclusions = all_deps(omnibus_graph.nodes, omnibus_graph.excluded.keys())

    force_hybrid_links = (sha256(ctx.attrs.sandcastle_alias) < "2") if ctx.attrs.sandcastle_alias else False

    return [DefaultInfo(), OmnibusEnvironment(
        dummy_omnibus = omnibus,
        exclusions = {e.raw_target(): None for e in ctx.attrs.exclusions + dynamic_exclusions},
        roots = {e.raw_target(): None for e in ctx.attrs.roots},
        enable_explicit_roots = ctx.attrs.enable_explicit_roots,
        prefer_stripped_objects = ctx.attrs.prefer_stripped_native_objects,
        shared_root_ld_flags = ctx.attrs.shared_root_ld_flags,
        force_hybrid_links = force_hybrid_links,
    )]

omnibus_environment = rule(impl = _omnibus_environment_impl, attrs = {
    "dynamic_exclusions": attrs.list(attrs.transition_dep(cfg = do_not_inject_omnibus_environment_transition)),
    "enable_explicit_roots": attrs.bool(),
    "exclusions": attrs.list(attrs.label(), default = []),
    "labels": attrs.list(attrs.string(), default = []),
    # Same name as the Python attr
    "prefer_stripped_native_objects": attrs.bool(),
    "roots": attrs.list(attrs.label(), default = []),
    "sandcastle_alias": attrs.option(attrs.string()),
    "shared_root_ld_flags": attrs.list(attrs.arg(), default = []),
    "_cxx_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:cxx", providers = [CxxToolchainInfo, CxxPlatformInfo])),
})

def _cxx_hacks_impl(_ctx):
    return [DefaultInfo(), TemplatePlaceholderInfo(
        unkeyed_variables = {
            "cxx-header-tree": "/dev/null/HACK-CXX-HEADER-TREE",
            "output-dwo-dir": "/dev/null/HACK-OUTPUT-DWO-DIR",
        },
    )]

cxx_hacks = rule(
    impl = _cxx_hacks_impl,
    attrs = {},
)
