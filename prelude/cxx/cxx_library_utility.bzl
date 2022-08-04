load("@fbcode//buck2/prelude:paths.bzl", "paths")
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "LinkStyle",
    "Linkage",
    "MergedLinkInfo",
    "merge_link_infos",
)
load(
    "@fbcode//buck2/prelude/utils:utils.bzl",
    "expect",
    "flatten",
    "from_named_set",
    "map_idx",
    "value_or",
)
load(
    ":cxx_context.bzl",
    "CxxContext",  # @unused Used as a type
    "ctx_to_cxx_context",
    "get_cxx_toolchain_info",
)
load(
    ":headers.bzl",
    "cxx_attr_header_namespace",
)
load(
    ":linker.bzl",
    "get_shared_library_name",
)
load(":platform.bzl", "cxx_by_platform")

ARGSFILES_SUBTARGET = "argsfiles"

# The dependencies
def cxx_attr_deps(ctx: "context") -> ["dependency"]:
    return ctx.attrs.deps + flatten(cxx_by_platform(ctx_to_cxx_context(ctx), ctx.attrs.platform_deps))

def cxx_attr_exported_deps(ctx: "context") -> ["dependency"]:
    return ctx.attrs.exported_deps + flatten(cxx_by_platform(ctx_to_cxx_context(ctx), ctx.attrs.exported_platform_deps))

def cxx_attr_exported_linker_flags(ctx: "context") -> [""]:
    return (
        ctx.attrs.exported_linker_flags +
        flatten(cxx_by_platform(ctx_to_cxx_context(ctx), ctx.attrs.exported_platform_linker_flags))
    )

def cxx_attr_exported_post_linker_flags(ctx: "context") -> [""]:
    return (
        ctx.attrs.exported_post_linker_flags +
        flatten(cxx_by_platform(ctx_to_cxx_context(ctx), ctx.attrs.exported_post_platform_linker_flags))
    )

def cxx_inherited_link_info(actions: "actions", first_order_deps: ["dependency"]) -> MergedLinkInfo.type:
    # We filter out nones because some non-cxx rule without such providers could be a dependency, for example
    # cxx_binary "fbcode//one_world/cli/util/process_wrapper:process_wrapper" depends on
    # python_library "fbcode//third-party-buck/$platform/build/glibc:__project__"
    return merge_link_infos(actions, filter(None, map_idx(MergedLinkInfo, first_order_deps)))

# Linker flags
def cxx_attr_linker_flags(ctx: "context") -> [""]:
    return (
        ctx.attrs.linker_flags +
        flatten(cxx_by_platform(ctx_to_cxx_context(ctx), ctx.attrs.platform_linker_flags))
    )

def cxx_attr_link_style(ctx: "context") -> LinkStyle.type:
    if ctx.attrs.link_style != None:
        return LinkStyle(ctx.attrs.link_style)
    if ctx.attrs.defaults != None:
        # v1 equivalent code is in CxxConstructorArg::getDefaultFlavors and ParserWithConfigurableAttributes::applyDefaultFlavors
        # Only values in the map are used by v1 as flavors, copy this behavior and return the first value which is compatible with link style.
        v1_flavors = ctx.attrs.defaults.values()
        for s in [LinkStyle("static"), LinkStyle("static_pic"), LinkStyle("shared")]:
            if s.value in v1_flavors:
                return s
    return get_cxx_toolchain_info(ctx).linker_info.link_style

def cxx_attr_preferred_linkage(ctx: "context") -> Linkage.type:
    preferred_linkage = ctx.attrs.preferred_linkage

    # force_static is deprecated, but it has precedence over preferred_linkage
    if getattr(ctx.attrs, "force_static", False):
        preferred_linkage = "static"

    return Linkage(preferred_linkage)

def cxx_attr_resources(ctx: "context") -> {str.type: ("artifact", ["_arglike"])}:
    """
    Return the resources provided by this rule, as a map of resource name to
    a tuple of the resource artifact and any "other" outputs exposed by it.
    """

    resources = {}
    namespace = cxx_attr_header_namespace(ctx)

    # Use getattr, as apple rules don't have a `resources` parameter.
    for name, resource in from_named_set(getattr(ctx.attrs, "resources", {})).items():
        if type(resource) == "artifact":
            other = []
        else:
            info = resource[DefaultInfo]
            expect(
                len(info.default_outputs) == 1,
                "expected exactly one default output from {} ({})"
                    .format(resource, info.default_outputs),
            )
            [resource] = info.default_outputs
            other = info.other_outputs
        resources[paths.join(namespace, name)] = (resource, other)

    return resources

def cxx_mk_shlib_intf(
        cxx_context: CxxContext.type,
        name: str.type,
        shared_lib: "artifact") -> "artifact":
    """
    Convert the given shared library into an interface used for linking.
    """
    linker_info = cxx_context.cxx_toolchain_info.linker_info
    args = cmd_args(linker_info.mk_shlib_intf[RunInfo])
    args.add(shared_lib)
    output = cxx_context.actions.declare_output(
        get_shared_library_name(linker_info.type, name + "-interface"),
    )
    args.add(output.as_output())
    cxx_context.actions.run(args, category = "generate_shared_library_interface")
    return output

def cxx_is_gnu(cxx_context: CxxContext.type) -> bool.type:
    return cxx_context.cxx_toolchain_info.linker_info.type == "gnu"

def cxx_use_link_groups(ctx: "context") -> bool.type:
    # Link groups is enabled by default in darwin
    cxx_context = ctx_to_cxx_context(ctx)
    return cxx_is_gnu(cxx_context) and value_or(ctx.attrs.use_link_groups, False)

def cxx_use_shlib_intfs(cxx_context: CxxContext.type) -> bool.type:
    """
    Return whether we should use shared library interfaces for linking.
    """
    linker_info = cxx_context.cxx_toolchain_info.linker_info

    # TODO(T110378128): Apple currently uses the same configuration as fbcode
    # platforms, so only explicitly enable for linux until this is fixed.
    return linker_info.shlib_interfaces != "disabled" and linker_info.type == "gnu"

def cxx_platform_supported(ctx: "context", cxx_context: CxxContext.type) -> bool.type:
    """
    Return whether this rule's `supported_platforms_regex` matches the current
    platform name.
    """

    if ctx.attrs.supported_platforms_regex == None:
        return True

    return regex_match(
        ctx.attrs.supported_platforms_regex,
        cxx_context.cxx_platform_info.name,
    )
