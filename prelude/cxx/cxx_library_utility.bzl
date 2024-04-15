# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifacts.bzl",
    "ArtifactOutputs",  # @unused Used as a type
    "single_artifact",
)
load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//linking:link_info.bzl",
    "LinkStrategy",
    "LinkStyle",
    "LinkerFlags",
    "MergedLinkInfo",
)
load("@prelude//linking:types.bzl", "Linkage")
load(
    "@prelude//utils:utils.bzl",
    "flatten",
    "from_named_set",
)
load(":cxx_context.bzl", "get_cxx_platform_info", "get_cxx_toolchain_info")
load(":cxx_toolchain_types.bzl", "ShlibInterfacesMode")
load(
    ":headers.bzl",
    "cxx_attr_header_namespace",
)
load(":platform.bzl", "cxx_by_platform")

OBJECTS_SUBTARGET = "objects"

# The dependencies
def cxx_attr_deps(ctx: AnalysisContext) -> list[Dependency]:
    return (
        ctx.attrs.deps +
        flatten(cxx_by_platform(ctx, getattr(ctx.attrs, "platform_deps", []))) +
        (getattr(ctx.attrs, "deps_query", []) or [])
    )

def cxx_attr_exported_deps(ctx: AnalysisContext) -> list[Dependency]:
    return ctx.attrs.exported_deps + flatten(cxx_by_platform(ctx, ctx.attrs.exported_platform_deps))

def cxx_attr_linker_flags_all(ctx: AnalysisContext) -> LinkerFlags:
    flags = cxx_attr_linker_flags(ctx)
    post_flags = (
        (ctx.attrs.post_linker_flags if hasattr(ctx.attrs, "post_linker_flags") else []) +
        (flatten(cxx_by_platform(ctx, ctx.attrs.post_platform_linker_flags)) if hasattr(ctx.attrs, "post_platform_linker_flags") else [])
    )
    exported_flags = cxx_attr_exported_linker_flags(ctx)
    exported_post_flags = cxx_attr_exported_post_linker_flags(ctx)
    return LinkerFlags(
        flags = flags,
        post_flags = post_flags,
        exported_flags = exported_flags,
        exported_post_flags = exported_post_flags,
    )

def cxx_attr_exported_linker_flags(ctx: AnalysisContext) -> list[typing.Any]:
    return (
        ctx.attrs.exported_linker_flags +
        (flatten(cxx_by_platform(ctx, ctx.attrs.exported_platform_linker_flags)) if hasattr(ctx.attrs, "exported_platform_linker_flags") else [])
    )

def cxx_attr_exported_post_linker_flags(ctx: AnalysisContext) -> list[typing.Any]:
    return (
        ctx.attrs.exported_post_linker_flags +
        (flatten(cxx_by_platform(ctx, ctx.attrs.exported_post_platform_linker_flags)) if hasattr(ctx.attrs, "exported_post_platform_linker_flags") else [])
    )

def cxx_inherited_link_info(first_order_deps: list[Dependency]) -> list[MergedLinkInfo]:
    """
    Returns the list of MergedLinkInfo from the dependencies, filtering out those without a MergedLinkInfo
    """

    # We filter out nones because some non-cxx rule without such providers could be a dependency, for example
    # cxx_binary "fbcode//one_world/cli/util/process_wrapper:process_wrapper" depends on
    # python_library "fbcode//third-party-buck/$platform/build/glibc:__project__"
    return filter(None, [x.get(MergedLinkInfo) for x in first_order_deps])

# Linker flags
def cxx_attr_linker_flags(ctx: AnalysisContext) -> list[typing.Any]:
    return (
        ctx.attrs.linker_flags +
        (flatten(cxx_by_platform(ctx, ctx.attrs.platform_linker_flags)) if hasattr(ctx.attrs, "platform_linker_flags") else [])
    )

# Even though we're returning the shared library links, we must still
# respect the `link_style` attribute of the target which controls how
# all deps get linked. For example, you could be building the shared
# output of a library which has `link_style = "static"`.
#
# The fallback equivalent code in Buck v1 is in CxxLibraryFactor::createBuildRule()
# where link style is determined using the `linkableDepType` variable.

# Note if `static` link style is requested, we assume `static_pic`
# instead, so that code in the shared library can be correctly
# loaded in the address space of any process at any address.
def cxx_attr_link_strategy(attrs: typing.Any) -> LinkStrategy:
    value = attrs.link_style if attrs.link_style != None else "shared"
    if value == "static":
        value = "static_pic"
    return LinkStrategy(value)

def cxx_attr_link_style(ctx: AnalysisContext) -> LinkStyle:
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

def cxx_attr_preferred_linkage(ctx: AnalysisContext) -> Linkage:
    preferred_linkage = ctx.attrs.preferred_linkage

    # force_static is deprecated, but it has precedence over preferred_linkage
    if getattr(ctx.attrs, "force_static", False):
        preferred_linkage = "static"

    return Linkage(preferred_linkage)

def cxx_attr_resources(ctx: AnalysisContext) -> dict[str, ArtifactOutputs]:
    """
    Return the resources provided by this rule, as a map of resource name to
    a tuple of the resource artifact and any "other" outputs exposed by it.
    """

    resources = {}
    namespace = cxx_attr_header_namespace(ctx)

    # Use getattr, as apple rules don't have a `resources` parameter.
    for name, resource in from_named_set(getattr(ctx.attrs, "resources", {})).items():
        resources[paths.join(namespace, name)] = single_artifact(resource)

    return resources

def cxx_is_gnu(ctx: AnalysisContext) -> bool:
    return get_cxx_toolchain_info(ctx).linker_info.type == "gnu"

def cxx_use_shlib_intfs(ctx: AnalysisContext) -> bool:
    """
    Return whether we should use shared library interfaces for linking.
    """

    # Per-rule opt-out.
    if not getattr(ctx.attrs, "supports_shlib_interfaces", True):
        return False

    linker_info = get_cxx_toolchain_info(ctx).linker_info
    return linker_info.shlib_interfaces != ShlibInterfacesMode("disabled")

def cxx_use_shlib_intfs_mode(ctx: AnalysisContext, mode: ShlibInterfacesMode) -> bool:
    """
    Verify we are using a specific shared library interface mode.
    """
    return cxx_use_shlib_intfs(ctx) and get_cxx_toolchain_info(ctx).linker_info.shlib_interfaces == mode

def cxx_platform_supported(ctx: AnalysisContext) -> bool:
    """
    Return whether this rule's `supported_platforms_regex` matches the current
    platform name.
    """

    if ctx.attrs.supported_platforms_regex == None:
        return True

    return regex_match(
        ctx.attrs.supported_platforms_regex,
        get_cxx_platform_info(ctx).name,
    )
