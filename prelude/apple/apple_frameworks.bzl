load("@fbcode//buck2/prelude:paths.bzl", "paths")
load(
    "@fbcode//buck2/prelude/linking:link_info.bzl",
    "FrameworksLinkable",
    "LinkArgs",
    "LinkInfo",
    "LinkInfos",
    "LinkInfosTSet",
    "LinkableType",
    "get_link_args",
    "merge_framework_linkables",
)
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")

_IMPLICIT_SDKROOT_FRAMEWORK_SEARCH_PATHS = [
    "$SDKROOT/Library/Frameworks",
    "$SDKROOT/System/Library/Frameworks",
]

def create_frameworks_linkable(ctx: "context") -> [FrameworksLinkable.type, None]:
    if not ctx.attr.libraries and not ctx.attr.frameworks:
        return None

    return FrameworksLinkable(
        library_names = [_library_name(x) for x in ctx.attr.libraries],
        resolved_framework_paths = _get_non_sdk_framework_directories(ctx, ctx.attr.frameworks),
        framework_names = [to_framework_name(x) for x in ctx.attr.frameworks],
    )

def _get_apple_frameworks_linker_flags(linkable: [FrameworksLinkable.type, None]) -> [""]:
    if not linkable:
        return []

    flags = _get_framework_search_path_flags(linkable.resolved_framework_paths)

    for framework_name in linkable.framework_names:
        flags.extend(["-framework", framework_name])

    for library_name in linkable.library_names:
        flags.extend(["-l" + library_name])

    return flags

def get_framework_search_path_flags(ctx: "context") -> [""]:
    return _get_framework_search_path_flags(_get_non_sdk_framework_directories(ctx, ctx.attr.frameworks))

def _get_framework_search_path_flags(frameworks: [""]) -> [""]:
    flags = []
    for directory in frameworks:
        flags.extend(["-F", directory])

    return flags

def _get_non_sdk_framework_directories(ctx: "context", frameworks: [""]) -> [""]:
    # We don't want to include SDK directories as those are already added via `isysroot` flag in toolchain definition.
    # Adding those directly via `-F` will break building Catalyst applications as frameworks from support directory
    # won't be found and those for macOS platform will be used.
    return dedupe(filter(None, [_non_sdk_framework_directory(ctx, x) for x in frameworks]))

def to_framework_name(framework_path: str.type) -> str.type:
    name, ext = paths.split_extension(paths.basename(framework_path))
    expect(ext == ".framework", "framework `{}` missing `.framework` suffix", framework_path)
    return name

def _library_name(library: str.type) -> str.type:
    name = paths.basename(library)
    if not name.startswith("lib"):
        fail("unexpected library: {}".format(library))
    return paths.split_extension(name[3:])[0]

def _expand_sdk_framework_path(ctx: "context", framework_path: str.type) -> str.type:
    apple_toolchain_info = ctx.attr._apple_toolchain[AppleToolchainInfo]
    path_expansion_map = {
        "$PLATFORM_DIR": apple_toolchain_info.platform_path,
        "$SDKROOT": apple_toolchain_info.sdk_path,
    }

    expanded_path = framework_path
    for (path_variable, path_value) in path_expansion_map.items():
        expanded_path = expanded_path.replace(path_variable, path_value)

    if expanded_path.find("$") == 0:
        fail("Failed to expand framework path: {}".format(expanded_path))

    return expanded_path

def _non_sdk_framework_directory(ctx: "context", framework_path: str.type) -> [str.type, None]:
    # We must only drop any framework paths that are part of the implicit
    # framework search paths in the linker + compiler, all other paths
    # must be expanded and included as part of the command.
    for implicit_search_path in _IMPLICIT_SDKROOT_FRAMEWORK_SEARCH_PATHS:
        if framework_path.find(implicit_search_path) == 0:
            return None
    expanded_framework_path = _expand_sdk_framework_path(ctx, framework_path)
    return paths.dirname(expanded_framework_path)

def build_link_args_with_deduped_framework_flags(
        ctx: "context",
        info: "MergedLinkInfo",
        frameworks_linkable: ["FrameworksLinkable", None],
        link_style: "LinkStyle",
        prefer_stripped: bool.type = False) -> LinkArgs.type:
    frameworks_link_info = _link_info_from_frameworks_linkable([info.frameworks[link_style], frameworks_linkable])
    if not frameworks_link_info:
        return get_link_args(info, link_style, prefer_stripped)

    return LinkArgs(
        tset = (ctx.actions.tset(
            LinkInfosTSet,
            value = LinkInfos(default = frameworks_link_info, stripped = frameworks_link_info),
            children = [info._infos[link_style]],
        ), prefer_stripped),
    )

def get_frameworks_link_info_by_deduping_link_infos(
        infos: [[LinkInfo.type, None]],
        framework_linkable: [FrameworksLinkable.type, None]) -> [LinkInfo.type, None]:
    # When building a framework or executable, all frameworks used by the statically-linked
    # deps in the subtree need to be linked.
    #
    # Without deduping, we've seen the linking step fail because the argsfile
    # exceeds the acceptable size by the linker.
    framework_linkables = _extract_framework_linkables(infos)
    if framework_linkable:
        framework_linkables.append(framework_linkable)

    return _link_info_from_frameworks_linkable(framework_linkables)

def _extract_framework_linkables(link_infos: [[LinkInfo.type], None]) -> [FrameworksLinkable.type]:
    frameworks_type = LinkableType("frameworks")

    linkables = []
    for info in link_infos:
        for linkable in info.linkables:
            if linkable._type == frameworks_type:
                linkables.append(linkable)

    return linkables

def _link_info_from_frameworks_linkable(framework_linkables: [[FrameworksLinkable.type, None]]) -> [LinkInfo.type, None]:
    framework_link_args = _get_apple_frameworks_linker_flags(merge_framework_linkables(framework_linkables))
    return LinkInfo(
        pre_flags = framework_link_args,
    ) if framework_link_args else None
