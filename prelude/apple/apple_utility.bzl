load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@fbcode//buck2/prelude/cxx:headers.bzl", "CxxHeadersLayout", "CxxHeadersNaming")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect", "value_or")
load(":apple_target_sdk_version.bzl", "get_min_deployment_version_for_node")

_VERSION_PLACEHOLDER = "(VERSION)"

# TODO(T115177501): Make target triples part of the toolchains
# Map from SDK name -> target triple _without_ leading architecture
_TARGET_TRIPLE_MAP = {
    "iphoneos": "apple-ios{}".format(_VERSION_PLACEHOLDER),
    "iphonesimulator": "apple-ios{}-simulator".format(_VERSION_PLACEHOLDER),
    "macosx": "apple-macosx{}".format(_VERSION_PLACEHOLDER),
    "watchos": "apple-watchos{}".format(_VERSION_PLACEHOLDER),
    "watchsimulator": "apple-watchos{}-simulator".format(_VERSION_PLACEHOLDER),
}

def get_apple_cxx_headers_layout(ctx: "context") -> CxxHeadersLayout.type:
    namespace = value_or(ctx.attr.header_path_prefix, ctx.attr.name)
    return CxxHeadersLayout(namespace = namespace, naming = CxxHeadersNaming("apple"))

def get_apple_frameworks_linker_flags(ctx: "context") -> [[""]]:
    flags = []

    flags.extend(get_framework_search_path_flags(ctx))

    framework_names = [to_framework_name(x) for x in ctx.attr.frameworks]
    flags.extend([["-framework", x] for x in framework_names])

    library_names = [_library_name(x) for x in ctx.attr.libraries]
    flags.extend([["-l" + x] for x in library_names])

    return flags

def get_framework_search_path_flags(ctx: "context") -> [[""]]:
    # We don't want to include SDK directories as those are already added via `isysroot` flag in toolchain definition.
    # Adding those directly via `-F` will break building Catalyst applications as frameworks from support directory
    # won't be found and those for macOS platform will be used.
    non_sdk_framework_directories = _uniq(filter(None, [_non_sdk_framework_directory(ctx, x) for x in ctx.attr.frameworks]))
    return [["-F", x] for x in non_sdk_framework_directories]

def get_module_name(ctx: "context") -> str.type:
    return ctx.attr.module_name or ctx.attr.header_path_prefix or ctx.attr.name

def has_apple_toolchain(ctx: "context") -> bool.type:
    return hasattr(ctx.attr, "_apple_toolchain")

def to_framework_name(framework_path: str.type) -> str.type:
    name, ext = paths.split_extension(paths.basename(framework_path))
    expect(ext == ".framework", "framework `{}` missing `.framework` suffix", framework_path)
    return name

def get_versioned_target_triple(ctx: "context") -> str.type:
    apple_toolchain_info = ctx.attr._apple_toolchain[AppleToolchainInfo]
    swift_toolchain_info = apple_toolchain_info.swift_toolchain_info

    architecture = swift_toolchain_info.architecture
    if architecture == None:
        fail("Need to set `architecture` field of swift_toolchain(), target: {}".format(ctx.label))

    target_sdk_version = get_min_deployment_version_for_node(ctx) or ""

    sdk_name = apple_toolchain_info.sdk_name
    target_triple_with_version_placeholder = _TARGET_TRIPLE_MAP.get(sdk_name)
    if target_triple_with_version_placeholder == None:
        fail("Could not find target triple for sdk = {}".format(sdk_name))

    versioned_target_triple = target_triple_with_version_placeholder.replace(_VERSION_PLACEHOLDER, target_sdk_version)
    return "{}-{}".format(architecture, versioned_target_triple)

def _library_name(library: str.type) -> str.type:
    name = paths.basename(library)
    if not name.startswith("lib"):
        fail("unexpected library: {}".format(library))
    return paths.split_extension(name[3:])[0]

def expand_relative_prefixed_sdk_path(
        sdk_path: "cmd_args",
        resource_dir: "cmd_args",
        path_to_expand: str.type) -> "cmd_args":
    path_expansion_map = {
        "$RESOURCEDIR": resource_dir,
        "$SDKROOT": sdk_path,
    }
    expanded_cmd = cmd_args()
    for (path_variable, path_value) in path_expansion_map.items():
        if path_to_expand.startswith(path_variable):
            path = path_to_expand[len(path_variable):]
            if path.find("$") == 0:
                fail("Failed to expand framework path: {}".format(path))
            expanded_cmd.add(cmd_args([path_value, path], delimiter = ""))

    return expanded_cmd

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

IMPLICIT_SDKROOT_FRAMEWORK_SEARCH_PATHS = [
    "$SDKROOT/Library/Frameworks",
    "$SDKROOT/System/Library/Frameworks",
]

def _non_sdk_framework_directory(ctx: "context", framework_path: str.type) -> [str.type, None]:
    # We must only drop any framework paths that are part of the implicit
    # framework search paths in the linker + compiler, all other paths
    # must be expanded and included as part of the command.
    for implicit_search_path in IMPLICIT_SDKROOT_FRAMEWORK_SEARCH_PATHS:
        if framework_path.find(implicit_search_path) == 0:
            return None
    expanded_framework_path = _expand_sdk_framework_path(ctx, framework_path)
    return paths.dirname(expanded_framework_path)

# Filters out duplicates from input array of strings, order is not preserved
def _uniq(input: [str.type]) -> [str.type]:
    return {x: True for x in input}.keys()
