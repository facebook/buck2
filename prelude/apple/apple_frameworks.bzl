load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")

_IMPLICIT_SDKROOT_FRAMEWORK_SEARCH_PATHS = [
    "$SDKROOT/Library/Frameworks",
    "$SDKROOT/System/Library/Frameworks",
]

def get_apple_frameworks_linker_flags(ctx: "context") -> [""]:
    flags = get_framework_search_path_flags(ctx)

    framework_names = [to_framework_name(x) for x in ctx.attr.frameworks]
    for framework_name in framework_names:
        flags.extend(["-framework", framework_name])

    library_names = [_library_name(x) for x in ctx.attr.libraries]
    for library_name in library_names:
        flags.extend(["-l" + library_name])

    return flags

def get_framework_search_path_flags(ctx: "context") -> [""]:
    # We don't want to include SDK directories as those are already added via `isysroot` flag in toolchain definition.
    # Adding those directly via `-F` will break building Catalyst applications as frameworks from support directory
    # won't be found and those for macOS platform will be used.
    flags = []
    non_sdk_framework_directories = dedupe(filter(None, [_non_sdk_framework_directory(ctx, x) for x in ctx.attr.frameworks]))
    for directory in non_sdk_framework_directories:
        flags.extend(["-F", directory])

    return flags

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
