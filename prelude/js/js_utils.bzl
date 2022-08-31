load("@prelude//:paths.bzl", "paths")
load("@prelude//:worker_tool.bzl", "WorkerToolInfo")
load("@prelude//js:js_providers.bzl", "JsToolchainInfo")
load("@prelude//utils:utils.bzl", "expect")

RAM_BUNDLE_TYPES = {
    "": "",
    "rambundle-indexed": "--indexed-rambundle",
}

TRANSFORM_PROFILES = ["transform-profile-default", "hermes-stable", "hermes-canary"]

# Matches the default value for resolver.assetExts in metro-config
ASSET_EXTENSIONS = [
    # Image formats
    "bmp",
    "gif",
    "jpg",
    "jpeg",
    "png",
    "psd",
    "svg",
    "webp",
    # Video formats
    "m4v",
    "mov",
    "mp4",
    "mpeg",
    "mpg",
    "webm",
    # Audio formats
    "aac",
    "aiff",
    "caf",
    "m4a",
    "mp3",
    "wav",
    # Document formats
    "html",
    "pdf",
    "yaml",
    "yml",
    # Font formats
    "otf",
    "ttf",
    # Archives (virtual files)
    "zip",
]

# Matches the default value for resolver.platforms in metro-config
ASSET_PLATFORMS = ["ios", "android", "windows", "web"]

def _strip_platform_from_asset_name(name: str.type) -> str.type:
    name_without_extension, extension = paths.split_extension(name)
    return name_without_extension if extension[1:] in ASSET_PLATFORMS else name

def _strip_scale_from_asset_name(name: str.type) -> str.type:
    scale_start = -1
    for i in range(len(name)):
        char = name[i]
        if scale_start != -1:
            if char == "x":
                return name[:scale_start] + name[i + 1:]
            if char.isdigit() or char == ".":
                continue
            fail("Invalid format for scale of asset {}!".format(name))
        if name[i] == "@":
            scale_start = i

    expect(scale_start == -1, "Found scale_start but not its end {}!".format(name))

    return name

def get_canonical_src_name(src: str.type) -> str.type:
    basename, extension = paths.split_extension(src)
    if extension[1:] not in ASSET_EXTENSIONS:
        return src

    basename = _strip_platform_from_asset_name(basename)
    basename = _strip_scale_from_asset_name(basename)

    return basename + extension

def get_flavors(ctx: "context") -> [str.type]:
    flavors = [ctx.attrs._platform]
    if ctx.attrs._is_release:
        flavors.append("release")

    return flavors

def get_bundle_name(ctx: "context", default_bundle_name: str.type) -> str.type:
    bundle_name_for_flavor_map = {key: value for key, value in ctx.attrs.bundle_name_for_flavor}
    flavors = bundle_name_for_flavor_map.keys()
    for flavor in flavors:
        expect(
            flavor == "android" or flavor == "ios",
            "Currently only support picking bundle name by platform!",
        )

    platform = ctx.attrs._platform
    if platform in flavors:
        return bundle_name_for_flavor_map[platform]
    else:
        return default_bundle_name

def run_worker_command(
        ctx: "context",
        worker_tool: "dependency",
        command_args_file: "artifact",
        identifier: str.type,
        category: str.type,
        hidden_artifacts = "cmd_args"):
    worker_tool_info = worker_tool[WorkerToolInfo]
    worker_command = worker_tool_info.command.copy()
    worker_command.add("--command-args-file", command_args_file)
    worker_command.hidden(hidden_artifacts)

    ctx.actions.run(
        worker_command,
        category = category.replace("-", "_"),
        identifier = identifier,
    )

def fixup_command_args(ctx: "context", args_file: "artifact") -> "artifact":
    """
    Context: D36482836 + T120448845

    Before the existence of ctx.actions.write_json() we were manually creating
    JSON objects via cmd_args, and it was causing heavy regressions to
    heap allocations in iOS/Android Apps.

    Unfortunately, when we migrated to ctx.actions.write_json() it didn't support
    encoding JSON strings as a JSON object. Eventually, T120448845
    was filed because JS bundling in Facebook for Android broke.

    In D36482836 you can see the many different attempts to fix this problem,
    but the most sure-fire route was building a script which takes in a JSON
    file, fixes it, and outputs the JSON file to a new artifact.

    T121096376: Tracks the long-term solution for this 'hack'.
    """
    output = ctx.actions.declare_output("fixup_{}".format(args_file.short_path))
    ctx.actions.run(
        cmd_args([
            ctx.attrs._js_toolchain[JsToolchainInfo].command_args_fixup[RunInfo],
            args_file,
            output.as_output(),
        ]),
        category = "fixup_command_args",
        identifier = "{}.{}".format(args_file.short_path, ctx.attrs.name),
    )
    return output
