load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(":apple_bundle_part.bzl", "AppleBundlePart")
load(":apple_bundle_utility.bzl", "get_product_name")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(
    ":apple_sdk_metadata.bzl",
    "AppleSdkMetadata",  # @unused Used as a type
    "MacOSXSdkMetadata",
    "get_apple_sdk_metadata_for_sdk_name",
)
load(":apple_toolchain_types.bzl", "AppleToolsInfo")

def process_info_plist(ctx: "context", additional_input: ["artifact", None]) -> AppleBundlePart.type:
    input = _preprocess_info_plist(ctx)
    output = ctx.actions.declare_output("Info.plist")
    additional_keys = _additional_keys_as_json_file(ctx)
    process_plist(
        ctx = ctx,
        input = input,
        additional_input = additional_input,
        additional_keys = additional_keys,
        output = output.as_output(),
        action_id = None,
    )
    return AppleBundlePart(source = output, destination = AppleBundleDestination("metadata"))

def _preprocess_info_plist(ctx: "context") -> "artifact":
    input = ctx.attr.info_plist
    output = ctx.actions.declare_output("PreprocessedInfo.plist")
    substitutions_json = _plist_substitutions_as_json_file(ctx)
    apple_tools = ctx.attr._apple_tools[AppleToolsInfo]
    processor = apple_tools.info_plist_processor
    command = cmd_args([
        processor,
        "preprocess",
        "--input",
        input,
        "--output",
        output.as_output(),
        "--product-name",
        get_product_name(ctx),
    ])
    if substitutions_json != None:
        command.add(["--substitutions-json", substitutions_json])
    ctx.actions.run(command, category = "apple_preprocess_info_plist")
    return output

def _plist_substitutions_as_json_file(ctx: "context") -> ["artifact", None]:
    info_plist_substitutions = ctx.attr.info_plist_substitutions
    if not info_plist_substitutions:
        return None

    substitutions_json = ctx.actions.write_json("plist_substitutions.json", info_plist_substitutions)
    return substitutions_json

def process_plist(ctx: "context", input: "artifact", additional_input: ["artifact", None], additional_keys: ["artifact", None], output: "output_artifact", action_id: [str.type, None]):
    apple_tools = ctx.attr._apple_tools[AppleToolsInfo]
    processor = apple_tools.info_plist_processor
    additional_input_arguments = ["--additional-input", additional_input] if additional_input != None else []
    additional_keys_arguments = ["--additional-keys", additional_keys] if additional_keys != None else []
    command = cmd_args([
        processor,
        "process",
        "--input",
        input,
        "--output",
        output,
    ] + additional_input_arguments + additional_keys_arguments)
    ctx.actions.run(command, category = "apple_process_info_plist", identifier = action_id or input.basename)

def _additional_keys_as_json_file(ctx: "context") -> "artifact":
    additional_keys = _info_plist_additional_keys(ctx)
    return ctx.actions.write_json("plist_additional.json", additional_keys)

def _info_plist_additional_keys(ctx: "context") -> {str.type: ""}:
    sdk_name = get_apple_sdk_name(ctx)
    sdk_metadata = get_apple_sdk_metadata_for_sdk_name(sdk_name)
    result = _extra_mac_info_plist_keys(sdk_metadata, ctx.attr.extension)
    result["CFBundleSupportedPlatforms"] = sdk_metadata.info_plist_supported_platforms_values
    result["DTPlatformName"] = sdk_name
    return result

def _extra_mac_info_plist_keys(sdk_metadata: AppleSdkMetadata.type, extension: str.type) -> {str.type: ""}:
    if sdk_metadata.name == MacOSXSdkMetadata.name and extension == "xpc":
        return {
            "NSHighResolutionCapable": True,
            "NSSupportsAutomaticGraphicsSwitching": True,
        }
    else:
        return {}
