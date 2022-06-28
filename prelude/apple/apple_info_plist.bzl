load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load(":apple_bundle_destination.bzl", "AppleBundleDestination")
load(":apple_bundle_part.bzl", "AppleBundlePart")
load(":apple_bundle_utility.bzl", "get_product_name")

def process_info_plist(ctx: "context", additional_input: ["artifact", None]) -> AppleBundlePart.type:
    input = _preprocess_info_plist(ctx)
    output = ctx.actions.declare_output("Info.plist")
    process_plist(ctx, input, additional_input, output.as_output(), None)
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

def process_plist(ctx: "context", input: "artifact", additional_input: ["artifact", None], output: "output_artifact", action_id: [str.type, None]):
    apple_tools = ctx.attr._apple_tools[AppleToolsInfo]
    processor = apple_tools.info_plist_processor
    command = cmd_args([
        processor,
        "process",
        "--input",
        input,
        "--output",
        output,
    ] + (
        ["--additional-input", additional_input] if additional_input != None else []
    ))
    ctx.actions.run(command, category = "apple_process_info_plist", identifier = action_id or input.basename)
