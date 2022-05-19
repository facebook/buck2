load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load(":apple_bundle_destination.bzl", "AppleBundleDestination", "bundle_relative_path_for_destination")
load(":apple_bundle_utility.bzl", "get_extension_attr", "get_product_name")
load(":apple_code_signing_types.bzl", "AppleEntitlementsInfo", "CodeSignType")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(":apple_sdk_metadata.bzl", "get_apple_sdk_metadata_for_sdk_name")

# Defines where and what should be copied into
AppleBundlePart = record(
    # A file of directory which content should be copied
    source = field("artifact"),
    # Where the source should be copied, the actual destination directory
    # inside bundle depends on target platform
    destination = AppleBundleDestination.type,
    # New file name if it should be renamed before copying.
    # Only applicable when `source` is a file.
    new_name = field([str.type, None], None),
    # Marks parts which should be code signed separately from the whole bundle.
    codesign_on_copy = field(bool.type, False),
)

def bundle_output(ctx: "context") -> "artifact":
    bundle_dir_name = _get_bundle_dir_name(ctx)
    output = ctx.actions.declare_output(bundle_dir_name)
    return output

def assemble_bundle(ctx: "context", bundle: "artifact", parts: ["AppleBundlePart"], info_plist_part: ["AppleBundlePart", None]) -> None:
    all_parts = parts + [info_plist_part] if info_plist_part else []
    spec_file = _bundle_spec_json(ctx, all_parts)

    tools = ctx.attr._apple_tools[AppleToolsInfo]
    tool = tools.assemble_bundle

    codesign_args = []
    codesign_type = _detect_codesign_type(ctx)

    if ctx.attr.extension not in ["app", "appex"]:
        # Only code sign application bundles and extensions
        pass
    elif codesign_type.value in ["distribution", "adhoc"]:
        codesign_args = ["--codesign"]

        external_name = get_apple_sdk_name(ctx)
        platform_args = ["--platform", external_name]
        codesign_args.extend(platform_args)

        if codesign_type.value != "adhoc":
            provisioning_profiles = ctx.attr._provisioning_profiles[DefaultInfo]
            provisioning_profiles_args = ["--profiles-dir"] + provisioning_profiles.default_outputs
            codesign_args.extend(provisioning_profiles_args)

            # TODO(T116604880): use `apple.use_entitlements_when_adhoc_code_signing` buckconfig value
            maybe_entitlements = _entitlements_file(ctx)
            entitlements_args = ["--entitlements", maybe_entitlements] if maybe_entitlements else []
            codesign_args.extend(entitlements_args)
        else:
            codesign_args.append("--ad-hoc")

        info_plist_args = ["--info-plist", _bundle_relative_destination_path(ctx, info_plist_part)] if info_plist_part else []
        codesign_args.extend(info_plist_args)
    elif codesign_type.value == "skip":
        pass
    else:
        fail("Code sign type `{}` not supported".format(codesign_type))

    command = cmd_args([
        tool,
        "--output",
        bundle.as_output(),
        "--spec",
        spec_file,
    ] + codesign_args)
    command.hidden([part.source for part in all_parts])
    ctx.actions.run(command, local_only = True, category = "apple_assemble_bundle")

def _get_bundle_dir_name(ctx: "context") -> str.type:
    return paths.replace_extension(get_product_name(ctx), "." + get_extension_attr(ctx))

def _bundle_relative_destination_path(ctx: "context", part: "AppleBundlePart") -> str.type:
    bundle_relative_path = bundle_relative_path_for_destination(part.destination, get_apple_sdk_name(ctx), ctx.attr.extension)
    destination_file_or_directory_name = part.new_name if part.new_name != None else paths.basename(part.source.short_path)
    return paths.join(bundle_relative_path, destination_file_or_directory_name)

# Returns JSON to be passed into bundle assembling tool. It should contain a dictionary which maps bundle relative destination paths to source paths."
def _bundle_spec_json(ctx: "context", parts: ["AppleBundlePart"]) -> "artifact":
    specs = []

    for part in parts:
        part_spec = {
            "dst": _bundle_relative_destination_path(ctx, part),
            "src": part.source,
        }
        if part.codesign_on_copy:
            part_spec["codesign_on_copy"] = True
        specs.append(part_spec)

    return ctx.actions.write_json("bundle_spec.json", specs)

def _detect_codesign_type(ctx: "context") -> CodeSignType.type:
    if ctx.attr._codesign_type:
        return CodeSignType(ctx.attr._codesign_type)
    sdk_name = get_apple_sdk_name(ctx)
    is_ad_hoc_sufficient = get_apple_sdk_metadata_for_sdk_name(sdk_name).is_ad_hoc_code_sign_sufficient
    return CodeSignType("adhoc" if is_ad_hoc_sufficient else "distribution")

def _entitlements_file(ctx: "context") -> ["artifact", None]:
    if not ctx.attr.binary:
        return None

    # The `binary` attribute can be either an apple_binary or a dynamic library from apple_library
    binary_entitlement_info = ctx.attr.binary[AppleEntitlementsInfo]
    if binary_entitlement_info and binary_entitlement_info.entitlements_file:
        return binary_entitlement_info.entitlements_file

    return ctx.attr._codesign_entitlements
