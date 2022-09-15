load("@prelude//apple:apple_sdk.bzl", "get_apple_sdk_name")
load("@prelude//apple:apple_target_sdk_version.bzl", "get_min_deployment_version_for_node")
load("@prelude//apple:apple_utility.bzl", "has_apple_toolchain")
load(
    "@prelude//cxx:compile.bzl",
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@prelude//cxx:xcode.bzl", "cxx_populate_xcode_attributes")

def _add_label_for_field(ctx: "context", field_name: str.type, attrib_name: str.type, attribs: {str.type: ""}):
    if hasattr(ctx.attrs, field_name):
        dep = getattr(ctx.attrs, field_name)
        default_info = dep[DefaultInfo]
        if len(default_info.default_outputs) > 0:
            # When there's no xctoolchain (i.e., non-Pika), there will be an empty `DefaultInfo`.
            # So, an emmpty `DefaultInfo` basically signifies that there's no xctoolchain.
            attribs[attrib_name] = dep.label

def apple_xcode_attributes_add_xctoolchain(ctx: "context", attribs: {str.type: ""}):
    _add_label_for_field(ctx, "_apple_xctoolchain_bundle_id", "xctoolchain_bundle_id_target", attribs)
    _add_label_for_field(ctx, "_apple_xctoolchain", "xctoolchain_bundle_target", attribs)

def apple_populate_xcode_attributes(
        ctx,
        srcs: [CxxSrcWithFlags.type],
        argsfiles_by_ext: {str.type: "artifact"},
        product_name: str.type) -> {str.type: ""}:
    data = cxx_populate_xcode_attributes(ctx = ctx, srcs = srcs, argsfiles_by_ext = argsfiles_by_ext, product_name = product_name)

    if has_apple_toolchain(ctx):
        data["sdk"] = get_apple_sdk_name(ctx)
        data["deployment_version"] = get_min_deployment_version_for_node(ctx)

    if hasattr(ctx.attrs, "swift_version"):
        swift_version = ctx.attrs.swift_version
        if swift_version != None:
            data["swift_version"] = swift_version

    apple_xcode_attributes_add_xctoolchain(ctx, data)
    return data
