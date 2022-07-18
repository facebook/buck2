load("@fbcode//buck2/prelude/apple:apple_sdk.bzl", "get_apple_sdk_name")
load("@fbcode//buck2/prelude/apple:apple_target_sdk_version.bzl", "get_min_deployment_version_for_node")
load("@fbcode//buck2/prelude/apple:apple_utility.bzl", "has_apple_toolchain")
load(
    "@fbcode//buck2/prelude/cxx:compile.bzl",
    "CxxExtension",  # @unused Used as a type
    "CxxSrcWithFlags",  # @unused Used as a type
)
load("@fbcode//buck2/prelude/cxx:xcode.bzl", "cxx_populate_xcode_attributes")

def apple_populate_xcode_attributes(
        ctx,
        srcs: [CxxSrcWithFlags.type],
        argsfiles_by_ext: {CxxExtension.type: "artifact"},
        product_name: str.type) -> {str.type: ""}:
    data = cxx_populate_xcode_attributes(ctx = ctx, srcs = srcs, argsfiles_by_ext = argsfiles_by_ext, product_name = product_name)

    if has_apple_toolchain(ctx):
        data["sdk"] = get_apple_sdk_name(ctx)
        data["deployment_version"] = get_min_deployment_version_for_node(ctx)

    if hasattr(ctx.attrs, "swift_version"):
        swift_version = ctx.attrs.swift_version
        if swift_version != None:
            data["swift_version"] = swift_version

    return data
