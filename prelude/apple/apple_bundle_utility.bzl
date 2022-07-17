load(":apple_bundle_types.bzl", "AppleMinDeploymentVersionInfo")
load(":apple_resource_types.bzl", "AppleResourceProcessingOptions")
load(":apple_target_sdk_version.bzl", "get_min_deployment_version_for_node")
load(":apple_toolchain_types.bzl", "AppleToolchainInfo")

# `ctx` in all functions below is expected to be of `apple_bundle` or `apple_test` rule

def get_product_name(ctx: "context") -> str.type:
    return ctx.attrs.product_name if hasattr(ctx.attr, "product_name") and ctx.attrs.product_name != None else ctx.attrs.name

def get_extension_attr(ctx: "context") -> "":
    return ctx.attrs.extension

# Derives the effective deployment target for the bundle. It's
# usually the deployment target of the binary if present,
# otherwise it falls back to other values (see implementation).
def get_bundle_min_target_version(ctx: "context") -> str.type:
    binary_min_version = None

    # Could be not set for e.g. watchOS bundles which have a stub
    # binary that comes from the apple_toolchain(), not from the
    # apple_bundle() itself (i.e., binary field will be None).
    #
    # TODO(T114147746): The top-level stub bundle for a watchOS app
    # does not have the ability to set its deployment target via
    # a binary (as that field is empty). If it contains asset
    # catalogs (can it?), we need to use correct target version.
    #
    # The solution might to be support SDK version from
    # Info.plist (T110378109).
    if ctx.attrs.binary != None:
        min_version_info = ctx.attrs.binary[AppleMinDeploymentVersionInfo]
        if min_version_info != None:
            binary_min_version = min_version_info.version

    fallback_min_version = get_min_deployment_version_for_node(ctx)
    min_version = binary_min_version or fallback_min_version

    if min_version != None:
        return min_version

    # TODO(T110378109): support default value from SDK `Info.plist`
    fail("Could not determine min target sdk version for bundle: {}".format(ctx.label))

def get_bundle_resource_processing_options(ctx: "context") -> AppleResourceProcessingOptions.type:
    compile_resources_locally = ctx.attrs._apple_toolchain[AppleToolchainInfo].compile_resources_locally
    return AppleResourceProcessingOptions(prefer_local = compile_resources_locally)
