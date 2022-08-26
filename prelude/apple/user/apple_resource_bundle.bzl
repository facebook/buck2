load("@prelude//:attributes.bzl", "AppleBundleExtension")
load("@prelude//apple:apple_bundle_resources.bzl", "get_apple_bundle_resource_part_list")
load("@prelude//apple:apple_bundle_types.bzl", "AppleBundleResourceInfo")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo", "AppleToolsInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")
load(":resource_group_map.bzl", "resource_group_map_attr")

def _get_apple_resources_toolchain_attr():
    return attrs.toolchain_dep(default = "fbcode//buck2/platform/toolchain:apple-resources", providers = [AppleToolchainInfo])

def _impl(ctx: "context") -> ["provider"]:
    resource_output = get_apple_bundle_resource_part_list(ctx)
    return [
        DefaultInfo(),
        AppleBundleResourceInfo(
            resource_output = resource_output,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_resource_bundle",
    impl = _impl,
    attrs = {
        "asset_catalogs_compilation_options": attrs.dict(key = attrs.string(), value = attrs.any(), default = {}),
        "binary": attrs.option(attrs.dep(), default = None),
        "deps": attrs.list(attrs.dep(), default = []),
        "extension": attrs.one_of(attrs.enum(AppleBundleExtension), attrs.string()),
        "ibtool_flags": attrs.option(attrs.list(attrs.string()), default = None),
        "ibtool_module_flag": attrs.option(attrs.bool(), default = None),
        "info_plist": attrs.source(),
        "info_plist_substitutions": attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False, default = {}),
        "product_name": attrs.option(attrs.string(), default = None),
        "resource_group": attrs.option(attrs.string(), default = None),
        "resource_group_map": resource_group_map_attr(),
        # Only include macOS hosted toolchains, so we compile resources directly on Mac RE
        "_apple_toolchain": _get_apple_resources_toolchain_attr(),
        "_apple_tools": attrs.exec_dep(default = "fbsource//xplat/buck2/platform/apple:apple-tools", providers = [AppleToolsInfo]),
    },
)
