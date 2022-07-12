load("@fbcode//buck2/prelude:attributes.bzl", "AppleBundleExtension", "Traversal")
load("@fbcode//buck2/prelude/apple:apple_bundle_resources.bzl", "get_apple_bundle_resource_part_list")
load("@fbcode//buck2/prelude/apple:apple_bundle_types.bzl", "AppleBundleResourceInfo")
load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo", "AppleToolsInfo")
load("@fbcode//buck2/prelude/user:rule_spec.bzl", "RuleRegistrationSpec")

def _get_apple_resources_tolchain_attr():
    return attr.toolchain_dep(default = "fbcode//buck2/platform/toolchain:apple-resources", providers = [AppleToolchainInfo])

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
        "asset_catalogs_compilation_options": attr.dict(key = attr.string(), value = attr.any(), default = {}),
        "binary": attr.option(attr.dep(), default = None),
        "deps": attr.list(attr.dep(), default = []),
        "extension": attr.one_of(attr.enum(AppleBundleExtension), attr.string()),
        "ibtool_flags": attr.option(attr.list(attr.string()), default = None),
        "ibtool_module_flag": attr.option(attr.bool(), default = None),
        "info_plist": attr.source(),
        "info_plist_substitutions": attr.dict(key = attr.string(), value = attr.string(), sorted = False, default = {}),
        "product_name": attr.option(attr.string(), default = None),
        "resource_group": attr.option(attr.string(), default = None),
        "resource_group_map": attr.option(attr.list(attr.tuple(attr.string(), attr.list(attr.tuple(attr.dep(), attr.enum(Traversal), attr.option(attr.string()))))), default = None),
        # Only include macOS hosted toolchains, so we compile resources directly on Mac RE
        "_apple_toolchain": _get_apple_resources_tolchain_attr(),
        "_apple_tools": attr.exec_dep(default = "fbsource//xplat/buck2/platform/apple:apple-tools", providers = [AppleToolsInfo]),
    },
)
