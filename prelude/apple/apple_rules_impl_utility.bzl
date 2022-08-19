load("@fbcode//buck2/prelude/apple:apple_bundle_types.bzl", "AppleBundleResourceInfo")
load("@fbcode//buck2/prelude/apple:apple_code_signing_types.bzl", "CodeSignType")
load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo", "AppleToolsInfo")
load("@fbcode//buck2/prelude/apple/user:resource_group_map.bzl", "resource_group_map_attr")

def get_apple_toolchain_attr():
    return attrs.toolchain_dep(default = "fbcode//buck2/platform/toolchain:apple-default", providers = [AppleToolchainInfo])

def apple_bundle_extra_attrs():
    return {
        "resource_group_map": resource_group_map_attr(),
        "_apple_installer": attrs.label(default = "buck//src/com/facebook/buck/installer/apple:apple_installer"),
        "_apple_toolchain": get_apple_toolchain_attr(),
        "_apple_tools": attrs.exec_dep(default = "fbsource//xplat/buck2/platform/apple:apple-tools", providers = [AppleToolsInfo]),
        "_apple_xctoolchain": attrs.toolchain_dep(default = "fbcode//buck2/platform/toolchain:apple-xctoolchain"),
        "_codesign_entitlements": attrs.option(attrs.source(), default = None),
        "_codesign_type": attrs.option(attrs.enum(CodeSignType.values()), default = None),
        "_incremental_bundling_enabled": attrs.bool(),
        "_provisioning_profiles": attrs.dep(default = "fbsource//xplat/buck2/provisioning_profiles:all"),
        "_resource_bundle": attrs.option(attrs.dep(providers = [AppleBundleResourceInfo]), default = None),
    }
