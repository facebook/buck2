# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_bundle_resources.bzl", "get_apple_bundle_resource_part_list")
load("@prelude//apple:apple_bundle_types.bzl", "AppleBundleResourceInfo")
load("@prelude//apple:apple_common.bzl", "apple_common")
load("@prelude//apple:apple_rules_impl_utility.bzl", "AppleFrameworkBundleModuleMapType", "get_apple_info_plist_build_system_identification_attrs")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//apple:resource_groups.bzl", "RESOURCE_GROUP_MAP_ATTR")
load("@prelude//apple/user:cpu_split_transition.bzl", "cpu_split_transition")
load("@prelude//decls:apple_rules.bzl", "AppleBundleExtension")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _get_apple_resources_toolchain_attr():
    return attrs.toolchain_dep(default = "toolchains//:apple-resources", providers = [AppleToolchainInfo])

def _apple_resource_bundle_impl(ctx: AnalysisContext) -> list[Provider]:
    resource_output = get_apple_bundle_resource_part_list(ctx)
    return [
        DefaultInfo(),
        AppleBundleResourceInfo(
            resource_output = resource_output,
        ),
    ]

def _apple_resource_bundle_attrs():
    attribs = {
        "asset_catalogs_compilation_options": attrs.dict(key = attrs.string(), value = attrs.any(), default = {}),
        "binary": attrs.option(attrs.split_transition_dep(cfg = cpu_split_transition), default = None),
        "copy_public_framework_headers": attrs.option(attrs.bool(), default = None),
        "deps": attrs.list(attrs.dep(), default = []),
        "extension": attrs.one_of(attrs.enum(AppleBundleExtension), attrs.string()),
        "ibtool_flags": attrs.option(attrs.list(attrs.string()), default = None),
        "info_plist": attrs.source(),
        "info_plist_substitutions": attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False, default = {}),
        "labels": attrs.list(attrs.string(), default = []),
        "module_map": attrs.option(attrs.one_of(attrs.enum(AppleFrameworkBundleModuleMapType), attrs.source()), default = None),
        "privacy_manifest": attrs.option(attrs.source(), default = None),
        "product_name": attrs.option(attrs.string(), default = None),
        "product_name_from_module_name": attrs.bool(default = False),
        "resource_group": attrs.option(attrs.string(), default = None),
        "resource_group_map": RESOURCE_GROUP_MAP_ATTR,
        "universal": attrs.option(attrs.bool(), default = None),
        # Only include macOS hosted toolchains, so we compile resources directly on Mac RE
        "_apple_toolchain": _get_apple_resources_toolchain_attr(),
        # Because `apple_resource_bundle` is a proxy for `apple_bundle`, we need to get `name`
        # field of the `apple_bundle`, as it's used as a fallback value in Info.plist.
        "_bundle_target_name": attrs.string(),
        "_compile_resources_locally_override": attrs.option(attrs.bool(), default = None),
    }
    attribs.update(get_apple_info_plist_build_system_identification_attrs())
    attribs.update(apple_common.apple_tools_arg())
    return attribs

registration_spec = RuleRegistrationSpec(
    name = "apple_resource_bundle",
    impl = _apple_resource_bundle_impl,
    attrs = _apple_resource_bundle_attrs(),
)
