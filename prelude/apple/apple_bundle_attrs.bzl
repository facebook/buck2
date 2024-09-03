# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_platforms.bzl", "APPLE_PLATFORMS_KEY")
load("@prelude//apple:apple_rules_impl_utility.bzl", "apple_bundle_extra_attrs")
load("@prelude//apple:resource_groups.bzl", "RESOURCE_GROUP_MAP_ATTR")
load("@prelude//decls/ios_rules.bzl", "AppleBundleExtension")

def _apple_bundle_base_attrs():
    return {
        # Attributes comes from `attributes.bzl` but since it's autogenerated, we cannot easily abstract
        "asset_catalogs_compilation_options": attrs.dict(key = attrs.string(), value = attrs.any(), default = {}),
        "codesign_flags": attrs.list(attrs.string(), default = []),
        "codesign_identity": attrs.option(attrs.string(), default = None),
        "contacts": attrs.list(attrs.string(), default = []),
        "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
        "default_platform": attrs.option(attrs.string(), default = None),
        "deps": attrs.list(attrs.dep(), default = []),
        "extension": attrs.one_of(attrs.enum(AppleBundleExtension), attrs.string()),
        "ibtool_flags": attrs.option(attrs.list(attrs.string()), default = None),
        "ibtool_module_flag": attrs.option(attrs.bool(), default = None),
        "incremental_bundling_enabled": attrs.option(attrs.bool(), default = None),
        "info_plist": attrs.source(),
        "info_plist_substitutions": attrs.dict(key = attrs.string(), value = attrs.string(), sorted = False, default = {}),
        "labels": attrs.list(attrs.string(), default = []),
        "licenses": attrs.list(attrs.source(), default = []),
        "platform_binary": attrs.option(attrs.list(attrs.tuple(attrs.regex(), attrs.dep())), default = None),
        "product_name": attrs.option(attrs.string(), default = None),
        "resource_group": attrs.option(attrs.string(), default = None),
        "resource_group_map": attrs.option(RESOURCE_GROUP_MAP_ATTR, default = None),
        "skip_copying_swift_stdlib": attrs.option(attrs.bool(), default = None),
        "try_skip_code_signing": attrs.option(attrs.bool(), default = None),
        "xcode_product_type": attrs.option(attrs.string(), default = None),
    }

def _apple_bundle_default_attrs():
    attributes = {}
    attributes.update(_apple_bundle_base_attrs())
    attributes.update(apple_bundle_extra_attrs())
    attributes.update({
        APPLE_PLATFORMS_KEY: attrs.dict(key = attrs.string(), value = attrs.dep(), sorted = False, default = {}),
    })
    return attributes

def apple_watchos_bundle_attrs():
    attributes = _apple_bundle_default_attrs()
    attributes.update({
        "bundle_type": attrs.string(default = "watchapp"),
    })
    return attributes

def apple_macos_bundle_attrs():
    attributes = _apple_bundle_default_attrs()
    attributes.update({
        "bundle_type": attrs.string(default = "default"),
    })
    return attributes
