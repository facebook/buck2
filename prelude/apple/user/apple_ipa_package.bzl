# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_bundle_types.bzl", "AppleBundleInfo", "ApplePackageExtension")
load("@prelude//apple:apple_package_config.bzl", "IpaCompressionLevel")
load("@prelude//apple:apple_rules_impl_utility.bzl", "get_apple_bundle_toolchain_attr")
load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _apple_ipa_package_impl(ctx: AnalysisContext) -> list[Provider]:
    print(ctx)  # Silence warning
    return [DefaultInfo()]

def _apple_ipa_package_attribs():
    attribs = {
        "bundle": attrs.dep(providers = [AppleBundleInfo]),
        "ext": attrs.enum(ApplePackageExtension.values(), default = "ipa"),
        "labels": attrs.list(attrs.string(), default = []),
        "package_name": attrs.option(attrs.string(), default = None),
        "_apple_toolchain": get_apple_bundle_toolchain_attr(),
        "_apple_tools": attrs.exec_dep(default = "prelude//apple/tools:apple-tools", providers = [AppleToolsInfo]),
        "_ipa_compression_level": attrs.enum(IpaCompressionLevel.values()),
    }
    return attribs

registration_spec = RuleRegistrationSpec(
    name = "apple_ipa_package",
    impl = _apple_ipa_package_impl,
    attrs = _apple_ipa_package_attribs(),
)

_IPA_PACKAGE_FORWARDED_FIELDS = [
    "bundle",
    "ext",
    "package_name",
    "_ipa_compression_level",
    "compatible_with",
    "exec_compatible_with",
    "target_compatible_with",
    "default_target_platform",
    "within_view",
    "visibility",
]

def make_apple_ipa_package_target(apple_ipa_package_rule, **kwargs) -> [None, str]:
    ipa_package_kwargs = {
        "labels": ["generated"],
    }
    for field_name in _IPA_PACKAGE_FORWARDED_FIELDS:
        ipa_package_kwargs[field_name] = kwargs.get(field_name)

    ipa_package_target_name = kwargs["name"] + "__IPA_Package_Private"
    apple_ipa_package_rule(
        name = ipa_package_target_name,
        **ipa_package_kwargs
    )

    return ":{}".format(ipa_package_target_name)
