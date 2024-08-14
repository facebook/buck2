# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_bundle_types.bzl", "AppleBundleInfo", "ApplePackageExtension")
load("@prelude//apple:apple_package_config.bzl", "IpaCompressionLevel")
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
        "_apple_tools": attrs.exec_dep(default = "prelude//apple/tools:apple-tools", providers = [AppleToolsInfo]),
        "_ipa_compression_level": attrs.enum(IpaCompressionLevel.values()),
    }
    return attribs

registration_spec = RuleRegistrationSpec(
    name = "apple_ipa_package",
    impl = _apple_ipa_package_impl,
    attrs = _apple_ipa_package_attribs(),
)
