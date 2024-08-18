# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    base = ctx.attrs.base[AppleToolchainInfo]
    return [
        DefaultInfo(),
        AppleToolchainInfo(
            actool = base.actool,
            architecture = base.architecture,
            codesign = base.codesign,
            codesign_allocate = base.codesign_allocate,
            copy_scene_kit_assets = base.copy_scene_kit_assets,
            compile_resources_locally = base.compile_resources_locally,
            dsymutil = base.dsymutil,
            dwarfdump = base.dwarfdump,
            extra_linker_outputs = base.extra_linker_outputs,
            ibtool = base.ibtool,
            installer = base.installer,
            libtool = base.libtool,
            lipo = base.lipo,
            mapc = base.mapc,
            momc = base.momc,
            objdump = base.objdump,
            platform_path = base.platform_path,
            sdk_build_version = base.sdk_build_version,
            sdk_name = base.sdk_name,
            sdk_path = base.sdk_path,
            sdk_version = base.sdk_version,
            swift_toolchain_info = base.swift_toolchain_info,
            xcode_build_version = base.xcode_build_version,
            xcode_version = base.xcode_version,
            xctest = base.xctest,
        ),
    ]

registration_spec = RuleRegistrationSpec(
    name = "apple_toolchain_override",
    impl = _impl,
    attrs = {
        "base": attrs.toolchain_dep(providers = [AppleToolchainInfo]),
    },
    is_toolchain_rule = True,
)
