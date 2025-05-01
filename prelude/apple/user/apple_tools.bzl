# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

def _apple_tools_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        AppleToolsInfo(
            assemble_bundle = ctx.attrs.assemble_bundle[RunInfo],
            split_arch_combine_dsym_bundles_tool = ctx.attrs.split_arch_combine_dsym_bundles_tool[RunInfo],
            dry_codesign_tool = ctx.attrs.dry_codesign_tool[RunInfo],
            adhoc_codesign_tool = ctx.attrs.adhoc_codesign_tool[RunInfo],
            info_plist_processor = ctx.attrs.info_plist_processor[RunInfo],
            ipa_package_maker = ctx.attrs.ipa_package_maker[RunInfo],
            make_modulemap = ctx.attrs.make_modulemap[RunInfo],
            make_vfsoverlay = ctx.attrs.make_vfsoverlay[RunInfo],
            selective_debugging_scrubber = ctx.attrs.selective_debugging_scrubber[RunInfo],
            xcframework_maker = ctx.attrs.xcframework_maker[RunInfo],
            framework_sanitizer = ctx.attrs.framework_sanitizer[RunInfo],
            static_archive_linker = ctx.attrs.static_archive_linker[RunInfo],
            spm_packager = ctx.attrs.spm_packager[RunInfo],
        ),
    ]

# The `apple_tools` rule exposes a set of supplementary tools
# required by the Apple rules _internally_. Such tools are not
# toolchain/SDK specific, they're just internal helper tools.
registration_spec = RuleRegistrationSpec(
    name = "apple_tools",
    impl = _apple_tools_impl,
    attrs = {
        "adhoc_codesign_tool": attrs.option(attrs.exec_dep(providers = [RunInfo]), default = None),
        "assemble_bundle": attrs.exec_dep(providers = [RunInfo]),
        "dry_codesign_tool": attrs.exec_dep(providers = [RunInfo]),
        "framework_sanitizer": attrs.exec_dep(providers = [RunInfo]),
        "info_plist_processor": attrs.exec_dep(providers = [RunInfo]),
        "ipa_package_maker": attrs.exec_dep(providers = [RunInfo]),
        "make_modulemap": attrs.exec_dep(providers = [RunInfo]),
        "make_vfsoverlay": attrs.exec_dep(providers = [RunInfo]),
        "selective_debugging_scrubber": attrs.exec_dep(providers = [RunInfo]),
        "split_arch_combine_dsym_bundles_tool": attrs.exec_dep(providers = [RunInfo]),
        "spm_packager": attrs.exec_dep(providers = [RunInfo]),
        "static_archive_linker": attrs.exec_dep(providers = [RunInfo]),
        "xcframework_maker": attrs.exec_dep(providers = [RunInfo]),
    },
)
