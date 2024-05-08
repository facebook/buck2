# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftToolchainInfo")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")

AppleToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "actool": provider_field(RunInfo),
        "architecture": provider_field(str),
        "codesign_allocate": provider_field(RunInfo),
        "codesign_identities_command": provider_field(RunInfo | None, default = None),
        "codesign": provider_field(RunInfo),
        "compile_resources_locally": provider_field(bool),
        "copy_scene_kit_assets": provider_field(RunInfo),
        "cxx_platform_info": provider_field(CxxPlatformInfo),
        "cxx_toolchain_info": provider_field(CxxToolchainInfo),
        "dsymutil": provider_field(RunInfo),
        "dwarfdump": provider_field(RunInfo | None, default = None),
        "extra_linker_outputs": provider_field(list[str]),
        "ibtool": provider_field(RunInfo),
        "installer": provider_field(Label),
        "libtool": provider_field(RunInfo),
        "lipo": provider_field(RunInfo),
        "mapc": provider_field(RunInfo | None, default = None),
        "min_version": provider_field(str | None, default = None),
        "momc": provider_field(RunInfo),
        "objdump": provider_field(RunInfo | None, default = None),
        "platform_path": provider_field(str | Artifact),
        "sdk_build_version": provider_field(str | None, default = None),
        # SDK name to be passed to tools (e.g. actool), equivalent to ApplePlatform::getExternalName() in v1.
        "sdk_name": provider_field(str),
        "sdk_path": provider_field(str | Artifact),
        # TODO(T124581557) Make it non-optional once there is no "selected xcode" toolchain
        "sdk_version": provider_field(str | None, default = None),
        "swift_toolchain_info": provider_field(SwiftToolchainInfo),
        "xcode_build_version": provider_field(str | None, default = None),
        "xcode_version": provider_field(str | None, default = None),
        "xctest": provider_field(RunInfo),
    },
)

AppleToolsInfo = provider(
    # @unsorted-dict-items
    fields = {
        "assemble_bundle": provider_field(RunInfo),
        "split_arch_combine_dsym_bundles_tool": provider_field(RunInfo),
        "dry_codesign_tool": provider_field(RunInfo),
        "adhoc_codesign_tool": provider_field(RunInfo),
        "selective_debugging_scrubber": provider_field(RunInfo),
        "info_plist_processor": provider_field(RunInfo),
        "ipa_package_maker": provider_field(RunInfo),
        "make_modulemap": provider_field(RunInfo),
        "make_vfsoverlay": provider_field(RunInfo),
        "xcframework_maker": provider_field(RunInfo),
    },
)
