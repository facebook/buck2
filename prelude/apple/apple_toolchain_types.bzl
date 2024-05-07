# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# PLEASE DON'T CHANGE THE `typing.Any`` TO ACTUAL TYPE HERE. IT BREAKS AUTODEPS.
AppleToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "actool": provider_field(typing.Any, default = None),  # "RunInfo"
        "architecture": provider_field(typing.Any, default = None),  # str
        "codesign_allocate": provider_field(typing.Any, default = None),  # "RunInfo"
        "codesign_identities_command": provider_field(typing.Any, default = None),  # ["RunInfo", None]
        "codesign": provider_field(typing.Any, default = None),  # "RunInfo"
        "compile_resources_locally": provider_field(typing.Any, default = None),  # bool
        "copy_scene_kit_assets": provider_field(typing.Any, default = None),  # "RunInfo"
        "cxx_platform_info": provider_field(typing.Any, default = None),  # "CxxPlatformInfo"
        "cxx_toolchain_info": provider_field(typing.Any, default = None),  # "CxxToolchainInfo"
        "dsymutil": provider_field(typing.Any, default = None),  # "RunInfo"
        "dwarfdump": provider_field(typing.Any, default = None),  # ["RunInfo", None]
        "extra_linker_outputs": provider_field(typing.Any, default = None),  # [str]
        "ibtool": provider_field(typing.Any, default = None),  # "RunInfo"
        "installer": provider_field(typing.Any, default = None),  # label
        "libtool": provider_field(typing.Any, default = None),  # "RunInfo"
        "lipo": provider_field(typing.Any, default = None),  # "RunInfo"
        "mapc": provider_field(typing.Any, default = None),  # "RunInfo"
        "min_version": provider_field(typing.Any, default = None),  # [None, str]
        "momc": provider_field(typing.Any, default = None),  # "RunInfo"
        "objdump": provider_field(typing.Any, default = None),  # ["RunInfo", None]
        "platform_path": provider_field(typing.Any, default = None),  # [str, artifact]
        "sdk_build_version": provider_field(typing.Any, default = None),  # "[None, str]"
        # SDK name to be passed to tools (e.g. actool), equivalent to ApplePlatform::getExternalName() in v1.
        "sdk_name": provider_field(typing.Any, default = None),  # str
        "sdk_path": provider_field(typing.Any, default = None),  # [str, artifact]
        # TODO(T124581557) Make it non-optional once there is no "selected xcode" toolchain
        "sdk_version": provider_field(typing.Any, default = None),  # [None, str]
        "swift_toolchain_info": provider_field(typing.Any, default = None),  # "SwiftToolchainInfo"
        "xcode_build_version": provider_field(typing.Any, default = None),  # "[None, str]"
        "xcode_version": provider_field(typing.Any, default = None),  # "[None, str]"
        "xctest": provider_field(typing.Any, default = None),  # "RunInfo"
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
