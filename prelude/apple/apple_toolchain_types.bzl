# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

AppleToolchainInfo = provider(fields = [
    "actool",  # "RunInfo"
    "codesign_allocate",  # "RunInfo"
    "codesign_identities_command",  # ["RunInfo", None]
    "codesign",  # "RunInfo"
    "compile_resources_locally",  # bool.type
    "cxx_platform_info",  # "CxxPlatformInfo"
    "cxx_toolchain_info",  # "CxxToolchainInfo"
    "dsymutil",  # "RunInfo"
    "dwarfdump",  # ["RunInfo", None]
    "ibtool",  # "RunInfo"
    "installer",  # label
    "libtool",  # "RunInfo"
    "lipo",  # "RunInfo"
    "min_version",  # [None, str.type]
    "momc",  # "RunInfo"
    "platform_path",  # [str.type, artifact]
    "sdk_build_version",  # "[None, str.type]"
    # SDK name to be passed to tools (e.g. actool), equivalent to ApplePlatform::getExternalName() in v1.
    "sdk_name",  # str.type
    "sdk_path",  # [str.type, artifact]
    # TODO(T124581557) Make it non-optional once there is no "selected xcode" toolchain
    "sdk_version",  # [None, str.type]
    "swift_toolchain_info",  # "SwiftToolchainInfo"
    "watch_kit_stub_binary",  # "artifact"
    "xcode_build_version",  # "[None, str.type]"
    "xcode_version",  # "[None, str.type]"
    "xctest",  # "RunInfo"
])

AppleToolsInfo = provider(fields = [
    "assemble_bundle",  # RunInfo
    "dry_codesign_tool",  # "RunInfo"
    "selective_debugging_scrubber",  # "RunInfo"
    "info_plist_processor",  # RunInfo
    "make_modulemap",  # "RunInfo"
    "make_vfsoverlay",  # "RunInfo"
    "swift_objc_header_postprocess",  # "RunInfo"
])
