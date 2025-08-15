# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":apple_sdk_metadata.bzl", "MacOSXSdkMetadata", "WatchSimulatorSdkMetadata")

AppleTestDeviceType = enum(
    "default",
    "catalyst",
    "ipad",
    "iphone",
    "mac",
    "watch",
)

def get_default_test_device(sdk: str, platform: str) -> AppleTestDeviceType:
    if sdk == MacOSXSdkMetadata.name:
        return AppleTestDeviceType("mac")
    elif sdk == WatchSimulatorSdkMetadata.name:
        return AppleTestDeviceType("watch")
    elif "catalyst" in platform:
        return AppleTestDeviceType("catalyst")
    else:
        return AppleTestDeviceType("iphone")
