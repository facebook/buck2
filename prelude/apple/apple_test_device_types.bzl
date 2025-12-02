# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":apple_sdk_metadata.bzl", "MacOSXSdkMetadata", "WatchSimulatorSdkMetadata")

apple_test_device_types = struct(
    DEFAULT = "default",
    CATALYST = "catalyst",
    IPAD = "ipad",
    IPHONE = "iphone",
    MAC = "mac",
    WATCH = "watch",
)

AppleTestDeviceType = enum(
    apple_test_device_types.DEFAULT,
    apple_test_device_types.CATALYST,
    apple_test_device_types.IPAD,
    apple_test_device_types.IPHONE,
    apple_test_device_types.MAC,
    apple_test_device_types.WATCH,
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

def tpx_label_for_test_device_type(test_device_type: AppleTestDeviceType) -> str:
    if test_device_type in (AppleTestDeviceType("catalyst"), AppleTestDeviceType("mac")):
        return "tpx:apple_test:device_type:mac"
    elif test_device_type == AppleTestDeviceType("ipad"):
        return "tpx:apple_test:device_type:ipad"
    elif test_device_type == AppleTestDeviceType("watch"):
        return "tpx:apple_test:device_type:watch"
    return "tpx:apple_test:device_type:iphone"
