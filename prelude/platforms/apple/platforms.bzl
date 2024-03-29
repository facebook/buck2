# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//platforms/apple:platforms_map.bzl", "APPLE_PLATFORMS_MAP")

def get_base_target_platform_for_platform(sdk_arch) -> [str, None]:
    data = APPLE_PLATFORMS_MAP.get(sdk_arch)
    if data != None:
        return data.base_target_platform

    return None

def get_default_target_platform_for_platform(sdk_arch) -> [str, None]:
    data = APPLE_PLATFORMS_MAP.get(sdk_arch)
    if data != None:
        return data.target_platform

    return None
