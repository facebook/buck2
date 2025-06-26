# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

DEVSERVER_PLATFORM_REGEX = "UNUSED"
ANDROID = "Android"
APPLE = "Apple"
CXX = "Default"
FBCODE = "Fbcode"
IOS = "ios"
MACOSX = "macosx"
WATCHOS = "watchos"
WINDOWS = "Windows"

def get_available_platforms():
    if native.host_info().os.is_macos:
        return (CXX, ANDROID, APPLE)
    if native.host_info().os.is_windows:
        return (CXX, ANDROID, WINDOWS)
    return (CXX, ANDROID)
