# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@fbsource//tools/build_defs/default_platform_defs.bzl",
    _ANDROID = "ANDROID",
    _APPLE = "APPLE",
    _CXX = "CXX",
    _FBCODE = "FBCODE",
    _IOS = "IOS",
    _MACOSX = "MACOSX",
    _WATCHOS = "WATCHOS",
    _WINDOWS = "WINDOWS",
)

ANDROID = _ANDROID
APPLE = _APPLE
CXX = _CXX
FBCODE = _FBCODE
IOS = _IOS
MACOSX = _MACOSX
WATCHOS = _WATCHOS
WINDOWS = _WINDOWS
