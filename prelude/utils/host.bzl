# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

HostOSTypes = ["linux", "macos", "windows"]

HostOSType = enum(*HostOSTypes)

def _compute_get_host_os() -> HostOSType:
    info = host_info()
    if info.os.is_linux:
        return HostOSType("linux")
    elif info.os.is_macos:
        return HostOSType("macos")
    elif info.os.is_windows:
        return HostOSType("windows")
    else:
        fail("Unknown host OS")

_HOST_OS = _compute_get_host_os()

def get_host_os() -> HostOSType:
    return _HOST_OS
