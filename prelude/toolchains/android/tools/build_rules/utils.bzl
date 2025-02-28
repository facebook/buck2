# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:native.bzl", "native")

# Returns true if build.is_oss is set to true; this should indicate that the current build is
# being done for the purpose of an oss release.
def is_oss_build():
    return native.read_config("build", "is_oss", "false") == "true"

def add_os_labels(**kwargs):
    if "labels" not in kwargs:
        kwargs["labels"] = []

    if native.host_info().os.is_macos:
        kwargs["labels"] += ["tpx:platform:macos"]
    if native.host_info().os.is_linux:
        kwargs["labels"] += ["tpx:platform:linux"]
    if native.host_info().os.is_windows:
        kwargs["labels"] += ["tpx:platform:windows"]

    if is_oss_build():
        kwargs["labels"] += ["tpx:is_oss_build"]

    return kwargs
