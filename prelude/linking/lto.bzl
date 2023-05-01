# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Styles of LTO.
LtoMode = enum(
    # No LTO
    "none",
    # Object files contain both LTO IR and native code to allow binaries to link
    # either via standard or LTO.
    "fat",
    # Traditional, monolithic LTO.
    "monolithic",
    # https://clang.llvm.org/docs/ThinLTO.html
    "thin",
)

def lto_compiler_flags(lto_mode: "LtoMode") -> [str.type]:
    if lto_mode == LtoMode("none"):
        return []
    elif lto_mode == LtoMode("fat") or lto_mode == LtoMode("monolithic"):
        return ["-flto=full"]
    elif lto_mode == LtoMode("thin"):
        return ["-flto=thin"]
    else:
        fail("Unhandled LTO mode: " + lto_mode)
