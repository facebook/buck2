# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:arglike.bzl", "ArgLike")
load(":swift_toolchain.bzl", "get_swift_toolchain_info")

def get_sdk_flags(ctx: AnalysisContext) -> list[ArgLike]:
    swift_toolchain_info = get_swift_toolchain_info(ctx)
    if swift_toolchain_info.sdk_path:
        return ["-sdk", swift_toolchain_info.sdk_path]
    else:
        return []
