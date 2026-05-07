# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")

def apple_strip_args(ctx: AnalysisContext) -> cmd_args:
    cxx_toolchain_info = get_cxx_toolchain_info(ctx)
    flags = cxx_toolchain_info.strip_flags_info.strip_non_global_flags
    if flags == None:
        fail("Toolchain strip flags are not set")

    return cmd_args(flags)
