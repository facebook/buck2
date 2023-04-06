# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

CxxCompilerInfo = provider(
    doc = "Information about how to invoke the cpp compiler.",
    fields = ["compiler_path", "include_directories", "lib_directories"],
)

def _cpp_local_toolchain_impl(ctx):
    return [DefaultInfo(), CxxCompilerInfo(compiler_path = ctx.attrs.command)]

cpp_local_toolchain = rule(
    impl = _cpp_local_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "command": attrs.string(),
    },
)
