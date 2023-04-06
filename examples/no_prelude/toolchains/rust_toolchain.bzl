# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

RustCompilerInfo = provider(
    doc = "Information about how to invoke the Rust compiler.",
    fields = ["compiler_path"],
)

def _rust_local_toolchain_impl(ctx):
    return [DefaultInfo(), RustCompilerInfo(compiler_path = ctx.attrs.command)]

rust_local_toolchain = rule(
    impl = _rust_local_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "command": attrs.string(),
    },
)
