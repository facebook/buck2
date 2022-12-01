# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _rust_binary_impl(ctx):
    file = ctx.attrs.file
    out = ctx.actions.declare_output("main")

    cmd = cmd_args(["rustc", "--crate-type=bin", file, "-o", out.as_output()])

    ctx.actions.run(cmd, category = "compile")

    return [DefaultInfo(default_outputs = [out]), RunInfo(args = cmd_args([out]))]

rust_binary = rule(
    impl = _rust_binary_impl,
    attrs = {
        "file": attrs.source(),
    },
)
