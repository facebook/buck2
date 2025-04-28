# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _duplicate_output_impl(ctx):
    out = ctx.actions.declare_output("bar.txt")
    data = ctx.actions.write("foo.txt", "42")
    ctx.actions.run(
        cmd_args(["cp", data, out.as_output()]),
        category = "copy",
    )
    return [DefaultInfo(default_outputs = [data, out])]

duplicate_output = rule(
    impl = _duplicate_output_impl,
    attrs = {},
)

def _fail_duplicate(ctx):
    out = ctx.actions.declare_output("bar.txt")
    data = ctx.actions.write("foo.txt", "42")
    ctx.actions.run(
        cmd_args(["wrong_cp", data, out.as_output()]),
        category = "copy",
    )
    return [DefaultInfo(default_outputs = [data, out])]

fail_duplicate = rule(
    impl = _fail_duplicate,
    attrs = {},
)
