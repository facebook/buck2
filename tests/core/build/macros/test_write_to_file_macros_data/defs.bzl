# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _write_file(ctx):
    f = ctx.actions.write("write_file.txt", "test test test")
    return [DefaultInfo(default_output = f)]

write_file = rule(
    impl = _write_file,
    attrs = {},
)

def _test_rule(ctx):
    arg = ctx.attrs.arg

    f = ctx.actions.declare_output("out.txt")
    f, _ = ctx.actions.write(f, [
        cmd_args(arg, hidden = arg),
        cmd_args(arg, relative_to = f),
        cmd_args(cmd_args(arg, relative_to = f)),
        cmd_args(cmd_args(arg), relative_to = f),
    ], allow_args = True, with_inputs = True)

    return [DefaultInfo(default_output = f)]

test_rule = rule(
    impl = _test_rule,
    attrs = {
        "arg": attrs.arg(),
    },
)
