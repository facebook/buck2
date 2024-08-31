# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _upload_action(ctx):
    val = read_config("test", "content", "")
    tmp = ctx.actions.write("tmp.txt", val)
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(["sh", "-c", 'echo > "$1"', "--", out.as_output()], hidden = [tmp]),
        category = "run",
    )
    return [DefaultInfo(default_outputs = [out])]

upload_rule = rule(
    impl = _upload_action,
    attrs = {},
)
