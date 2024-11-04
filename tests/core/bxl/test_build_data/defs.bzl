# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _run_remote(ctx):
    out = ctx.actions.declare_output("out.txt")
    data = ctx.actions.write("src.txt", "abcd")
    ctx.actions.run(
        cmd_args(["cp", data, out.as_output()]),
        category = "touch",
        prefer_remote = True,
    )
    return [DefaultInfo(default_output = out)]

run_remote = rule(
    impl = _run_remote,
    attrs = {},
)
