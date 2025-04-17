# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    out = ctx.actions.declare_output("out.txt")
    ctx.actions.run(
        cmd_args(
            "touch",
            out.as_output(),
        ),
        category = "run",
    )
    return [DefaultInfo(default_output = out)]

run_action = rule(
    impl = _impl,
    attrs = {},
)
