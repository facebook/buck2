# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _build_fail(ctx):
    out = ctx.actions.declare_output("out.txt")
    ctx.actions.run(
        cmd_args("python3", "-c", "import sys; sys.exit(1)", hidden = out.as_output()),
        category = "run",
    )
    return [DefaultInfo(default_output = out)]

build_fail = rule(
    impl = _build_fail,
    attrs = {},
)
