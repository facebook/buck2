# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _simple_impl(ctx):
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        [
            "python3",
            "-c",
            "import sys; open(sys.argv[1], 'w')",
            out.as_output(),
        ],
        category = "write",
    )

    return [
        DefaultInfo(
            default_output = out,
        ),
    ]

simple = rule(
    attrs = {},
    impl = _simple_impl,
)
