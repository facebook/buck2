# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _dep_impl(ctx):
    out = ctx.actions.declare_output("dep")
    ctx.actions.run(
        [
            "python3",
            ctx.attrs.script,
            out.as_output(),
        ],
        category = "test",
    )
    return [DefaultInfo(default_output = out)]

dep = rule(
    impl = _dep_impl,
    attrs = {
        "script": attrs.source(),
    },
)

def _large_action_input(ctx):
    dep = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(
            [
                "python3",
                "-c",
                "import sys, os; file_size = os.path.getsize(sys.argv[2]); open(sys.argv[1], 'w').write(str(file_size))",
            ],
            out.as_output(),
            dep,
        ),
        category = "test",
    )

    return [DefaultInfo(out)]

large_action_input = rule(
    impl = _large_action_input,
    attrs = {
        "dep": attrs.dep(),
    },
)
