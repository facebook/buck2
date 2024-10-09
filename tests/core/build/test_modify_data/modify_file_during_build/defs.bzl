# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _modify_file_impl(ctx):
    text = ctx.attrs.text

    out = ctx.actions.declare_output("out")

    ctx.actions.run([
        "python3",
        "-c",
        "import sys; fp=open(sys.argv[1], 'w'); fp.write('REPLACEMENT'); open(sys.argv[2], 'w')",
        text,
        out.as_output(),
    ], local_only = True, category = "test")

    return [DefaultInfo(default_output = out)]

modify_file = rule(
    impl = _modify_file_impl,
    attrs = {
        "text": attrs.source(),
    },
)

def _depend_impl(ctx):
    text = ctx.attrs.text
    modify_file = ctx.attrs.modify_file[DefaultInfo].default_outputs[0]

    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        cmd_args(
            [
                "cp",
                text,
                out.as_output(),
            ],
            hidden = modify_file,
        ),
        category = "test",
    )

    return [DefaultInfo(default_output = out)]

depend_file = rule(
    impl = _depend_impl,
    attrs = {
        "modify_file": attrs.dep(),
        "text": attrs.source(),
    },
)
