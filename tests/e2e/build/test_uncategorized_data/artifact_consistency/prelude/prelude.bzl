# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _gen_files(ctx):
    for i in range(0, ctx.attrs.action_index):
        ctx.actions.write("action-{}".format(i), "nonsense data")

    output = ctx.actions.declare_output("out_dir", dir = True)
    subouts = []
    subtargets = {}
    for f in ctx.attrs.files:
        out = output.project(f)
        subouts.append(out.as_output())
        subtargets[f] = [DefaultInfo(default_outputs = [out])]
    args = cmd_args(
        ["python3", ctx.attrs.script, output.as_output()] + ctx.attrs.files,
        hidden = subouts,
    )
    ctx.actions.run(args, category = "gen")
    return [DefaultInfo(sub_targets = subtargets)]

gen_files = rule(
    impl = _gen_files,
    attrs = {
        "action_index": attrs.int(),
        "files": attrs.list(attrs.string()),
        "script": attrs.source(),
    },
)
