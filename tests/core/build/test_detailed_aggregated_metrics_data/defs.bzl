# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def proj_impl(v):
    return v

MyTset = transitive_set(args_projections = {"proj1": proj_impl})

MyInfo = provider(fields = ["data"])

# We just want an impl that uses some dynamic outputs and tsets and that we can conditionally make the dynamic output's input fail.
def _impl(ctx):
    out1 = ctx.actions.copy_file("out1", ctx.attrs.input)
    out2 = ctx.actions.declare_output("out2")
    if ctx.attrs.dyn_input_good:
        ctx.actions.write(out2, "data")
    else:
        ctx.actions.run(cmd_args(["sh", "-c", "false"], hidden = [out2.as_output()]), category = "out2")

    tset = ctx.actions.tset(MyTset, value = out1, children = [d[MyInfo].data for d in ctx.attrs.deps])

    dyn_out = ctx.actions.declare_output("out3")

    def _dyn_impl(ctx, _inputs, outputs):
        ctx.actions.run(
            cmd_args(
                ["sh", "-c", 'echo > "$1"', "--", outputs[dyn_out].as_output()],
                hidden = [tset.project_as_args("proj1")],
            ),
            category = "dyn_out",
        )

    ctx.actions.dynamic_output(f = _dyn_impl, outputs = [dyn_out.as_output()], dynamic = [out2])

    out4 = ctx.actions.declare_output("out4")
    ctx.actions.run(
        cmd_args(
            ["sh", "-c", 'echo > "$1"', "--", out4.as_output()],
            hidden = [out1, out2, dyn_out],
        ),
        category = "out4",
    )

    return [DefaultInfo(default_outputs = [out4]), MyInfo(data = tset)]

my_rule = rule(impl = _impl, attrs = {
    "deps": attrs.list(attrs.dep()),
    "dyn_input_good": attrs.bool(),
    "input": attrs.source(),
})
