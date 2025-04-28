# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _write(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.write("out", "test")
    return [DefaultInfo(default_output = out)]

write = rule(impl = _write, attrs = {
})

def _cp(ctx: AnalysisContext) -> list[Provider]:
    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")

    ctx.actions.run([
        "sh",
        "-c",
        'sleep "$1" && cp "$2" "$3"',
        "--",
        str(ctx.attrs.sleep),
        inp,
        out.as_output(),
    ], category = "cp_action")
    return [DefaultInfo(default_output = out)]

cp = rule(impl = _cp, attrs = {
    "dep": attrs.dep(),
    "sleep": attrs.int(default = 0),
})

def _dynamic_cp(ctx: AnalysisContext) -> list[Provider]:
    dummy = ctx.actions.write("dummy", "")

    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")

    def f(ctx: AnalysisContext, _artifacts, outputs):
        # NOTE: dummy doesn't show in the critical path calculation at all.
        ctx.actions.run([
            "cp",
            inp,
            outputs[out].as_output(),
        ], category = "dynamic_cp_action")

    ctx.actions.dynamic_output(dynamic = [dummy], inputs = [inp], outputs = [out.as_output()], f = f)
    return [DefaultInfo(default_output = out)]

dynamic_cp = rule(impl = _dynamic_cp, attrs = {
    "dep": attrs.dep(),
})

def _dynamic_cp2(ctx: AnalysisContext) -> list[Provider]:
    dummy = ctx.actions.write("dummy", "")

    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")

    def f(ctx: AnalysisContext, artifacts, outputs):
        ctx.actions.write(outputs[out].as_output(), artifacts[inp].read_string())

    ctx.actions.dynamic_output(dynamic = [inp], inputs = [], outputs = [out.as_output()], f = f)
    return [DefaultInfo(default_output = out)]

dynamic_cp2 = rule(impl = _dynamic_cp2, attrs = {
    "dep": attrs.dep(),
})
