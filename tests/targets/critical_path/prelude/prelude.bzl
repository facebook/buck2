def _write(ctx: "context") -> ["provider"]:
    out = ctx.actions.write("out", "test")
    return [DefaultInfo(default_outputs = [out])]

write = rule(impl = _write, attrs = {
})

def _cp(ctx: "context") -> ["provider"]:
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
    return [DefaultInfo(default_outputs = [out])]

cp = rule(impl = _cp, attrs = {
    "dep": attrs.dep(),
    "sleep": attrs.int(default = 0),
})

def _dynamic_cp(ctx: "context") -> ["provider"]:
    dummy = ctx.actions.write("dummy", "")

    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", _artifacts, outputs):
        # NOTE: dummy doesn't show in the critical path calculation at all.
        ctx.actions.run([
            "cp",
            inp,
            outputs[out].as_output(),
        ], category = "dynamic_cp_action")

    ctx.actions.dynamic_output(dynamic = [dummy], inputs = [inp], outputs = [out], f = f)
    return [DefaultInfo(default_outputs = [out])]

dynamic_cp = rule(impl = _dynamic_cp, attrs = {
    "dep": attrs.dep(),
})
