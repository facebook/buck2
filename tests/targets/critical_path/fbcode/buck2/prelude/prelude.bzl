def _write(ctx: "context") -> ["provider"]:
    out = ctx.actions.write("out", "test")
    return [DefaultInfo(default_outputs = [out])]

write = rule(impl = _write, attrs = {
})

def _cp(ctx: "context") -> ["provider"]:
    inp = ctx.attr.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")

    ctx.actions.run([
        "sh",
        "-c",
        'sleep "$1" && cp "$2" "$3"',
        "--",
        str(ctx.attr.sleep),
        inp,
        out.as_output(),
    ], category = "cp_action")
    return [DefaultInfo(default_outputs = [out])]

cp = rule(impl = _cp, attrs = {
    "dep": attr.dep(),
    "sleep": attr.int(default = 0),
})

def _dynamic_cp(ctx: "context") -> ["provider"]:
    dummy = ctx.actions.write("dummy", "")

    inp = ctx.attr.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("out")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context"):
        # NOTE: dummy doesn't show in the critical path calculation at all.
        ctx.actions.run([
            "cp",
            inp,
            ctx.outputs[out].as_output(),
        ], category = "dynamic_cp_action")

    ctx.actions.dynamic_output([dummy], [inp], [out], f)
    return [DefaultInfo(default_outputs = [out])]

dynamic_cp = rule(impl = _dynamic_cp, attrs = {
    "dep": attr.dep(),
})
