# @nolint

def loop_long(kind):
    if read_config("should", "loop", "") != kind:
        return

    for i in range(2147483647):
        for j in range(2147483647):
            for k in range(2147483647):
                pass

def _noop(ctx):
    outs = []
    out = ctx.actions.write("out.txt", ctx.attrs.name)
    outs.append(out)
    loop_long("analysis")

    return [DefaultInfo(default_outputs = outs)]

noop = rule(
    impl = _noop,
    attrs = {},
)
