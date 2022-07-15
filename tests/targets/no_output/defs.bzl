def _out_library_impl(ctx):
    outs = []
    for out in ctx.attrs.outs:
        for default_out in out[DefaultInfo].default_outputs:
            ctx.actions.write(default_out.basename, default_out)
            outs.append(default_out)

    return [DefaultInfo(default_outputs = outs)]

out_library = rule(
    impl = _out_library_impl,
    attrs = {
        "outs": attrs.list(attrs.dep()),
    },
)
