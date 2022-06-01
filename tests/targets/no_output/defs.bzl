def _out_library_impl(ctx):
    outs = []
    for out in ctx.attr.outs:
        for default_out in out[DefaultInfo].default_outputs:
            ctx.actions.write(default_out.basename, default_out)
            outs.append(default_out)

    return [DefaultInfo(default_outputs = outs)]

out_library = rule(
    implementation = _out_library_impl,
    attrs = {
        "outs": attr.list(attr.dep()),
    },
)
