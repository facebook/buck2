def _out_library_impl(ctx):
    outs = []
    for out in ctx.attrs.outs:
        for default_out in out[DefaultInfo].default_outputs:
            out_artifact = ctx.actions.declare_output(default_out.basename)
            ctx.actions.copy_file(out_artifact, default_out)
            outs.append(out_artifact)

    return [DefaultInfo(default_outputs = outs)]

out_library = rule(
    impl = _out_library_impl,
    attrs = {
        "outs": attrs.list(attrs.dep()),
    },
)
