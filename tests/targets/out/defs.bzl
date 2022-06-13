def _out_library_impl(ctx):
    outs = []
    for out in ctx.attr.outs:
        for default_out in out[DefaultInfo].default_outputs:
            out_artifact = ctx.actions.declare_output(default_out.basename)
            ctx.actions.copy_file(out_artifact, default_out)
            outs.append(out_artifact)

    return [DefaultInfo(default_outputs = outs)]

out_library = rule(
    implementation = _out_library_impl,
    attrs = {
        "outs": attr.list(attr.dep()),
    },
)
