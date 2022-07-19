def _write_string_impl(ctx):
    out = ctx.actions.write(ctx.attrs.out, ctx.attrs.content)
    return [DefaultInfo(default_outputs = [out])]

write_string = rule(
    impl = _write_string_impl,
    attrs = {
        "content": attrs.string(default = ""),
        "out": attrs.string(),
    },
)
