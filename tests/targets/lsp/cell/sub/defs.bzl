# @lint-ignore-every BUCKRESTRICTEDSYNTAX STARLARK BUCKFORMAT
def _dummy_binary_impl(ctx):
    out = ctx.write("out.txt", ctx.attr.name)
    return [DefaultInfo(default_outputs=[out])]

<rule>dummy_binary</rule> = rule(
    implementation=dummy_binary_impl,
    attrs={
        "deps": attr.list(attr.dep(), default=[])
    }
)
