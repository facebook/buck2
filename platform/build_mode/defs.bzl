BuildModeInfo = provider(
    fields = ["cell", "mode"],
)

def _build_mode_impl(ctx):
    return [
        DefaultInfo(),
        BuildModeInfo(cell = ctx.attr.cell, mode = ctx.attr.mode),
    ]

build_mode = rule(
    attrs = {
        "cell": attr.string(),
        "mode": attr.option(attr.string(), default = None),
    },
    implementation = _build_mode_impl,
)
