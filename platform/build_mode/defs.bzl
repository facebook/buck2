BuildModeInfo = provider(
    fields = ["cell", "mode"],
)

def _build_mode_impl(ctx):
    return [
        DefaultInfo(),
        BuildModeInfo(cell = ctx.attrs.cell, mode = ctx.attrs.mode),
    ]

build_mode = rule(
    attrs = {
        "cell": attrs.string(),
        "mode": attrs.option(attrs.string(), default = None),
    },
    impl = _build_mode_impl,
)
