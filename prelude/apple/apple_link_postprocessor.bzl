def get_apple_link_postprocessor(ctx):
    if ctx.attrs.link_postprocessor:
        return cmd_args(ctx.attrs.link_postprocessor[RunInfo])
    return None
