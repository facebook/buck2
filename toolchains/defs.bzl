def _impl(ctx):
    return ctx.attrs.alias.providers

toolchain_alias = rule(
    impl = _impl,
    is_toolchain_rule = True,
    attrs = {
        "alias": attrs.toolchain_dep(),
    },
)
