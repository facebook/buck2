def _toolchain_impl(ctx: "context") -> ["provider"]:
    if ctx.attrs.dep != None:
        return ctx.attrs.dep.providers
    return [DefaultInfo()]

toolchain = rule(
    attrs = {"dep": attr.option(attr.exec_dep(), default = None)},
    impl = _toolchain_impl,
    is_toolchain_rule = True,
)
