def _toolchain_impl(ctx: "context") -> ["provider"]:
    return ctx.attrs.dep.providers

toolchain = rule(
    attrs = {"dep": attr.exec_dep()},
    impl = _toolchain_impl,
    is_toolchain_rule = True,
)
