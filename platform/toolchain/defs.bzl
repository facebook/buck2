def _toolchain_impl(ctx: "context") -> ["provider"]:
    return ctx.attr.dep.providers

toolchain = rule(
    attrs = {"dep": attr.exec_dep()},
    implementation = _toolchain_impl,
    is_toolchain_rule = True,
)
