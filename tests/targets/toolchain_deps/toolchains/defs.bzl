def _alias(ctx):
    return ctx.attr.dep.providers

alias = rule(
    impl = _alias,
    attrs = {"dep": attr.dep()},
)

def _toolchain(ctx):
    return ctx.attr.dep.providers

toolchain = rule(
    impl = _toolchain,
    attrs = {"dep": attr.exec_dep()},
    is_toolchain_rule = True,
)

def _command(ctx):
    return [DefaultInfo(default_outputs = [ctx.attr.source]), RunInfo(args = cmd_args(ctx.attr.source))]

command = rule(
    impl = _command,
    attrs = {"source": attr.source()},
)
