def _alias(ctx):
    return ctx.attrs.dep.providers

alias = rule(
    impl = _alias,
    attrs = {"dep": attrs.dep()},
)

def _toolchain(ctx):
    return ctx.attrs.dep.providers

toolchain = rule(
    impl = _toolchain,
    attrs = {"dep": attrs.exec_dep()},
    is_toolchain_rule = True,
)

def _command(ctx):
    return [DefaultInfo(default_outputs = [ctx.attrs.source]), RunInfo(args = cmd_args(ctx.attrs.source))]

command = rule(
    impl = _command,
    attrs = {"source": attrs.source()},
)
