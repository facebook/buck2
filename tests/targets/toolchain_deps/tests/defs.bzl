def _python_binary_asic(ctx):
    out = ctx.actions.declare_output("out")
    cmd = cmd_args("echo $(", ctx.attrs._python_toolchain[RunInfo], ") $(", ctx.attrs._asic_toolchain[RunInfo], ") > ", out.as_output(), delimiter = "")
    ctx.actions.run(cmd_args("bash", "-c", cmd), category = "python_binary_asic")
    return [DefaultInfo(default_outputs = [out])]

python_binary_asic = rule(
    impl = _python_binary_asic,
    attrs = {
        "_asic_toolchain": attr.toolchain_dep(default = "//toolchains:asic"),
        "_python_toolchain": attr.toolchain_dep(default = "//toolchains:python"),
    },
)

def _python_binary(ctx):
    out = ctx.actions.declare_output("out")
    cmd = cmd_args("echo $(", ctx.attrs._python_toolchain[RunInfo], ") > ", out.as_output(), delimiter = "")
    ctx.actions.run(cmd_args("bash", "-c", cmd), category = "python_binary")
    return [DefaultInfo(default_outputs = [out])]

python_binary = rule(
    impl = _python_binary,
    attrs = {
        "_python_toolchain": attr.toolchain_dep(default = "//toolchains:python"),
    },
)
