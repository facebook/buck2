load("@fbcode//buck2/prelude/python_bootstrap:python_bootstrap.bzl", "PythonBootstrapToolchainInfo")

def config_backed_python_bootstrap_toolchain(flavor, **kwargs):
    interpreter = native.read_config("python#" + flavor, "interpreter", None)
    if interpreter == None:
        interpreter = native.read_config("python", "interpreter")
    _config_backed_python_bootstrap_toolchain_rule(
        name = "bootstrap-" + flavor,
        interpreter = interpreter,
        **kwargs
    )

def _config_backed_python_bootstrap_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(
            interpreter = ctx.attr.interpreter,
        ),
    ]

_config_backed_python_bootstrap_toolchain_rule = rule(
    impl = _config_backed_python_bootstrap_toolchain_rule_impl,
    attrs = {
        "interpreter": attr.arg(),
    },
)
