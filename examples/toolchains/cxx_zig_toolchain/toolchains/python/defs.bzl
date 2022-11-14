load(
    "@prelude//python_bootstrap:python_bootstrap.bzl",
    "PythonBootstrapToolchainInfo",
)

def _system_python_bootstrap_toolchain(_ctx):
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(
            interpreter = RunInfo(args = ["python3"]),
        ),
    ]

system_python_bootstrap_toolchain = rule(
    impl = _system_python_bootstrap_toolchain,
    attrs = {
    },
    is_toolchain_rule = True,
)
