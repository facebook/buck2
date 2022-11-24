load(
    "//python_bootstrap:python_bootstrap.bzl",
    "PythonBootstrapToolchainInfo",
)

def _python_bootstrap_toolchain_impl(_ctx):
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(interpreter = ctx.attrs.interpreter),
    ]

_python_bootstrap_toolchain = rule(
    impl = _python_bootstrap_toolchain_impl,
    attrs = {
        "interpreter": attrs.string(default = select({
            "ovr_config//os:linux": "python3",
            "ovr_config//os:macos": "python3",
            "ovr_config//os:windows": "python",
        })),
    },
    is_toolchain_rule = True,
)

def python_bootstrap_toolchain(name, visibility = None):
    """
    Creates a new bootstrap toolchain with the given name
    and visibility levels. IF the visibility is not provided,
    defaults to public. You may use it in your toolchain cell
    as follows:

    ```bzl
    load("@prelude//toolchains/python/bootstrap:defs.bzl", "python_bootstrap_toolchain")

    python_bootstrap_toolchain(
        name="python_bootstrap", # the default name rules look for
        visibility=["PUBLIC"],
    )
    ```
    """
    if visibility == None:
        visibility = ["PUBLIC"]

    _python_bootstrap_toolchain(
        name = name,
        visibility = visibility,
    )
