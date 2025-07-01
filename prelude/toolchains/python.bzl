# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//python:toolchain.bzl",
    "PythonPlatformInfo",
    "PythonToolchainInfo",
)
load(
    "@prelude//python_bootstrap:python_bootstrap.bzl",
    "PythonBootstrapToolchainInfo",
)

_INTERPRETER = select({
    "DEFAULT": "python3",
    "config//os:windows": "python.exe",
})

def _system_python_bootstrap_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(interpreter = ctx.attrs.interpreter),
    ]

# Creates a new bootstrap toolchain using Python that is installed on your system.
# You may use it in your toolchain cell as follows:
#
# ```bzl
# load("@prelude//toolchains:python.bzl", "system_python_bootstrap_toolchain")
#
# system_python_bootstrap_toolchain(
#     name = "python_bootstrap", # the default name rules look for
#     visibility = ["PUBLIC"],
# )
# ```
system_python_bootstrap_toolchain = rule(
    impl = _system_python_bootstrap_toolchain_impl,
    attrs = {
        "interpreter": attrs.string(default = _INTERPRETER),
    },
    is_toolchain_rule = True,
)

def _system_python_toolchain_impl(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(),
        PythonToolchainInfo(
            binary_linker_flags = ctx.attrs.binary_linker_flags,
            linker_flags = ctx.attrs.linker_flags,
            host_interpreter = RunInfo(args = [ctx.attrs.interpreter]),
            interpreter = RunInfo(args = [ctx.attrs.interpreter]),
            compile = RunInfo(args = ["echo", "COMPILEINFO"]),
            package_style = "inplace",
            pex_extension = ctx.attrs.pex_extension,
            native_link_strategy = "separate",
        ),
        PythonPlatformInfo(name = "x86_64"),
    ]

system_python_toolchain = rule(
    impl = _system_python_toolchain_impl,
    attrs = {
        "binary_linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "interpreter": attrs.string(default = _INTERPRETER),
        "linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "pex_extension": attrs.string(default = ".pex"),
    },
    is_toolchain_rule = True,
)
