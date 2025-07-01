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

def _python_toolchain_impl(ctx):
    distribution = ctx.attrs.distribution[DefaultInfo].default_outputs[0]
    interpreter = cmd_args(distribution, absolute_suffix = "/python/bin/python")
    return [
        DefaultInfo(),
        PythonToolchainInfo(
            binary_linker_flags = ctx.attrs.binary_linker_flags,
            linker_flags = ctx.attrs.linker_flags,
            host_interpreter = RunInfo(args = ["python3"]),
            interpreter = RunInfo(args = [interpreter]),
            compile = RunInfo(args = ["echo", "COMPILEINFO"]),
            package_style = "inplace",
            pex_extension = ctx.attrs.pex_extension,
            native_link_strategy = "separate",
        ),
        PythonPlatformInfo(name = "x86_64"),
    ]

python_toolchain = rule(
    impl = _python_toolchain_impl,
    attrs = {
        "binary_linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "distribution": attrs.exec_dep(),
        "linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "pex_extension": attrs.string(default = ".pex"),
    },
    is_toolchain_rule = True,
)
