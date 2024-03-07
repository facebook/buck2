# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//:artifacts.bzl",
    "ArtifactGroupInfo",
)
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
    "config//os:windows": "python",
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
            fail_with_message = ctx.attrs.fail_with_message[RunInfo],
            make_source_db = ctx.attrs.make_source_db[RunInfo],
            make_source_db_no_deps = ctx.attrs.make_source_db_no_deps[RunInfo],
            host_interpreter = RunInfo(args = [ctx.attrs.interpreter]),
            interpreter = RunInfo(args = [ctx.attrs.interpreter]),
            make_py_package_modules = ctx.attrs.make_py_package_modules[RunInfo],
            make_py_package_inplace = ctx.attrs.make_py_package_inplace[RunInfo],
            compile = RunInfo(args = ["echo", "COMPILEINFO"]),
            package_style = "inplace",
            pex_extension = ctx.attrs.pex_extension,
            native_link_strategy = "separate",
            runtime_library = ctx.attrs.runtime_library,
        ),
        PythonPlatformInfo(name = "x86_64"),
    ]

system_python_toolchain = rule(
    impl = _system_python_toolchain_impl,
    attrs = {
        "fail_with_message": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:fail_with_message")),
        "interpreter": attrs.string(default = _INTERPRETER),
        "make_py_package_inplace": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_py_package_inplace")),
        "make_py_package_modules": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_py_package_modules")),
        "make_source_db": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_source_db")),
        "make_source_db_no_deps": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_source_db_no_deps")),
        "pex_extension": attrs.string(default = ".pex"),
        "runtime_library": attrs.default_only(attrs.dep(providers = [ArtifactGroupInfo], default = "prelude//python/runtime:bootstrap_files")),
    },
    is_toolchain_rule = True,
)
