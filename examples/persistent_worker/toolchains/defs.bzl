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

def _python_toolchain_impl(ctx):
    distribution = ctx.attrs.distribution[DefaultInfo].default_outputs[0]
    interpreter = cmd_args(distribution, absolute_suffix = "/python/bin/python")
    return [
        DefaultInfo(),
        PythonToolchainInfo(
            binary_linker_flags = ctx.attrs.binary_linker_flags,
            linker_flags = ctx.attrs.linker_flags,
            fail_with_message = ctx.attrs.fail_with_message[RunInfo],
            generate_static_extension_info = ctx.attrs.generate_static_extension_info,
            make_source_db = ctx.attrs.make_source_db[RunInfo],
            make_source_db_no_deps = ctx.attrs.make_source_db_no_deps[RunInfo],
            host_interpreter = RunInfo(args = ["python3"]),
            interpreter = RunInfo(args = [interpreter]),
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

python_toolchain = rule(
    impl = _python_toolchain_impl,
    attrs = {
        "binary_linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "distribution": attrs.exec_dep(),
        "fail_with_message": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:fail_with_message")),
        "generate_static_extension_info": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:generate_static_extension_info")),
        "linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "make_py_package_inplace": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_py_package_inplace")),
        "make_py_package_modules": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_py_package_modules")),
        "make_source_db": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_source_db")),
        "make_source_db_no_deps": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//python/tools:make_source_db_no_deps")),
        "pex_extension": attrs.string(default = ".pex"),
        "runtime_library": attrs.default_only(attrs.dep(providers = [ArtifactGroupInfo], default = "prelude//python/runtime:bootstrap_files")),
    },
    is_toolchain_rule = True,
)
