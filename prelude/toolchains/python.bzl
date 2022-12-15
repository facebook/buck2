# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "//python_bootstrap:python_bootstrap.bzl",
    "PythonBootstrapToolchainInfo",
)

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
        "interpreter": attrs.string(default = select({
            "ovr_config//os:linux": "python3",
            "ovr_config//os:macos": "python3",
            "ovr_config//os:windows": "python",
        })),
    },
    is_toolchain_rule = True,
)
