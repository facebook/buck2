# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

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
