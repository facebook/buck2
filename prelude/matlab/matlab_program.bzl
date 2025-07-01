# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":matlab_info.bzl", "MatlabToolchainInfo")

def matlab_program_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs._matlab_toolchain[MatlabToolchainInfo]

    cmd = cmd_args(toolchain.matlab_exe)
    cmd.add(
        "-batch",
        cmd_args(
            ctx.attrs.main.basename.removesuffix(".m"),
            quote = "shell",
        ),
    )
    cmd.add("-sd", cmd_args(ctx.attrs.main, parent = 1))

    return [DefaultInfo(default_output = None, other_outputs = [cmd]), RunInfo(cmd)]
