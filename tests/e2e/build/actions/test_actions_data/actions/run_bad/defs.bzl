# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _run_invalid_command_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        ["this_binary_does_not_exist", out.as_output()],
        category = "test",
        local_only = ctx.attrs.local_only,
    )
    return [DefaultInfo(default_output = out)]

run_invalid_command = rule(impl = _run_invalid_command_impl, attrs = {"local_only": attrs.bool()})

def _run_odd_exit_code_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args("fbpython", "-c", "import sys; sys.exit(int(sys.argv[1]))", ctx.attrs.exit_code, out.as_output()),
        category = "test",
    )
    return [DefaultInfo(default_output = out)]

run_odd_exit_code = rule(impl = _run_odd_exit_code_impl, attrs = {"exit_code": attrs.string(default = "1")})
