# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:expect.bzl", "expect")
load(":cxx_context.bzl", "get_cxx_toolchain_info")

def check_sub_target(
        ctx: AnalysisContext,
        diagnostics: dict[str, Artifact]) -> list[Provider]:
    expect(len(diagnostics) > 0)

    if len(diagnostics) == 1:
        all_diagnostics = diagnostics.values()[0]
    else:
        toolchain = get_cxx_toolchain_info(ctx)
        concatenate_diagnostics_tool = toolchain.internal_tools.concatenate_diagnostics
        all_diagnostics = ctx.actions.declare_output("diagnostics.txt")
        ctx.actions.run(
            [
                concatenate_diagnostics_tool,
                cmd_args(all_diagnostics.as_output(), format = "--out={}"),
                diagnostics.values(),
            ],
            category = "diagnostics",
        )

    return [DefaultInfo(
        default_output = all_diagnostics,
        sub_targets = {
            short_path: [DefaultInfo(default_output = diagnostics)]
            for short_path, diagnostics in diagnostics.items()
        },
    )]
