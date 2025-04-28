# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _declare_sub_targets(ctx: AnalysisContext) -> list[Provider]:
    out_dir = ctx.actions.declare_output("out_dir", dir = True)
    sub_targets = {
        name: [DefaultInfo(default_output = out_dir.project(name))]
        for name in ctx.attrs.sub_targets
    }
    ctx.actions.run(["python3", ctx.attrs.command, out_dir.as_output()], category = "mkdirs")
    return [DefaultInfo(default_output = out_dir, sub_targets = sub_targets)]

declare_sub_targets = rule(
    impl = _declare_sub_targets,
    attrs = {"command": attrs.source(), "sub_targets": attrs.list(attrs.string())},
)

def _exists(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("check")
    ctx.actions.run(
        ["python3", ctx.attrs.command, out.as_output(), ctx.attrs.paths],
        category = "check",
        local_only = ctx.attrs.local,
    )
    return [DefaultInfo(default_output = out)]

exists = rule(
    impl = _exists,
    attrs = {"command": attrs.source(), "local": attrs.bool(default = False), "paths": attrs.list(attrs.arg())},
)
