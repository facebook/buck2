# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _slow_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        ["python3", ctx.attrs.src, out.as_output()],
        category = "slow",
    )
    return [DefaultInfo(out)]

slow_actions = rule(impl = _slow_impl, attrs = {
    "src": attrs.source(),
})
