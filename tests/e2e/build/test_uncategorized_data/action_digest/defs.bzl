# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test(ctx: AnalysisContext):
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        [
            "python3",
            ctx.attrs.script,
            out.as_output(),
        ],
        category = "check",
        identifier = "out",
    )

    return [DefaultInfo(out)]

test = rule(attrs = {"script": attrs.source()}, impl = _test)
