# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_impl(ctx):
    out = ctx.actions.declare_output("file")
    ctx.actions.run(
        ["touch", out.as_output()],
        category = "touch",
        env = {"seed": ctx.attrs.seed},
    )
    return [DefaultInfo(out)]

test = rule(attrs = {"seed": attrs.string()}, impl = _test_impl)
