# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _top(ctx):
    out = ctx.actions.declare_output("out")

    ctx.actions.run(
        [ctx.attrs.src, out.as_output()],
        category = "test",
    )

    return [DefaultInfo(out)]

top = rule(impl = _top, attrs = {"src": attrs.source()})
