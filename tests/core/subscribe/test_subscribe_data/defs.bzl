# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl_cp(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(["cp", ctx.attrs.src, out.as_output()], category = "cp")
    return [DefaultInfo(out)]

cp = rule(attrs = {"src": attrs.source()}, impl = _impl_cp)
