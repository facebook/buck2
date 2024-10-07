# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    out = ctx.actions.declare_output("bound_dynamic.txt")

    def dynamic(ctx, out):
        defined_dynamic = ctx.actions.write("defined_dynamic.txt", "abcd")
        ctx.actions.copy_file(out.as_output(), defined_dynamic)

    f = lambda ctx, _dyn, outputs: dynamic(ctx, outputs[out])

    ctx.actions.dynamic_output(dynamic = [], inputs = [], outputs = [out.as_output()], f = f)

    return [DefaultInfo(default_output = out)]

dynamic_output = rule(
    impl = _impl,
    attrs = {},
)
