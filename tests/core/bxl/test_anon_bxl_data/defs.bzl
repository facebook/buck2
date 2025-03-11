# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _simple_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.write(out, ctx.attrs.arg_attr)

    return [DefaultInfo(default_output = out)]

simple = rule(
    impl = _simple_impl,
    attrs = {
        "arg_attr": attrs.list(attrs.arg(anon_target_compatible = True), default = []),
    },
)
