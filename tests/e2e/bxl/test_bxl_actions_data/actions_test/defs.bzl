# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _fail_impl(ctx):
    out = ctx.actions.declare_output("output1")
    ctx.actions.run(cmd_args("false", hidden = out.as_output()), category = "fail")
    return [DefaultInfo(default_outputs = [out])]

fail = rule(
    impl = _fail_impl,
    attrs = {},
)
