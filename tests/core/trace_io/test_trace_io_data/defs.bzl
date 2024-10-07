# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _binary_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(
            "touch",
            out.as_output(),
            hidden = ctx.attrs.srcs,
        ),
        category = "test",
    )
    return [DefaultInfo(default_output = out)]

def _library_impl(_ctx):
    return [DefaultInfo()]

my_binary = rule(impl = _binary_impl, attrs = {
    "deps": attrs.list(attrs.dep(), default = []),
    "srcs": attrs.list(attrs.source(), default = []),
})

my_library = rule(impl = _library_impl, attrs = {
    "deps": attrs.list(attrs.dep(), default = []),
    "srcs": attrs.list(attrs.source(), default = []),
})
