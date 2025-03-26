# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _simple_write_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.write(out, ctx.attrs.string_attr)

    return [DefaultInfo(default_output = out)]

simple_write = rule(
    impl = _simple_write_impl,
    attrs = {
        "string_attr": attrs.string(),
    },
)

def _simple_copy_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.copy_file(out, ctx.attrs.to_copy)

    return [DefaultInfo(default_output = out)]

simple_copy = rule(
    impl = _simple_copy_impl,
    attrs = {
        "to_copy": attrs.source(),
    },
)

def _simple_symlinked_dir_impl(ctx):
    srcs = {str(k): v for (k, v) in enumerate(ctx.attrs.inputs)}
    out = ctx.actions.symlinked_dir("out", srcs)

    return [DefaultInfo(default_output = out)]

simple_symlinked_dir = rule(
    impl = _simple_symlinked_dir_impl,
    attrs = {
        "inputs": attrs.list(attrs.source()),
    },
)
