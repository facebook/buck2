# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _write_string_impl(ctx):
    out = ctx.actions.write(ctx.attrs.out, ctx.attrs.content)
    return [DefaultInfo(default_output = out)]

write_string = rule(
    impl = _write_string_impl,
    attrs = {
        "content": attrs.string(default = ""),
        "out": attrs.string(),
    },
)

def _copy_file_impl(ctx):
    out = ctx.actions.copy_file(ctx.attrs.out, ctx.attrs.src)
    return [DefaultInfo(default_output = out)]

copy_file = rule(
    impl = _copy_file_impl,
    attrs = {
        "out": attrs.string(),
        "src": attrs.source(),
    },
)
