# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _out_library_impl(ctx):
    outs = []
    for out in ctx.attrs.outs:
        for default_out in out[DefaultInfo].default_outputs:
            out_artifact = ctx.actions.copy_file(default_out.basename, default_out)
            outs.append(out_artifact)

    return [DefaultInfo(default_outputs = outs)]

out_library = rule(
    impl = _out_library_impl,
    attrs = {
        "outs": attrs.list(attrs.dep()),
    },
)
