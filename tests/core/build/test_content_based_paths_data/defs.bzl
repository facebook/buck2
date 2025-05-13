# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _write_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.txt", uses_experimental_content_based_path_hashing = True)
    return [DefaultInfo(default_output = ctx.actions.write(out, ctx.attrs.data))]

write_with_content_based_path = rule(
    impl = _write_with_content_based_path_impl,
    attrs = {
        "data": attrs.string(),
    },
)
