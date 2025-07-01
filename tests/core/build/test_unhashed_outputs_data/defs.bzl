# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _projected_output_impl(ctx):
    f = ctx.actions.write("f", "")
    d = ctx.actions.copied_dir("dir", {"file": f})
    return [
        DefaultInfo(default_output = d.project("file")),
    ]

projected_output = rule(
    impl = _projected_output_impl,
    attrs = {},
)
