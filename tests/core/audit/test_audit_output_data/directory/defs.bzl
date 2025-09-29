# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _impl(ctx):
    out = ctx.actions.copied_dir("outputdir", {}, has_content_based_path = ctx.attrs.has_content_based_path)
    return [DefaultInfo(default_output = out)]

empty_dir = rule(
    impl = _impl,
    attrs = {
        "has_content_based_path": attrs.bool(default = False),
    },
)
