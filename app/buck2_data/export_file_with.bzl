# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# A version of the `export_file` rule that allows attaching some associated
# artifacts
def _impl(ctx):
    out = ctx.attrs.src
    out = out.with_associated_artifacts(ctx.attrs.attach)
    return [DefaultInfo(default_output = out)]

export_file_with = rule(
    impl = _impl,
    attrs = {
        "attach": attrs.list(attrs.source()),
        "src": attrs.source(),
    },
)
