# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _export_file_impl(ctx):
    return [DefaultInfo(default_output = ctx.attrs.src)]

export_file = rule(
    impl = _export_file_impl,
    attrs = {
        "src": attrs.source(allow_directory = True),
    },
)
