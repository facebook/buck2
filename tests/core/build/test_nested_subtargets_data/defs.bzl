# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _nested_subtargets(ctx):
    out = ctx.actions.write("foo", "foo_content")

    nested_info = [DefaultInfo(
        sub_targets = {"nested_sub": [
            DefaultInfo(default_output = out),
        ]},
    )]

    return [DefaultInfo(
        sub_targets = {"sub": nested_info},
    )]

nested_subtargets = rule(
    impl = _nested_subtargets,
    attrs = {},
)
