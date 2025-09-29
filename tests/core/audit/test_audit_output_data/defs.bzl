# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _simple_rule(ctx):
    return [DefaultInfo(default_output = ctx.actions.write("foo.txt", "abcd", has_content_based_path = ctx.attrs.has_content_based_path))]

simple_rule = rule(
    impl = _simple_rule,
    attrs = {
        "has_content_based_path": attrs.bool(default = False),
    },
)
