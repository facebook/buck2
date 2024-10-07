# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @nolint

def _dummy_binary_impl(ctx):
    out = ctx.write("out.txt", ctx.attrs.name)
    return [DefaultInfo(default_output=out)]

<rule>dummy_binary</rule> = rule(
    impl=dummy_binary_impl,
    attrs={
        "deps": attrs.list(attrs.dep(), default=[])
    }
)
