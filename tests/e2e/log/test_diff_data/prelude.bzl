# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Rule with no attrs that may produce an output non-deterministically based on the path provided in buckconfig
def _non_det_build(ctx):
    return [DefaultInfo(default_output = ctx.actions.write("foo.txt", ctx.attrs.buck2_output))]

non_det_build = rule(
    impl = _non_det_build,
    attrs = {
        "buck2_output": attrs.string(),
    },
)

# Rule with no attrs that produces an output deterministically
def _trivial_build(ctx):
    return [DefaultInfo(default_output = ctx.actions.write("foo.txt", "abcd"))]

trivial_build = rule(
    impl = _trivial_build,
    attrs = {},
)
