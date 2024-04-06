# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# @nolint

def _stub(ctx):
    _ignore = ctx
    return [
        DefaultInfo(),
    ]

# Rule that has dependencies and does nothing. Useful for query-like tests.
stub = rule(
    impl = _stub,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "exec_deps": attrs.list(attrs.exec_dep(), default = []),
        "toolchain_deps": attrs.list(attrs.toolchain_dep(), default = []),
    },
)

# Rule with no attrs that produces an output. Useful if you want to be able to
# build literally anything
def _trivial_build(ctx):
    return [DefaultInfo(default_output = ctx.actions.write("foo.txt", "abcd"))]

trivial_build = rule(
    impl = _trivial_build,
    attrs = {},
)
