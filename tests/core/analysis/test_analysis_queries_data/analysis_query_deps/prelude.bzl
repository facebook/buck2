# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

target = rule(
    impl = lambda ctx: [
        DefaultInfo(default_output = ctx.actions.write("out", ctx.attrs.arg or "", allow_args = True)[0]),
        RunInfo(),
    ],
    attrs = {"arg": attrs.option(attrs.arg(), default = None)},
)

def defs():
    # Targets we want to run deps queries from
    target(
        name = "deps",
        arg = "$(query_targets deps(:foo))",
    )
    target(
        name = "deps1",
        arg = "$(query_targets deps(:foo, 1))",
    )
    target(
        name = "target_deps",
        arg = "$(query_targets deps(:foo, 100000, target_deps()))",
    )

    # Targets that are deps of targets we want to run deps queries from.
    target(
        name = "foo",
        arg = "$(location :bar) $(exe :baz)",
    )
    target(name = "bar")
    target(name = "baz", arg = "$(location :qux)")
    target(name = "qux")
