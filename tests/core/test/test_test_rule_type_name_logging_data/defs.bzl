# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _one(ctx):
    return [
        DefaultInfo(default_output = ctx.actions.write("out", "one")),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(0)"],
            type = "custom",
        ),
    ]

one = rule(
    impl = _one,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
    },
)

def _two(ctx):
    return [
        DefaultInfo(default_output = ctx.actions.write("out", "two")),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(0)"],
            type = "custom",
        ),
    ]

two = rule(
    impl = _two,
    attrs = {},
)
