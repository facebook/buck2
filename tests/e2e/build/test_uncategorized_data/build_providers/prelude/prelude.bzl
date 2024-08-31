# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _simple_test_impl(ctx):
    return [
        DefaultInfo(default_output = ctx.actions.write("build", "")),
        RunInfo(args = cmd_args([
            ctx.actions.write("run", ""),
        ])),
        ExternalRunnerTestInfo(
            type = "custom",
            command = [cmd_args([
                ctx.actions.write("test", ""),
            ])],
            env = {},
            labels = [],
            contacts = [],
        ),
    ]

simple_test = rule(
    impl = _simple_test_impl,
    attrs = {},
)
