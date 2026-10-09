# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(":nodejs_binary.bzl", "build_nodejs_command")

def nodejs_test_impl(ctx: AnalysisContext) -> list[Provider]:
    command = build_nodejs_command(ctx)
    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "custom",
            command = [command.output.cmd],
            env = command.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
        ),
    ) + [
        DefaultInfo(default_output = command.merged, other_outputs = list(command.output.output.default_outputs) + list(command.output.output.other_outputs)),
    ]
