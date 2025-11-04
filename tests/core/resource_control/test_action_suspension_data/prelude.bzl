# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

_use_some_memory = read_root_config("use_some_memory", "path")

def _memory_allocating_actions_impl(ctx: AnalysisContext) -> list[Provider]:
    all_outputs = []
    for i in range(ctx.attrs.width):
        last_output = cmd_args()
        for j in range(ctx.attrs.depth):
            output = ctx.actions.declare_output("output_{}_{}.txt".format(i, j))
            all_outputs.append(output)
            cmd = cmd_args(
                _use_some_memory,
                "--allocate-count",
                str(ctx.attrs.ticks),
                "--each-tick-allocate-memory",
                str(ctx.attrs.memory_per_tick_mb),
                "--tick-duration",
                str(ctx.attrs.tick_duration_ms / 1000.0),
                "--pre-exit-sleep-duration",
                str(ctx.attrs.sleep_ms / 1000.0),
                "--output",
                output.as_output(),
                hidden = last_output,
            )
            ctx.actions.run(cmd, category = "memory_allocating_actions", identifier = "action_{}_{}".format(i, j))
            last_output = output

    return [DefaultInfo(default_outputs = all_outputs)]

memory_allocating_actions = rule(
    impl = _memory_allocating_actions_impl,
    attrs = {
        "depth": attrs.int(),
        "memory_per_tick_mb": attrs.int(),
        "sleep_ms": attrs.int(),
        "tick_duration_ms": attrs.int(),
        "ticks": attrs.int(),
        "width": attrs.int(),
    },
)
