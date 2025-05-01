# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _dep_impl(ctx):
    out = ctx.actions.declare_output("dep")
    ctx.actions.run(
        [
            "python3",
            ctx.attrs.script,
            out.as_output(),
        ],
        env = {"cache_buster": ctx.attrs.cache_buster},
        category = "test",
    )
    return [DefaultInfo(default_output = out)]

dep = rule(
    impl = _dep_impl,
    attrs = {
        "cache_buster": attrs.string(default = read_config("test", "cache_buster", "")),
        "script": attrs.source(),
    },
)

def _action_fail(ctx):
    dep = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("failed_action")
    ctx.actions.run(
        cmd_args(
            "python3",
            "-c",
            "import sys; sys.exit(1)",
            out.as_output(),
            hidden = dep,
        ),
        env = {"cache_buster": ctx.attrs.cache_buster},
        category = "test",
    )
    return [DefaultInfo(default_outputs = [out])]

action_fail = rule(
    impl = _action_fail,
    attrs = {
        "cache_buster": attrs.string(default = read_config("test", "cache_buster", "")),
        "dep": attrs.dep(),
    },
)
