# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _action_fail(ctx):
    out = ctx.actions.declare_output("out.txt")
    ctx.actions.run(cmd_args("false", hidden = out.as_output()), category = "run")
    return [DefaultInfo(default_outputs = [out])]

action_fail = rule(
    impl = _action_fail,
    attrs = {},
)

def _action_missing_output(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(cmd_args("true", hidden = out.as_output()), category = "run")
    return [DefaultInfo(default_outputs = [out])]

missing_outputs = rule(
    impl = _action_missing_output,
    attrs = {},
)

def _bad_url(ctx):
    out = ctx.actions.declare_output("out.txt")
    ctx.actions.download_file(out.as_output(), "doesnotexist640693486.com", sha1 = "1" * 40)
    return [DefaultInfo(default_output = out)]

bad_url = rule(
    impl = _bad_url,
    attrs = {},
)

def _run_action(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(cmd_args(["sh", "-c", 'echo > "$1"', "--", out.as_output()]), category = "run", local_only = ctx.attrs.local_only)
    return [DefaultInfo(default_outputs = [out])]

run_action = rule(
    impl = _run_action,
    attrs = {
        "local_only": attrs.bool(default = False),
    },
)
