# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _hang(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        ["python3", "-c", 'import os, time; open(os.environ["TOUCH"], "w"); time.sleep(100)'],
        env = {"OUT": out.as_output(), "TOUCH": ctx.attrs.touch},
        category = "hang",
    )
    return [DefaultInfo(out)]

# Touch a file to signal, then hang.
hang = rule(attrs = {"touch": attrs.string()}, impl = _hang)

def _pass(ctx):
    out = ctx.actions.write("out", "")
    return [DefaultInfo(out)]

pass_ = rule(attrs = {}, impl = _pass)

def _kill(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        ["python3", "-c", 'import os, signal; os.kill(int(os.environ["PID"]), signal.SIGKILL)'],
        env = {"OUT": out.as_output(), "PID": ctx.attrs.pid},
        category = "kill",
    )
    return [DefaultInfo(out)]

kill = rule(attrs = {"pid": attrs.string()}, impl = _kill)

def _fail(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        cmd_args(
            "sh",
            "-c",
            'echo "Hi from stderr!" >&2 && false',
            hidden = out.as_output(),
        ),
        category = "fail",
    )
    return [DefaultInfo(out)]

fail = rule(attrs = {}, impl = _fail)

def _one(ctx):
    return [DefaultInfo(default_output = ctx.actions.write("out", "one"))]

one = rule(
    impl = _one,
    attrs = {},
)

def _two(ctx):
    return [DefaultInfo(default_output = ctx.actions.write("out", "two"))]

two = rule(
    impl = _two,
    attrs = {},
)
