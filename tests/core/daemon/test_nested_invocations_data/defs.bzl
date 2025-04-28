# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _normal_impl(ctx):
    out = ctx.actions.declare_output("out.txt")

    cmd = cmd_args(
        ctx.attrs.buck2_path,
        "build",
        "root//:trivial",
        "-c",
        "nested.buck2_path=" + ctx.attrs.buck2_path,
        "--out",
        out.as_output(),
    )
    ctx.actions.run(
        cmd,
        # Unset the user version. Actions will only sometimes do this, tests always will
        #
        # Unfortunately, no good way to ask for it to be unset, so we do this instead
        env = {"SANDCASTLE_ID": ""},
        local_only = True,
        category = "run",
    )
    return [DefaultInfo(default_output = out)]

def _trace_impl(ctx):
    trace_out = ctx.actions.declare_output("trace_out.txt")
    nested_out = ctx.actions.declare_output("nested_out.txt")

    script = ctx.actions.write("script.py", """
import subprocess
import sys

buck_path = sys.argv[1]
subprocess.run([buck_path, "debug", "trace-io", "enable"])
    """)
    ctx.actions.run(
        [
            "python3",
            script,
            ctx.attrs.buck2_path,
            trace_out.as_output(),
        ],
        local_only = True,
        category = "trace",
    )

    nested_cmd = cmd_args(
        ctx.attrs.buck2_path,
        "build",
        "root//:trivial",
        "-c",
        "nested.buck2_path=" + ctx.attrs.buck2_path,
        "--out",
        nested_out.as_output(),
        hidden = trace_out,
    )
    ctx.actions.run(
        nested_cmd,
        local_only = True,
        category = "run",
    )
    return [DefaultInfo(default_output = nested_out)]

normal_nested_invocation = rule(
    impl = _normal_impl,
    attrs = {
        "buck2_path": attrs.string(),
    },
)

trace_nested_invocation = rule(
    impl = _trace_impl,
    attrs = {
        "buck2_path": attrs.string(),
    },
)
