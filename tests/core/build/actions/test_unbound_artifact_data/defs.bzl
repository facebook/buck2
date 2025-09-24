# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _action_with_unbound_artifact_impl(ctx):
    out = ctx.actions.declare_output("out")
    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "with open('sys.argv[1]', 'w') as f:",
            "  f.write('sys.argv[2]')",
        ],
    )

    args = cmd_args(["fbpython", script, out.as_output(), out])

    ctx.actions.run(args, category = "test_run")

    return [DefaultInfo(default_output = out)]

action_with_unbound_artifact = rule(
    impl = _action_with_unbound_artifact_impl,
    attrs = {
    },
)
