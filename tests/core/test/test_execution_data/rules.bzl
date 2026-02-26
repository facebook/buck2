# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

script = """
import sys;
if '--list' in sys.argv:
    print('test1\\n')
sys.exit(0)
"""

def _simple_test_impl(ctx):
    out = ctx.actions.declare_output("file")
    ctx.actions.run(
        ["touch", out.as_output()],
        category = "touch",
    )
    return [
        DefaultInfo(out),
        ExternalRunnerTestInfo(
            command = ["fbpython", "-c", script],
            use_project_relative_paths = True,
            type = "lionhead",
        ),
    ]

simple_test = rule(attrs = {}, impl = _simple_test_impl)
