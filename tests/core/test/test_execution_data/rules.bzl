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

# Writes into the execution directory the test runner declared as an output of the command,
# next to the infra failure report it asked for, and records which stage wrote it.
script_writing_output = """
import os, sys
report = os.environ["TEST_INFRA_FAILURE_REPORT_PATH"]
with open(os.path.join(os.path.dirname(report), "written_by_test"), "w") as f:
    f.write("list" if "--list" in sys.argv else "run")
if '--list' in sys.argv:
    print('test1\\n')
sys.exit(0)
"""

def _simple_test_impl(ctx):
    out = ctx.actions.declare_output("file", has_content_based_path = False)
    ctx.actions.run(
        ["touch", out.as_output()],
        category = "touch",
    )
    env = {}
    if ctx.attrs.seed:
        env["SEED"] = ctx.attrs.seed
    return [
        DefaultInfo(out),
        ExternalRunnerTestInfo(
            command = ["fbpython", "-c", script_writing_output if ctx.attrs.writes_output else script],
            use_project_relative_paths = True,
            type = "lionhead",
            supports_test_execution_caching = ctx.attrs.supports_test_execution_caching,
            env = env,
        ),
    ]

simple_test = rule(
    attrs = {
        "seed": attrs.string(default = ""),
        "supports_test_execution_caching": attrs.bool(default = False),
        "writes_output": attrs.bool(default = False),
    },
    impl = _simple_test_impl,
)
