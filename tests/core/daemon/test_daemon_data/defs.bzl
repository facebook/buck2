# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _long_running_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("out", has_content_based_path = False)
    ctx.actions.run(
        ["fbpython", "-c", "import time, sys; time.sleep(999999); open(sys.argv[1],'w')", out.as_output()],
        category = "test",
        identifier = "id",
    )

    return [DefaultInfo(out)]

long_running = rule(
    impl = _long_running_impl,
    attrs = {},
)

def _pwd_run_impl(_ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        RunInfo(args = cmd_args("fbpython", "-c", "import os; print(os.getcwd())")),
    ]

# Prints the working directory a `buck2 run` target starts in.
pwd_run = rule(
    impl = _pwd_run_impl,
    attrs = {},
)
