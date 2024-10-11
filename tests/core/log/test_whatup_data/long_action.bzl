# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    out = ctx.actions.declare_output("out.txt")
    py = cmd_args(
        "import time; time.sleep(3); ",
        "import sys; open(sys.argv[1], 'w').write('')",
        delimiter = "",
    )

    ctx.actions.run(
        cmd_args("python3", "-c", py, out.as_output()),
        category = "run_python",
    )

    return [DefaultInfo(default_output = out)]

long_action = rule(
    impl = _impl,
    attrs = {},
)
