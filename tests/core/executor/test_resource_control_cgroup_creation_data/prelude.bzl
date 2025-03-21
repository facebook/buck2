# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    out = ctx.actions.declare_output("out")

    # run a process in a background that exits in 1000 secs
    # this way a cgoup will exist till the process finishes
    # although action finishes immediately
    ctx.actions.run(cmd_args(["sh", "-c", 'touch "$1"; sleep 1000 &', "--", out.as_output()]), category = "run")
    return [DefaultInfo(default_output = out)]

my_rule = rule(impl = _impl, attrs = {})
