# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl_ok(_ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(0)"],
            type = "custom",
        ),
    ]

ok_test = rule(attrs = {}, impl = _impl_ok)

def _impl_fail(_ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", "import sys; sys.exit(1)"],
            type = "custom",
        ),
    ]

fail_test = rule(attrs = {}, impl = _impl_fail)

def _impl_build_fail1(ctx):
    ctx.actions.run(cmd_args("false"), category = "fail1")
    return [DefaultInfo(default_outputs = [])]

fail_build1 = rule(attrs = {}, impl = _impl_build_fail1)

def _impl_build_fail2(ctx):
    output = ctx.actions.declare_file("fail2")
    ctx.actions.run(cmd_args("false"), category = "fail2")
    return [DefaultInfo(default_outputs = [output])]

fail_build2 = rule(attrs = {}, impl = _impl_build_fail2)
