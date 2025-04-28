# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test(ctx: AnalysisContext):
    local = ctx.actions.declare_output("local")
    remote = ctx.actions.declare_output("remote")

    ctx.actions.run(
        [
            "python3",
            ctx.attrs.script,
            local.as_output(),
            "local",
        ],
        category = "check",
        identifier = "local",
        prefer_local = True,
    )

    ctx.actions.run(
        [
            "python3",
            ctx.attrs.script,
            remote.as_output(),
            "remote",
        ],
        category = "check",
        identifier = "remote",
        prefer_remote = True,
    )

    return [DefaultInfo(other_outputs = [local, remote])]

test = rule(attrs = {"script": attrs.source()}, impl = _test)
