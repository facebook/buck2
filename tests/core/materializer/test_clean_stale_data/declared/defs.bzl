# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _cp(ctx):
    out = ctx.actions.declare_output("__objects__/out")

    ctx.actions.run(
        ["cp", ctx.attrs.src, out.as_output()],
        category = "cp",
        prefer_remote = ctx.attrs.remote,
        local_only = not ctx.attrs.remote,
    )

    return [DefaultInfo(out)]

cp = rule(impl = _cp, attrs = {"remote": attrs.bool(), "src": attrs.source()})

def defs():
    cp(name = "declared", src = "src", remote = True)
    cp(name = "remote", src = ":declared", remote = True)
    cp(name = "local", src = ":declared", remote = False)
