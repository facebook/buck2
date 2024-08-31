# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_impl(ctx):
    out = ctx.actions.declare_output("file")
    ctx.actions.run(
        ["cp", "/run/re_worker/action_dependencies", out.as_output()],
        category = "cp",
        env = {"cache_buster": ctx.attrs.cache_buster},
        remote_execution_dependencies = ctx.attrs.remote_execution_dependencies,
    )
    return [DefaultInfo(out)]

test = rule(attrs = {
    "cache_buster": attrs.string(default = read_config("test", "cache_buster", "")),
    "remote_execution_dependencies": attrs.list(attrs.dict(key = attrs.string(), value = attrs.string()), default = []),
}, impl = _test_impl)
