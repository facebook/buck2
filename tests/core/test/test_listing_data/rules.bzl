# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

script = """
import sys;
if '--list' in sys.argv:
    print('test1\\n')
sys.exit(0)
"""

bad_script = """
import sys;
sys.exit(1)
"""

def _impl_listing_uncacheable(ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", script],
            type = "lionhead",
            env = {"seed": ctx.attrs.seed},
            labels = ["tpx:listing_uncacheable"],
        ),
    ]

def _impl_ok(ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", script],
            type = "lionhead",
            env = {"seed": ctx.attrs.seed},
        ),
    ]

def _impl_bad(ctx):
    return [
        DefaultInfo(),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", bad_script],
            type = "lionhead",
            env = {"seed": ctx.attrs.seed},
        ),
    ]

def _seed_impl(ctx):
    out = ctx.actions.declare_output("file")
    ctx.actions.run(
        ["touch", out.as_output()],
        category = "touch",
        env = {"seed": ctx.attrs.seed},
    )
    return [
        DefaultInfo(out),
        ExternalRunnerTestInfo(
            command = ["python3", "-c", script],
            use_project_relative_paths = True,
            type = "lionhead",
            env = {"seed": ctx.attrs.seed},
        ),
    ]

seed = rule(attrs = {"seed": attrs.string()}, impl = _seed_impl)
ok_test = rule(attrs = {"seed": attrs.string()}, impl = _impl_ok)
bad_test = rule(attrs = {"seed": attrs.string()}, impl = _impl_bad)
listing_uncacheable = rule(attrs = {"seed": attrs.string()}, impl = _impl_listing_uncacheable)
