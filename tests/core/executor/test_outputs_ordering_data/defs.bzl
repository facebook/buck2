# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx):
    a = ctx.actions.declare_output("a", dir = True)
    z = ctx.actions.declare_output("z", dir = True)
    cmd = ["sh", "-c", 'mkdir -p "$1" "$2" && touch "$1"/foo', "--", z.as_output(), a.as_output()]
    ctx.actions.run(
        cmd,
        category = "mkdir",
        env = {"CACHE_BUSTER": ctx.attrs.cache_buster},
    )
    return [DefaultInfo(a, other_outputs = [z])]

my_rule = rule(impl = _impl, attrs = {
    "cache_buster": attrs.string(default = read_config("test", "cache_buster", "")),
})
