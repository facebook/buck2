# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_impl(ctx):
    out = ctx.actions.symlinked_dir("out", {"in": ctx.attrs.src})
    return [DefaultInfo(default_output = out)]

test = rule(
    impl = _test_impl,
    attrs = {"src": attrs.source(allow_directory = True)},
)

def defs():
    test(name = "root", src = "src")
