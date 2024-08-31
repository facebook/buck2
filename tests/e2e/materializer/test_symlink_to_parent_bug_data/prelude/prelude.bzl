# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _whistle(ctx):
    whistle = ctx.actions.declare_output("whistle")
    ctx.actions.run([
        "ln",
        "-s",
        ".",
        whistle.as_output(),
    ], category = "test")
    return [DefaultInfo(default_output = whistle)]

whistle = rule(impl = _whistle, attrs = {})

def _flute(ctx):
    flute = ctx.actions.declare_output("flute")
    ctx.actions.run([
        "ln",
        "-s",
        "flute",
        flute.as_output(),
    ], category = "test")
    return [DefaultInfo(default_output = flute)]

flute = rule(impl = _flute, attrs = {})
