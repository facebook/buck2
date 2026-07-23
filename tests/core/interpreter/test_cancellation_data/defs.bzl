# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# @nolint

def spin():
    for i in range(2147483647):
        for j in range(2147483647):
            for k in range(2147483647):
                pass

def loop_long(kind):
    # `read_config` is only available while evaluating a `BUCK`/`PACKAGE` file,
    # so this may only be called from those contexts, not during analysis.
    if read_config("should", "loop", "") == kind:
        spin()

def _noop_impl(ctx):
    outs = []
    out = ctx.actions.write("out.txt", ctx.attrs.name, has_content_based_path = False)
    outs.append(out)

    # `read_config` is unavailable during analysis, so whether to loop is decided
    # during build file evaluation (in the `noop` macro) and threaded in via an
    # attribute.
    if ctx.attrs.loop_analysis:
        spin()

    return [DefaultInfo(default_outputs = outs)]

_noop = rule(
    impl = _noop_impl,
    attrs = {
        "loop_analysis": attrs.bool(default = False),
    },
)

def noop(**kwargs):
    kwargs["loop_analysis"] = read_config("should", "loop", "") == "analysis"
    _noop(**kwargs)
