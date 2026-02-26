# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _test(ctx: AnalysisContext):
    dep = ctx.actions.write("dep", "")
    default = ctx.actions.copy_file("default", dep)
    other_default = ctx.actions.copy_file("other_default", dep)
    other = ctx.actions.write("other", "")

    sub_default = ctx.actions.write("sub_default", "")
    sub_other = ctx.actions.write("sub_other", "")

    # Unused
    ctx.actions.write("unused", "")

    return [DefaultInfo(
        default_outputs = [default, other_default],
        other_outputs = [other],
        sub_targets = {
            "sub": [
                DefaultInfo(
                    default_outputs = [sub_default],
                    other_outputs = [sub_other],
                ),
            ],
        },
    )]

test = rule(impl = _test, attrs = {})
