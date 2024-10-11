# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _testtest(ctx: AnalysisContext) -> list[Provider]:
    a = ctx.actions.declare_output("uuuuuu")
    b = a.project("rel")
    ctx.actions.write(b.as_output(), "ccoonntteenntt")
    return [DefaultInfo(default_output = a)]

testtest = rule(
    impl = _testtest,
    attrs = {},
)
