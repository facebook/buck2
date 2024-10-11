# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_output_artifact_twice(ctx: AnalysisContext) -> list[Provider]:
    a = ctx.actions.declare_output("uuuuuu")
    ctx.actions.run(["python3", "-c", """
import sys
[_, f1, f2] = sys.argv
assert f1 == f2
with open(f1, "w") as f:
    f.write("green lamp")
""", a.as_output(), a.as_output()], category = "ignore")
    return [DefaultInfo(default_output = a)]

test_output_artifact_twice = rule(
    impl = _test_output_artifact_twice,
    attrs = {},
)
