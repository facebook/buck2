# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# Pass the same artifact twice to `run` action.
def _test_output_artifact_twice_same(ctx: AnalysisContext) -> list[Provider]:
    a = ctx.actions.declare_output("uuuuuu")
    ctx.actions.run(["python3", "-c", """
import sys
[_, f1, f2] = sys.argv
assert f1 == f2
with open(f1, "w") as f:
    f.write("green lamp")
""", a.as_output(), a.as_output()], category = "ignore")
    return [DefaultInfo(default_output = a)]

test_output_artifact_twice_same = rule(
    impl = _test_output_artifact_twice_same,
    attrs = {},
)

# Pass the same artifact twice to `run` action, but the second as a projection.
def _test_output_artifact_twice_with_projection(ctx: AnalysisContext) -> list[Provider]:
    a = ctx.actions.declare_output("ttttttttt")
    b = a.project("rel")
    ctx.actions.run(["python3", "-c", r"""
import sys
import os

[_, f1, f2] = sys.argv
bs = "\\"
assert f"{f1.replace(bs, '/')}/rel" == f2.replace(bs, '/')

os.mkdir(f1)

with open(f2, "w") as f:
    f.write("red alert")

""", a.as_output(), b.as_output()], category = "ignore")
    return [DefaultInfo(default_output = a)]

test_output_artifact_twice_with_projection = rule(
    impl = _test_output_artifact_twice_with_projection,
    attrs = {},
)
