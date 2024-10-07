# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_impl(ctx: AnalysisContext) -> list[Provider]:
    outs = {}
    for i in range(10):
        o = ctx.actions.declare_output("out/{}".format(i))
        ctx.actions.run(
            ["python3", "-c", "import time, sys; time.sleep(2); open(sys.argv[1],'w')", o.as_output()],
            category = "test",
            identifier = str(i),
            weight_percentage = 20,
        )
        outs[str(i)] = o

    out = ctx.actions.symlinked_dir("outs", outs)

    return [DefaultInfo(out)]

test = rule(attrs = {}, impl = _test_impl)
