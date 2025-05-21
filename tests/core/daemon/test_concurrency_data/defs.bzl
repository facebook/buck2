# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.declare_output("out")
    ctx.actions.run(
        ["python3", "-c", "import time, sys; time.sleep(999999); open(sys.argv[1],'w')", out.as_output()],
        category = "test",
        identifier = "id",
    )

    return [DefaultInfo(out)]

test = rule(
    impl = _test_impl,
    attrs = {},
)
