# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _artifacts(ctx):
    fast = ctx.actions.write("fast", "")
    slow = ctx.actions.declare_output("slow")

    ctx.actions.run(
        [
            "python3",
            "-c",
            "import time, sys; time.sleep(5); open(sys.argv[1], 'w')",
            slow.as_output(),
        ],
        category = "slow",
    )

    return [DefaultInfo(slow, other_outputs = [fast])]

artifacts = rule(impl = _artifacts, attrs = {})
