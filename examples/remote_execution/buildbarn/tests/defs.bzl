# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _tests(ctx):
    # Create locally
    stage0 = ctx.actions.declare_output("stage0")
    ctx.actions.run(
        ["sh", "-c", 'head -c 10 /dev/urandom > "$1"', "--", stage0.as_output()],
        category = "stage0",
        local_only = True,
    )

    # Use on RE
    stage1 = ctx.actions.declare_output("stage1")
    ctx.actions.run(["sh", "-c", 'cat "$1" "$1" > "$2"', "--", stage0, stage1.as_output()], category = "stage1")

    # Reuse on RE
    stage2 = ctx.actions.declare_output("stage2")
    ctx.actions.run(["sh", "-c", 'cat "$1" "$1" > "$2"', "--", stage1, stage2.as_output()], category = "stage2")

    # Reuse locally
    stage3 = ctx.actions.declare_output("stage3")
    ctx.actions.run(
        ["sh", "-c", 'cat "$1" "$1" > "$2"', "--", stage2, stage3.as_output()],
        category = "stage3",
        local_only = True,
    )

    # Verify
    stage4 = ctx.actions.declare_output("stage4")
    ctx.actions.run(
        [
            "sh",
            "-c",
            'cat "$1" "$1" "$1" "$1" "$1" "$1" "$1" "$1" > "$3" && diff "$2" "$3"',
            "--",
            stage0,
            stage3,
            stage4.as_output(),
        ],
        category = "stage4",
    )

    return [DefaultInfo(stage4)]

tests = rule(attrs = {}, impl = _tests)
