# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _run_python(ctx):
    return [
        DefaultInfo(),
        RunInfo(args = cmd_args("python3", "-c", ctx.attrs.script)),
    ]

run_python = rule(
    impl = _run_python,
    attrs = {
        "script": attrs.string(),
    },
)
