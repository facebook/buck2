# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _run_python(ctx):
    return [
        DefaultInfo(),
        RunInfo(args = cmd_args("python3", "-c", ctx.attrs.os + "; " + ctx.attrs.cpu)),
    ]

run_python = rule(
    impl = _run_python,
    attrs = {
        "cpu": attrs.string(),
        "os": attrs.string(),
    },
)
