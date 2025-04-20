# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _impl(ctx: AnalysisContext) -> list[Provider]:
    marker = ctx.actions.declare_output("marker")
    write_args = cmd_args(
        ctx.attrs.script,
        cmd_args(
            "from pathlib import Path; ",
            "import sys; ",
            'open(Path(sys.argv[0]).parent / "marker", "w").write("")',
            delimiter = "",
        ),
        hidden = marker.as_output(),
    )
    script, _ = ctx.actions.write("script.py", write_args, allow_args = True)
    ctx.actions.run(
        cmd_args("fbpython", script, ctx.attrs.script_args, hidden = write_args),
        category = "py_assertion",
    )
    return [
        DefaultInfo(default_output = marker),
    ]

py_assertion = rule(
    impl = _impl,
    attrs = {
        "script": attrs.arg(),
        "script_args": attrs.list(attrs.arg(), default = []),
    },
)
