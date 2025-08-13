# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _basic_incremental_actions_impl(ctx) -> list[Provider]:
    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = ctx.attrs.use_content_based_path)
    ctx.actions.run(
        cmd_args(["fbpython", ctx.attrs.increment] + ["--out", out.as_output()]),
        category = "incremental",
        no_outputs_cleanup = True,
        env = {"INVALIDATE_ACTION": ctx.attrs.invalidate},
    )
    return [
        DefaultInfo(out),
        RunInfo(args = ["cat", out]),
    ]

basic_incremental_action = rule(impl = _basic_incremental_actions_impl, attrs = {
    "increment": attrs.source(),
    "invalidate": attrs.string(),
    "use_content_based_path": attrs.bool(default = read_config("test", "use_content_based_path", "") in ["true", "True"]),
})
